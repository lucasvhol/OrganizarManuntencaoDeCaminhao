

# Constants

real_number = "[[[:digit:]]\\.[[:digit:]]+|[[:digit:]]]"
int_number  = "[[:digit:]]"
dec_number  = "[[[:digit:]]\\.[[:digit:]]]"


#'
#' string concatenation
#'
#' this functions makes string concatenatio cleaner
#'
#' @param a,b strings to concatenate
#' @return concatenation
#' @details Sem detalhes
#' @export
#' @examples

`%+%` = function(a,b) paste0(a,b)


#
#'
#' Função DE-PARA
#'
#' A função muda os valores do vetor de acordo com a abela de-para fornecida
#'
#' @param aux.da A vector containing the data to have the replacements
#' @param aux.DE_PARA A data.frame with the "DE PARA" rules
#' @return o mesmo vetor com os valores modificados
#' @details Sem detalhes
#' @export
#' @examples


f_DE_PARA = function(aux.da, aux.DE_PARA){
  # aux.da: A vector containing the data to have the replacements
  # aux.DE_PARA: A data.frame with the "DE PARA" rules
  
  
  # Replaces "(" by "" to use gsub easier
  aux.da = gsub(x = aux.da , pattern = '\\(', replacement = '')
  aux.da = gsub(x = aux.da , pattern = '\\)', replacement = '')
  
  ###+
  # changes the data according to the "DE PARA" table
  ###+
  
  # pattern "DE"
  patt.DE = aux.DE_PARA$DE
  patt.DE = gsub(x = patt.DE , pattern = '\\(', replacement = '')
  patt.DE = gsub(x = patt.DE , pattern = '\\)', replacement = '')
  
  
  # pattern "PARA"
  patt.PARA= aux.DE_PARA$PARA
  patt.PARA = gsub(x = patt.PARA , pattern = '\\(', replacement = '')
  patt.PARA = gsub(x = patt.PARA , pattern = '\\)', replacement = '')
  
  
  
  # calls gsub() for each value in aux.DE_PARA.
  i = 0
  while(i < length(patt.DE)){
    i = i+1
    
    aux.da = gsub(x = aux.da, pattern = patt.DE[i], replacement = patt.PARA[i])
    
    
  }
  
  return(aux.da)
  
}



#'
#' Title of the function
#'
#' switches values of columns col_name and col_name2 when the value of in col_name is NA
#'
#' @param aux.da a data frame
#' @param col_name a string containing the name of the columns to have the value replaced
#' @return 
#' @details
#' @export
#' @examples
#' f_primeira_troca_vira_segunda(aux.da,"Gearbox_Oil_1st")
#'
f_primeira_troca_vira_segunda = function(aux_da, col_name){
  ## switches values of columns col_name and col_name2 (col_name2 = col_name + '2')
  # aux_da: a data frame
  # col_name: a string containing the name of th ecolumns to have th evalue replaced

  col_name2 = gsub(col_name, patt = "1st", repl = "2nd")
  index = is.na(aux_da[,col_name2]) & !is.na(aux_da[,col_name])
  aux_da[index,c(col_name2)] = aux_da[index,c(col_name)]
  aux_da[index,col_name] = NA

  return(aux_da)

}


#'
#' Motor Trocas
#'
#' Extract the information of Engine oil replacements
#'
#' @param aux_df the data.frame containing the columns "TEXT" from which the informations will be extracted.
#' @param add_patt if \code{TRUE} a column will be added informating what pattern was used to extrach the informations
#' @return The same \code{data.frame} containing extra columns and the respective values extraced from the "TEXT" column.
#' @details
#' @export
#' @examples
#'

motor_trocas = function(aux_df, add_patt = FALSE){

  ####################
  # aux_df: the data.frame to be changed.
  ####################

  aux_df$pattern = 0

  # inicializating the columns
  aux_df$Engine_Oil_Replaces = NA
  aux_df$Engine_Oil = NA

  #- Pattern 1 ----------------------------------------+

  # index the NAs
  index_aux = is.na(aux_df$Engine_Oil_Replaces)

  # Motor
  str.master = aux_df$TEXT
  str.master[index_aux] = gsub(str.master[index_aux]
                               , patt=" M *(" %+% real_number %+% "+T)"
                               , repl = " M \\1")
  str.master[index_aux] = str_extract(str.master[index_aux], patt='M.*Cx')

  # trocas
  str.aux = NA
  str.aux[index_aux] = str_extract(str.master[index_aux], patt='M *'%+% real_number %+% '+T')
  str.aux[index_aux] = str_extract(str.aux[index_aux], patt = real_number %+% '+')
  str.aux[index_aux] = str_extract(str.aux[index_aux], patt = real_number %+%'+')
  aux_df$Engine_Oil_Replaces[index_aux] = str.aux[index_aux]

  # km
  # index_aux = is.na(aux_df$Engine_Oil) # maybe this re_indexing is necessary; may it will be problematic
  # str.aux = NA
  str.aux = rep(NA, nrow(aux_df))
  str.aux[index_aux] = str_extract(str.master[index_aux], patt='C *' %+% real_number %+% '*')
  str.aux[index_aux] = str_extract(str.aux[index_aux], patt= real_number %+% '+')
  aux_df$Engine_Oil[index_aux] = str.aux[index_aux]


  aux_df$pattern[index_aux] = 1
  #----------------------------------------------------+

  #- Pattern 2 ----------------------------------------+

  # Motor
  str.master = aux_df$TEXT
  # index the NAs
  index_aux = is.na(aux_df$Engine_Oil)

  # trocas
  aux_df$Engine_Oil_Replaces[index_aux] = NA

  # km
  str.aux = rep(NA, nrow(aux_df))
  str.aux[index_aux] = str_extract(str.master[index_aux]
                                   , patt='M *' %+% real_number%+% '+ {0,1}km')
  str.aux[index_aux] = str_extract(str.aux[index_aux], patt= real_number%+% '+')
  aux_df$Engine_Oil[index_aux] = str.aux[index_aux]

  aux_df$pattern[index_aux] = 2

  #- Pattern 3 ----------------------------------------+

  # Motor
  str.master = aux_df$TEXT
  # index the NAs
  index_aux = is.na(aux_df$Engine_Oil)

  # trocas
  aux_df$Engine_Oil_Replaces[index_aux] = NA


  # km
  str.aux = rep(NA, nrow(aux_df))
  str.aux[index_aux] = str_extract(str.master[index_aux], patt='M *' %+% real_number%+% '+')
  str.aux[index_aux] = str_extract(str.aux[index_aux], patt='' %+% real_number%+% '+')
  aux_df$Engine_Oil[index_aux] = str.aux[index_aux]

  aux_df$pattern[index_aux] = 3

  #- Pattern 4 ----------------------------------------+

  # Motor
  str.master = aux_df$TEXT
  # index the NAs
  index_aux = is.na(aux_df$Engine_Oil)

  # trocas
  aux_df$Engine_Oil_Replaces[index_aux] = NA


  # km
  str.aux = rep(NA, nrow(aux_df))

  str.aux[index_aux] = gsub(str.master[index_aux]
                            , patt="(" %+% real_number%+% "+km).*(" %+% real_number%+% "+km).*(" %+% real_number%+% "+km).*"
                            , repl = "\\1")
  str.aux[index_aux] = str_extract(str.aux[index_aux], patt= real_number %+% '+ {0,1}km')
  str.aux[index_aux] = str_extract(str.aux[index_aux], patt= real_number %+% '+')
  aux_df$Engine_Oil[index_aux] = str.aux[index_aux]


  aux_df$pattern[index_aux] = 4

  #- Pattern 5 ----------------------------------------+

  # Motor
  str.master = aux_df$TEXT
  # index the NAs
  index_aux = is.na(aux_df$Engine_Oil)

  # trocas
  aux_df$Engine_Oil_Replaces[index_aux] = NA

  # km
  str.aux = rep(NA, nrow(aux_df))
  str.aux[index_aux] = str_extract(str.master[index_aux]
                                   , patt=' Motor *C* *' %+% real_number%+% '+')
  str.aux[index_aux] = str_extract(str.aux[index_aux], patt='' %+% real_number%+% '+')
  aux_df$Engine_Oil[index_aux] = str.aux[index_aux]

  aux_df$pattern[index_aux] = 5


  #- result -------------------------------------------+
  # Removes pattern when not needed

  index_aux = is.na(aux_df$Engine_Oil) & is.na(aux_df$Engine_Oil_Replaces)
  aux_df$pattern[index_aux] = 0

  if(!add_patt) aux_df$pattern = NULL


  return(aux_df)

}



#'
#' Caixa Trocas
#'
#' Extract the information of Gearbox oil replacements
#'
#' @param aux_df the data.frame containing the columns "TEXT" from which the informations will be extracted.
#' @param add_patt if \code{TRUE} a column will be added informating what pattern was used to extrach the informations
#' @return The same \code{data.frame} containing extra columns and the respective values extraced from the "TEXT" column.
#' @details
#' @export
#' @examples
#'
caixa_trocas = function(aux_df, add_patt = TRUE){

  ####################
  # aux_df: the data.frame to be changed.
  ####################

  aux_df$pattern = 0

  # inicializating the columns
  aux_df$Gearbox_Oil_Replaces = NA
  aux_df$Gearbox_Oil_1st = NA
  aux_df$Gearbox_Oil_2nd = NA


  #- Pattern 1 ------------+
  # index the NAs
  index_aux = is.na(aux_df$Gearbox_Oil_Replaces)

  str.master = aux_df$TEXT
  str.master[index_aux] = str_extract(str.master[index_aux], patt='Cx *' %+% real_number %+% '*T* *C *' %+% real_number %+% '*/*' %+% real_number %+% '*')

  # trocas
  str.aux = NA
  str.aux[index_aux] = str_extract(str.master[index_aux], patt='Cx *' %+% real_number %+% '*T')
  str.aux[index_aux] = str_extract(str.aux[index_aux], patt='' %+% real_number %+% '*T')
  str.aux[index_aux] = str_extract(str.aux[index_aux], patt='' %+% real_number %+% '+')
  aux_df$Gearbox_Oil_Replaces[index_aux] = str.aux[index_aux]

  # km 1
  str.aux[index_aux] = str_extract(str.master[index_aux], patt='C *' %+% real_number %+% '+')
  str.aux[index_aux] = str_extract(str.aux[index_aux], patt= real_number %+% '+')
  aux_df$Gearbox_Oil_1st[index_aux] = str.aux[index_aux]

  # km 2
  str.aux[index_aux] = str_extract(str.master[index_aux], patt='C *' %+% real_number %+% '+/*' %+% real_number %+% '+')
  str.aux[index_aux] = str_extract(str.aux[index_aux], patt='/+' %+% real_number %+% '+')
  str.aux[index_aux] = str_extract(str.aux[index_aux], patt= real_number %+% '+')
  aux_df$Gearbox_Oil_2nd[index_aux] = str.aux[index_aux]


  aux_df$pattern[index_aux] = 1

  #----------------------------------------------------+

  #- Pattern 2 ----------------------------------------+
  # Gear_Box
  str.master = aux_df$TEXT
  # index the NAs
  index_aux = is.na(aux_df$Gearbox_Oil_2nd) & is.na(aux_df$Gearbox_Oil_1st)

  # trocas
  aux_df$Gearbox_Oil_Replaces[index_aux] = NA
  # km 1
  aux_df$Gearbox_Oil_1st[index_aux] = NA

  # km 2
  str.aux = rep(NA, nrow(aux_df))
  str.aux[index_aux] = str_extract(str.master[index_aux], patt='Cx *' %+% real_number %+% '+ {0,1}km')
  str.aux[index_aux] = str_extract(str.aux[index_aux], patt = real_number %+% '+')
  aux_df$Gearbox_Oil_2nd[index_aux] = str.aux[index_aux]

  # km 2
  aux_df$pattern[index_aux] = 2


  #- Pattern 3 ----------------------------------------+
  # Gear_Box
  str.master = aux_df$TEXT
  # index the NAs
  index_aux = is.na(aux_df$Gearbox_Oil_2nd) & is.na(aux_df$Gearbox_Oil_1st)

  # trocas
  aux_df$Gearbox_Oil_Replaces[index_aux] = NA
  # km 1
  aux_df$Gearbox_Oil_1st[index_aux] = NA

  # km 2
  str.aux = rep(NA, nrow(aux_df))
  str.aux[index_aux] = str_extract(str.master[index_aux], patt =' C ' %+% real_number %+% '+')
  str.aux[index_aux] = str_extract(str.aux[index_aux], patt = real_number %+% '+')
  aux_df$Gearbox_Oil_2nd[index_aux] = str.aux[index_aux]


  aux_df$pattern[index_aux] = 3

  #- Pattern 4 ----------------------------------------+
  str.master = aux_df$TEXT
  # index the NAs
  index_aux = is.na(aux_df$Gearbox_Oil_2nd) & is.na(aux_df$Gearbox_Oil_1st)

  # trocas
  aux_df$Gearbox_Oil_Replaces[index_aux] = NA
  # km 1
  aux_df$Gearbox_Oil_1st[index_aux] = NA

  # km 2
  str.aux = rep(NA, nrow(aux_df))
  str.aux[index_aux] = gsub(str.master[index_aux], patt="^.*( " %+% real_number %+% "+km).*( " %+% real_number %+% "+km).*( " %+% real_number %+% "+km).*", repl = "\\2")
  str.aux[index_aux] = str_extract(str.aux[index_aux], patt = real_number %+% '+km')
  str.aux[index_aux] = str_extract(str.aux[index_aux], patt = real_number %+% '+')
  aux_df$Gearbox_Oil_2nd[index_aux] = str.aux[index_aux]

  aux_df$pattern[index_aux] = 4

  #- Pattern 5 ----------------------------------------+
  index_aux = is.na(aux_df$Gearbox_Oil_2nd) & is.na(aux_df$Gearbox_Oil_1st)

  # Gear Box
  str.master = aux_df$TEXT
  str.master[index_aux] = str_extract(str.master[index_aux], patt='Caixa.*')

  # index the NAs
  index_aux = is.na(aux_df$Gearbox_Oil_1st) & is.na(aux_df$Gearbox_Oil_2nd)

  # trocas
  aux_df$Gearbox_Oil_Replaces[index_aux] = NA

  str.aux = rep(NA, nrow(aux_df))
  # km 1
  str.aux[index_aux] = str_extract(str.master[index_aux], patt='Caixa *C* *' %+% real_number %+% '+')
  str.aux[index_aux] = str_extract(str.aux[index_aux], patt= real_number %+% '+')
  aux_df$Gearbox_Oil_1st[index_aux] = str.aux[index_aux]

  # km 2
  str.aux[index_aux] = str_extract(str.master[index_aux], patt='Caixa *C*' %+% real_number %+% '+/*' %+% real_number %+% '+')
  str.aux[index_aux] = str_extract(str.aux[index_aux], patt='/+' %+% real_number %+% '+')
  str.aux[index_aux] = str_extract(str.aux[index_aux], patt='' %+% real_number %+% '+')
  aux_df$Gearbox_Oil_2nd[index_aux] = str.aux[index_aux]


  aux_df$pattern[index_aux] = 5

  #- result -------------------------------------------+
  # Removes pattern when not needed
  if(!add_patt) aux_df$pattern = NULL

  return(aux_df)


}


#'
#' Diferencial Trocas
#'
#' Extract the information of Differential oil replacements
#'
#' @param aux_df the data.frame containing the columns "TEXT" from which the informations will be extracted.
#' @param add_patt if \code{TRUE} a column will be added informating what pattern was used to extrach the informations
#' @return The same \code{data.frame} containing extra columns and the respective values extraced from the "TEXT" column.
#' @details
#' @export
#' @examples
#'
diferencial_trocas = function(aux_df, add_patt = TRUE){

  ####################
  # aux_df: the data.frame to be changed.
  ####################

  aux_df$pattern = 0

  # inicializating the columns
  aux_df$Diferencial_Replaces = NA
  aux_df$Diferencial_1st = NA
  aux_df$Diferencial_2nd = NA


  #- Pattern 1 ------------+
  # index the NAs
  index_aux = is.na(aux_df$Diferencial_Replaces)

  str.master = aux_df$TEXT
  str.master[index_aux] = str_extract(str.master[index_aux], patt='D *' %+% real_number %+% '*T *C *' %+% real_number %+% '*/*' %+% real_number %+% '*')

  # trocas
  str.aux = NA
  str.aux[index_aux] = str_extract(str.master[index_aux], patt='D *' %+% real_number %+% '*T')
  str.aux[index_aux] = str_extract(str.aux[index_aux], patt= real_number %+% '*T')
  str.aux[index_aux] = str_extract(str.aux[index_aux], patt= real_number %+% '+')
  aux_df$Diferencial_Replaces[index_aux] = str.aux[index_aux]

  # km 1
  str.aux[index_aux] = str_extract(str.master[index_aux], patt = 'C *' %+% real_number %+% '+')
  str.aux[index_aux] = str_extract(str.aux[index_aux], patt = real_number %+% '+')
  aux_df$Diferencial_1st[index_aux] = str.aux[index_aux]

  # km 2
  str.aux[index_aux] = str_extract(str.master[index_aux], patt='C *' %+% real_number %+% '+/*' %+% real_number %+% '+')
  str.aux[index_aux] = str_extract(str.aux[index_aux], patt='/' %+% real_number %+% '+')
  str.aux[index_aux] = str_extract(str.aux[index_aux], patt= real_number %+% '+')
  aux_df$Diferencial_2nd[index_aux] = str.aux[index_aux]


  aux_df$pattern[index_aux] = 1

  #- Pattern 2 ----------------------------------------+
  # Gear_Box
  str.master = aux_df$TEXT
  # index the NAs
  index_aux = is.na(aux_df$Diferencial_2nd) & is.na(aux_df$Diferencial_1st)

  # trocas
  aux_df$Diferencial_Replaces[index_aux] = NA
  # km 1
  aux_df$Diferencial_1st[index_aux] = NA

  # km 2
  str.aux = rep(NA, nrow(aux_df))
  str.aux[index_aux] = str_extract(str.master[index_aux], patt='E *' %+% real_number %+% '+ {0,1}km')
  str.aux[index_aux] = str_extract(str.aux[index_aux], patt= real_number %+% '+')
  aux_df$Diferencial_2nd[index_aux] = str.aux[index_aux]

  # km 2
  aux_df$pattern[index_aux] = 2


  #- Pattern 3 ----------------------------------------+
  # Gear_Box
  str.master = aux_df$TEXT
  # index the NAs
  index_aux = is.na(aux_df$Diferencial_2nd) & is.na(aux_df$Diferencial_1st)

  # trocas
  aux_df$Diferencial_Replaces[index_aux] = NA
  # km 1
  aux_df$Diferencial_1st[index_aux] = NA

  # km 2
  str.aux = rep(NA, nrow(aux_df))
  str.aux[index_aux] = str_extract(str.master[index_aux], patt='E *' %+% real_number %+% '+')
  str.aux[index_aux] = str_extract(str.aux[index_aux], patt= real_number %+% '+')
  aux_df$Diferencial_2nd[index_aux] = str.aux[index_aux]


  aux_df$pattern[index_aux] = 3

  #- Pattern 4 ----------------------------------------+
  str.master = aux_df$TEXT
  # index the NAs
  index_aux = is.na(aux_df$Diferencial_2nd) & is.na(aux_df$Diferencial_1st)

  # trocas
  aux_df$Diferencial_Replaces[index_aux] = NA
  # km 1
  aux_df$Diferencial_1st[index_aux] = NA

  # km 2
  str.aux = rep(NA, nrow(aux_df))
  str.aux[index_aux] = gsub(str.master[index_aux], patt='^.*( ' %+% real_number %+% '+km).*( ' %+% real_number %+% '+km).*( ' %+% real_number %+% '+km).*', repl = "\\3")
  str.aux[index_aux] = str_extract(str.aux[index_aux], patt = real_number %+% '+km')
  str.aux[index_aux] = str_extract(str.aux[index_aux], patt = real_number %+% '+')
  aux_df$Diferencial_2nd[index_aux] = str.aux[index_aux]

  aux_df$pattern[index_aux] = 4

  #- Pattern 5 ----------------------------------------+
  index_aux = is.na(aux_df$Diferencial_2nd) & is.na(aux_df$Diferencial_1st)

  # Gear Box
  str.master = aux_df$TEXT
  str.master[index_aux] = str_extract(str.master[index_aux], patt='Eixo.*')

  # index the NAs
  index_aux = is.na(aux_df$Diferencial_1st) & is.na(aux_df$Diferencial_2nd)

  # trocas
  aux_df$Diferencial_Replaces[index_aux] = NA

  str.aux = rep(NA, nrow(aux_df))
  # km 1
  str.aux[index_aux] = str_extract(str.master[index_aux], patt='Eixo *C* *' %+% real_number %+% '+')
  str.aux[index_aux] = str_extract(str.aux[index_aux], patt= real_number %+% '+')
  aux_df$Diferencial_1st[index_aux] = str.aux[index_aux]

  # km 2
  str.aux[index_aux] = str_extract(str.master[index_aux], patt='Eixo *C* *' %+% real_number %+% '+/*' %+% real_number %+% '+')
  str.aux[index_aux] = str_extract(str.aux[index_aux], patt='/+' %+% real_number %+% '+')
  str.aux[index_aux] = str_extract(str.aux[index_aux], patt= real_number %+% '+')
  aux_df$Diferencial_2nd[index_aux] = str.aux[index_aux]


  aux_df$pattern[index_aux] = 5

  #- result -------------------------------------------+
  # Removes pattern when not needed
  if(!add_patt) aux_df$pattern = NULL

  return(aux_df)


}


#'
#' Ar Motor Trocas
#'
#' Extract the information of Air Engine oil replacements
#'
#' @param aux_df the data.frame containing the columns "TEXT" from which the informations will be extracted.
#' @param add_patt if \code{TRUE} a column will be added informating what pattern was used to extrach the informations
#' @return The same \code{data.frame} containing extra columns and the respective values extraced from the "TEXT" column.
#' @details
#' @export
#' @examples
#'
ArMotor_trocas = function(aux_df, add_patt = TRUE){

  aux_df$pattern = 0

  # inicializating the columns
  aux_df$AirEngine_Oil_Replaces = NA
  aux_df$AirEngine_Oil = NA


  # aux_df
  str.master = aux_df$TEXT

  str.master = gsub(str.master, patt="Ar *Motor", repl = "ArMotor")
  str.master = str_extract(str.master, patt='ArMotor *[0-9]*T* *C* *' %+% real_number %+% '*C*')
  # str.master = str_extract(str.master, patt='ArMotor *[0-9]*T* *C* *([0-9]\\.[0-9]*|[0-9]*)C*')

  # aux_df$TEXT[is.na(str.master)]
  # aux_df$TEXT[!is.na(str.master)]

  # trocas
  str.aux = str_extract(str.master, patt='ArMotor *' %+% real_number %+% '*T')
  str.aux = str_extract(str.aux, patt= real_number %+% '*T')
  str.aux = str_extract(str.aux, patt= real_number %+% '+')
  aux_df$AirEngine_Oil_Replaces = str.aux


  # aux_df[is.na(str.aux),"TEXT"]

  # km
  str.aux = str_replace(str.master, patt='ArMotor *' %+% real_number %+% '*T', repl = "")
  # str.aux = str_extract(str.aux, patt='C* *([0-9]\\.[0-9]+|[0-9]+)')
  str.aux = str_extract(str.aux, patt='C* *' %+% real_number %+% '+')
  
  # str.aux = str_extract(str.aux, patt='([0-9]\\.[0-9]+|[0-9]+)')
  str.aux = str_extract(str.aux, patt= real_number %+% '+')
  
  aux_df$AirEngine_Oil = str.aux
  # aux_df[is.na(str.aux),"TEXT"]


  return(aux_df)
}


#' Arla Trocas
#'
#' Extract the information of Arla replacements
#'
#' @param aux_df the data.frame containing the columns "TEXT" from which the informations will be extracted.
#' @param add_patt if \code{TRUE} a column will be added informating what pattern was used to extrach the informations
#' @return The same \code{data.frame} containing extra columns and the respective values extraced from the "TEXT" column.
#' @details
#' @export
#' @examples
#'
Arla_trocas = function(aux_df){

  aux_df$Arla = NA

  str.master = aux_df$TEXT

  str.master = gsub(str.master, patt = 'Arla[0-9]{2}', repl ='Arla ')
  str.master = str_extract(str.master, patt='Arla[0-9]* *[0-9]*T* *C* *' %+% real_number %+% '*C*')
  # str.master = str_extract(str.master, patt='Arla[0-9]* *[0-9]*T* *C* *([0-9]\\.[0-9]*|[0-9]*)C*')
  

  # trocas
  str.aux = str_extract(str.master, patt='Arla[0-9]* *[0-9]*T')
  # str.aux = gsub(str.master, patt='Arla[0-9]{2}', repl = '')
  str.aux = str_extract(str.aux, patt='[0-9]*T')
  str.aux = str_extract(str.aux, patt='[0-9]+')
  aux_df$Arla_Replaces = str.aux


  # km
  str.aux = gsub(str.master, patt="Arla[0-9]* *[0-9]*T", repl = "")
  # str.aux = str_extract(str.aux, patt='C* *([0-9]\\.[0-9]+|[0-9]+)C*')
  # str.aux = str_extract(str.aux, patt='([0-9]\\.[0-9]+|[0-9]+)+')
  str.aux = str_extract(str.aux, patt='C* *' %+% real_number %+% '+C*')
  str.aux = str_extract(str.aux, patt='' %+% real_number %+% '+')
  aux_df$Arla = str.aux

  return(aux_df)


}


#' Correção de quilometragem
#'
#' Multiplies the mileage according to rules.
#'
#' @param aux_df the \code{data.frame} to be modified
#' @return 
#' @details
#' @export
#' @examples
#'
# Quilometragem muiltiplicada por 1000 ou 100
# de acrodo com a service_class.
mileage_correction = function(aux_df){

  # list of the columns that will have their value multiplied.
  coluns_multiply = c("Engine_Oil"
                      ,"Gearbox_Oil_1st"
                      ,"Gearbox_Oil_2nd"
                      ,"Diferencial_1st"
                      ,"Diferencial_2nd"
                      ,"AirEngine_Oil"
                      ,"Arla")
  
  for(i in coluns_multiply){
    aux_df[ ,i] = as.numeric(aux_df[ ,i] )
  }
  
  ####+
  # Get measurement unit of times
  ####+
  kms = list()
  hrs = list()
  na = list()
  
  
  # ---------------------------- #
  # -- info in  -- #
  # ---------------------------- #
  # No engine info
  index_NA = is.na(aux_df$Engine_Oil)
  aux_df_NA = aux_df[index_NA,]
  aux_df = aux_df[!index_NA,]
  
  # Hrs
  index_hrs = (aux_df$Engine_Oil >= 100) & (aux_df$Engine_Oil <= 1000) & (!is.na(aux_df$Engine_Oil))
  hrs[length(hrs) + 1] = list(aux_df[index_hrs,])
  
  index_kms = ((aux_df$Engine_Oil < 100) | (aux_df$Engine_Oil > 1000)) & (!is.na(aux_df$Engine_Oil))
  kms[length(kms) + 1] = list(aux_df[index_kms,])
  
  aux_df = aux_df_NA
  rm(aux_df_NA)
  
  # ---------------------------- #
  # -- service class -- #
  # ---------------------------- #
  # C
  index = grepl(aux_df$service_class, patt = '^C')
  kms[length(kms) + 1] = list(aux_df[index,])
  aux_df = aux_df[!index,]
  
  # H
  index = grepl(aux_df$service_class, patt = '^H')
  hrs[length(hrs) + 1] = list(aux_df[index,])
  aux_df = aux_df[!index,]
  
  # ---------------------------- #
  # -- Hour or Km using Arla info -- #
  # ---------------------------- #
  # aux_df = aux_df_NA
  index_NA = is.na(aux_df$Arla)
  aux_df_NA = aux_df[index_NA,]
  aux_df = aux_df[!index_NA,]
  
  # Hrs
  index_hrs = (aux_df$Arla >= 100) & (aux_df$Arla <= 1000) & (!is.na(aux_df$Arla))
  hrs[length(hrs) + 1] = list(aux_df[index_hrs,])
  # Kms
  index_kms = ((aux_df$Arla < 100) | (aux_df$Arla > 1000)) & (!is.na(aux_df$Arla))
  kms[length(kms) + 1] = list(aux_df[index_kms,])
  
  
  # -- Bind all -- #
  aux_df_hrs = bind_rows(hrs)
  aux_df_km = bind_rows(kms)
  
  
  ####+
  #Times measured in hs
  ####+

  # Values lower than 10 are multiplied by 1000
  for(i in coluns_multiply){
    
    index = as.numeric(aux_df_hrs[,i]) < 10
    # browser()
    aux_df_hrs[ ,i] = as.numeric(aux_df_hrs[,i]) * 1000^(index)
    
  }
  
  ####+
  #Times measured in km
  ####+
  
  aux_df_km = aux_df_km %>%
    mutate(
      
      # Engine_Oil  :  troca deve ser entre 9000 e 50000 km  (normal é 10.000,15.000 , 30.000 ou 40.000)
      Engine_Oil = ifelse((Engine_Oil >= 9000) & (Engine_Oil <= 50000)
                          , Engine_Oil, Engine_Oil*1000)
      
      
      # Condicional 1 : se valor menor que 1000 multiplicar por 1000.
      # Condicional 2 : Se valor maior que 1.000 deixar o valor original
      , Gearbox_Oil_1st = Gearbox_Oil_1st * 1000^(Gearbox_Oil_1st < 1000) 
      
      , Gearbox_Oil_2nd = Gearbox_Oil_2nd * 1000^(Gearbox_Oil_2nd < 1000) 
      
      # Multiplica o Cxx  por 1.000. Km nunca será menor que a troca de óleo
      , Diferencial_1st = Diferencial_1st*1000^(Diferencial_1st < 1000) 
      , Diferencial_2nd = Diferencial_2nd*1000^(Diferencial_2nd < 1000) 
      , AirEngine_Oil = AirEngine_Oil*1000^(AirEngine_Oil < 1000) 
      , Arla = Arla * 1000^(Arla < 1000) 
      
    )
  
  
  aux_df = rbind(aux_df_km, aux_df_hrs, aux_df_NA)

  
  return(aux_df)

}


