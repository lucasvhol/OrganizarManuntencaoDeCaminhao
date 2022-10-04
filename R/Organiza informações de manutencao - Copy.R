################################+
# Organizador de manutenções
# Esse script organiza as informações de manutenção preventiva dos veículos
# com plano de manutenção. As informações estão em formato desestruturado.
################################+

###################+
# packages and sources
###################+
###################+
###################+

# VERSAO DO R
getRversion()



 #devtools::install("C:/Leonardo/Projetos R/my_R_packages/extract.contract.info")

 rm(list = ls())
.libPaths("C:/RPackages")
install.packages('tidyverse')
install.packages('stringr')
install.packages('readxl')
install.packages('janitor')

install.packages("pillar", type="binary")

require(tidyverse)
require(stringr)
require(readxl)
library("extract.contract.info") - DELETAR
require(janitor)

# setwd("C:/Leonardo/Projetos R/Facilitadores")
setwd("C:/Leonardo/Projetos R/my_R_packages/extract.contract.info/R/")

# Sources
source("Functions Organiza informacoes de manutencao.R")



###################+
# data
###################+

### reads the data
# file = "//vcn.ds.volvo.net/vtc-cta/VTCPROJ05/026655/Silvia Surek/Assistencia/Contract_Info_Raw_Data.xlsx"
file = "//vcn.ds.volvo.net/vtc-cta/VTCPROJ05/026655/Silvia Surek/Assistencia/Contract_Info_Raw_Data_New.xlsx"

# file = file.choose() # alternative.
da = read_excel(path = file, sheet = "results final")


# names(da)
# dim(da)

# renames column to better standard
da = janitor::clean_names(da)


### "DE PARA" table

file = "//vcn.ds.volvo.net/vtc-cta/VTCPROJ05/026655/Silvia Surek/Assistencia/script_files/VF05 Contract Information - Modelo.xlsx"
DE_PARA.da = read_excel(path = file, sheet = 'DE_PARA')



###################+
# Plano BLUE
###################+

# Filters "Blue contract Types"
# blue.da = filter(da, current_contract_type == 'Blue')



###################+
# Concatenates the columns
###################+

# this line only makes sure that the object is of Type "data.frame"
aux.da = data.frame(da)


# removes all spaces in the begining.
i = 0
while(i < ncol(aux.da)){
  i = i+1
  aux.da[,i] = gsub(aux.da[,i], patt = "^ .* ", repl=  "")
}


geral.da = aux.da
geral.da$TEXT_ORIGINAL = paste(geral.da[,"line1"]
                      ,geral.da[, "line2"]
                      ,geral.da[, "line3"]
                      ,geral.da[, "line4"]
                      ,geral.da[, "line5"]
                      ,geral.da[, "line6"]
                      ,geral.da[, "line7"]
                      ,geral.da[, "line8"]
                      ,geral.da[, "line9"]
                      ,geral.da[, "line10"]
)

geral.da$TEXT  = geral.da$TEXT_ORIGINAL

#### Cleaning the strings
# Removes accent/diacritics.
# manualy replace some character because I had problems wuth encoding
geral.da$TEXT = geral.da$TEXT %>%
  gsub(patt = 'é', repl = 'e') %>%
  gsub(patt = 'ê', repl = 'e') %>%
  gsub(patt = 'á', repl = 'a') %>%
  gsub(patt = 'â', repl = 'a') %>%
  gsub(patt = 'ã', repl = 'a') %>%
  gsub(patt = 'ó', repl = 'o') %>%
  gsub(patt = 'ô', repl = 'o') %>%
  gsub(patt = 'õ', repl = 'o')

geral.da$TEXT = iconv(geral.da$TEXT, to = "ASCII//TRANSLIT")
# # Removes comas
# geral.da$TEXT = gsub(geral.da$TEXT, patt=",", repl = '')
# Removes information about Liters
geral.da$TEXT = gsub(geral.da$TEXT, patt="[0-9]*L", repl = '')
# Removes dots
geral.da$TEXT = gsub(geral.da$TEXT, patt='\\.*', repl = '')
# Replace comas
geral.da$TEXT = gsub(geral.da$TEXT, patt=',', repl = '.')


# # changes the label of Motor  to M
# geral.da$TEXT = gsub(geral.da$TEXT, patt="Motor", repl = 'M ')
# # changes the label of 'Motor[0-9]+'  to 'M '
geral.da$TEXT = gsub(geral.da$TEXT, patt=" Motor([0-9]+)", repl = ' M \\1')
# Ar Motor becomes ArMotor
geral.da$TEXT = gsub(geral.da$TEXT, patt="Ar Motor", repl = 'ArMotor')
# Ar 'M C +' becomes ArMotor 'M C'
geral.da$TEXT = gsub(geral.da$TEXT, patt="Motor C +", repl = 'Motor C')



# Changes the label of Cx Dir to CxDir
geral.da$TEXT = gsub(geral.da$TEXT, patt="Cx Dir", repl = 'CxDir')


# 'Caixa Motor' becomes 'Cx'
# geral.da$TEXT = gsub(geral.da$TEXT, patt="Caixa", repl = 'Cx')
# ' Meses C ' becomes  ' Meses C '
geral.da$TEXT = gsub(geral.da$TEXT, patt=' Meses C ', repl = ' Meses C ')
# # '<number> C ' becomes  ' <number> Cx '
# geral.da$TEXT = gsub(geral.da$TEXT, patt='([0-9]+ )(C )', repl = '\\1Cx ')
# 'CAIXA' becomes  'Caixa'
geral.da$TEXT = gsub(geral.da$TEXT, patt='CAIXA', repl = 'Caixa')



# Diferencial becomes Eixo
geral.da$TEXT = gsub(geral.da$TEXT, patt="Diferencial", repl = "Eixo ")
# Dif becomes D
geral.da$TEXT = gsub(geral.da$TEXT, patt="Dif", repl = "D ")
# # Eixo becomes E
# geral.da$TEXT = gsub(geral.da$TEXT, patt="Eixo", repl = "E ")
# # EIXO becomes Eixo
geral.da$TEXT = gsub(geral.da$TEXT, patt="EIXO", repl = "Eixo")


geral.daOLD = geral.da


##########+
# Motor
##########+

# geral.da$Engine_Oil_Replaces = NA
# geral.da$Engine_Oil = NA

geral.da = motor_trocas(geral.da)


##########+
# caixa
##########+

# geral.da$Gearbox_Oil_Replaces = NA
# geral.da$Gearbox_Oil_1st = NA
# geral.da$Gearbox_Oil_2nd = NA

geral.da = caixa_trocas(geral.da)


##########+
# Diferencial
##########+

# geral.da$Diferencial_Replaces = NA
# geral.da$Diferencial_1st = NA
# geral.da$Diferencial_2nd = NA


geral.da = diferencial_trocas(geral.da)

##########+
# Ar Motor
##########+
geral.da = ArMotor_trocas(geral.da)

##########+
# Arla
##########+
geral.da = Arla_trocas(geral.da)

##########+
# Aplicação
##########+

str.master = geral.da$TEXT

## DE PARA
# Here I replace the aplication by a longer mane (ex. AplicacaoVS) so it is easier to extract from the text.


aux.DE_PARA.da = DE_PARA.da # make a copy so I can rename without problems
index = which(names(aux.DE_PARA.da) == "PARA2")
names(aux.DE_PARA.da)[index] = "PARA"
str.master = f_DE_PARA(str.master,aux.DE_PARA.da )

# Extracts aplication
str.master = str_extract(str.master, patt='Aplicacao[A-Z]*')


# Here I replace the aplication by a shorter mane
aux.DE_PARA.da = DE_PARA.da # make a copy so I can rename without problems
aux.DE_PARA.da = aux.DE_PARA.da[,c("PARA2", "PARA3")]
names(aux.DE_PARA.da) = c("DE", "PARA")
str.master = f_DE_PARA(str.master,aux.DE_PARA.da )

# creates column
geral.da$Aplicacao = str.master

rm(str.master)


##########+
# Desconsiderar Revisões
##########+

str.master = geral.da$TEXT
str.master = str_extract(str.master, patt='Desconsiderar Revis')

geral.da$complete_service = "contratado"
geral.da$complete_service[!is.na(str.master)] = "nao contratado"

geral.da$basic_service = "contratado"
geral.da$basic_service[!is.na(str.master)] = "nao contratado"

rm(str.master)


##########+
# Regulagem de Valvula
##########+

str.master = geral.da$TEXT
str.master = str_extract(str.master, patt='Reg Valvulas contratada')

geral.da$valve_adjustment = "nao contratado"
geral.da$valve_adjustment[!is.na(str.master)] = "contratado"


##############################+
# Transformações
##############################+

# ----------+
# Troca valor de colunas
# Quando só ha informação de primeira troca de óles, essa na verdade deve ser a segunda troca.
# ----------+

# debugonce(f_primeira_troca_vira_segunda)
dim(geral.da)
geral.da_OLD = geral.da
aux.da = geral.da
aux.da = f_primeira_troca_vira_segunda(aux.da,"Gearbox_Oil_1st")
aux.da = f_primeira_troca_vira_segunda(aux.da,"Diferencial_1st")
# aux.da = f_primeira_troca_vira_segunda(aux.da,"Direcao_km")
geral.da = aux.da


# ----------+
# transforma 0 (zero) em NA
# ----------+


for(i in c("Gearbox_Oil_1st",
           "Gearbox_Oil_2nd",
           "Diferencial_1st",
           "Diferencial_2nd",
           "AirEngine_Oil",
           "Arla")){


  geral.da[,i][geral.da[,i] == 0] = NA

}


# ----------+
# Quilometragem muiltiplicada por 1000 ou 100
# de acrodo com a service_class.
# ----------+

geral.da = mileage_correction(geral.da)


##############################+
# Selecao de colunas
##############################+

index = names(geral.da) %in%
  c( "line1"
     , "line2"
     , "line3"
     , "line4"
     , "line5"
     , "line6"
     , "line7"
     , "line8"
     , "line9"
     , "line10"
     , "x_1"
     , "x_2"
     , "Direcao_trocas"
     , "Direcao_km"
     , "Direcao_km2"
     , "service_class2"
     , "pattern"
  )

index = !index
geral.da = geral.da[,index]


##########+
# Save File
##########+

write.csv2(geral.da, file = paste0("//vcn.ds.volvo.net/vtc-cta/VTCPROJ05/026655/Silvia Surek/Assistencia/Contract_Info_Data",
           '_',
           as.character(Sys.Date()),
           ".csv"),
           row.names = FALSE)


# # ------------------------------
# no_corection_geral.da = geral.da
# geral.da = no_corection_geral.da
#
#
# aux_geral.da = geral.da %>%
#   filter(chassis_number %in% c("E-836035", "E-852496"))
#
#
# aux_geral.da$AirEngine_Oil
#
# # debugonce(mileage_correction)
# aux_correction = aux_geral.da %>%
#   mileage_correction()
#
#
#
# aux_correction$AirEngine_Oil
# aux_geral.da$AirEngine_Oil
# # ------------------------------


dim(geral.da)



