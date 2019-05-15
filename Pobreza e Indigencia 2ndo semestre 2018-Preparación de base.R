# Descargar las EPH del tercer y cuarto trimestre de 2018 desde el INDEC:
#  https://www.indec.gob.ar/bases-de-datos.asp

setwd("D:/Pobreza-2s-2018")

##Cálculo de Pobreza por Línea

library(tidyverse, warn = FALSE)

library(openxlsx, warn = FALSE)


var.ind <- c('CODUSU','NRO_HOGAR' ,'COMPONENTE','ANO4','TRIMESTRE','REGION',
'AGLOMERADO', 'PONDERA', 'CH03','CH04', 'CH06','NIVEL_ED','IPCF', 'ITF', 'PONDIH','P47T',
'P21','PONDIIO','PONDII','PP04D_COD')


tmp<- tempdir() 
unzip("EPH_usu_3_Trim_2018_txt.zip",exdir=tmp)


hogar.318 <- read.table("C:\\Users\\Alumno\\AppData\\Local\\Temp\\RtmpUBkCce\\usu_hogar_t318.txt", 
sep=";", dec=",", header = TRUE, fill = TRUE) 

zmp<- tempdir()
unzip("EPH_usu_4_Trim_2018_txt.zip",exdir=zmp)

hogar.418 <- read.table("C:\\Users\\Alumno\\AppData\\Local\\Temp\\RtmpUBkCce\\usu_hogar_t418.txt", 
sep=";", dec=",", header = TRUE, fill = TRUE) 


Adequi <- read.xlsx("ADEQUI.xlsx")
CBA <- read.xlsx("CANASTAS-2S-2018.xlsx",sheet = "CBA")
CBT <- read.xlsx("CANASTAS-2S-2018.xlsx",sheet = "CBT")
dic.regiones <- read.xlsx("Regiones.xlsx")

CBA <- CBA %>%
mutate(Canasta = 'CBA')
CBT <- CBT %>%
mutate(Canasta = 'CBT')
Canastas_Reg <- bind_rows(CBA,CBT) %>%
gather(.,Region, Valor, c(3:(ncol(.)-1) )) %>%
mutate(Trimestre = case_when(Mes %in% c(1:3) ~1,
Mes %in% c(4:6) ~2,
Mes %in% c(7:9) ~3,
Mes %in% c(10:12) ~4),
Periodo = paste(Año, Trimestre, sep='.'))

Canastas_Reg_2 <- Canastas_Reg %>%
group_by(Canasta, Region, Periodo) %>%
summarise(Valor = mean(Valor)) %>%
spread(., Canasta,Valor) %>%
left_join(., dic.regiones, by = "Region") %>%
ungroup()




#Pobreza Individual



individual.318 <- read.table("C:\\Users\\Alumno\\AppData\\Local\\Temp\\RtmpUBkCce\\usu_individual_t318.txt", 
sep=";", dec=",", header = TRUE, fill = TRUE) %>% 
              select(var.ind)


individual.418 <- read.table("C:\\Users\\Alumno\\AppData\\Local\\Temp\\RtmpUBkCce\\usu_individual_t418.txt", 
sep=";", dec=",", header = TRUE, fill = TRUE) %>% 
              select(var.ind)

bases_3t_2018 <-individual.318 %>% 
  left_join(., hogar.318 %>% 
              select(CODUSU, NRO_HOGAR,IV3,IV4,IV5,IV6,IV7,IV8,IV9,IV10,IV11, II2, IX_TOT), by = c("CODUSU", "NRO_HOGAR"))


bases_4t_2018 <-individual.418 %>% 
  left_join(., hogar.418 %>% 
              select(CODUSU, NRO_HOGAR,IV3,IV4,IV5,IV6,IV7,IV8,IV9,IV10,IV11, II2, IX_TOT), by = c("CODUSU", "NRO_HOGAR"))




Base <- bind_rows(bases_3t_2018,bases_4t_2018)

##Unidades de Adulto Equivalente por hogar en la base Individual

Pobreza_Individual <- Base %>%
mutate(Periodo = paste(ANO4, TRIMESTRE, sep='.')) %>%
left_join(., Adequi, by = c("CH04", "CH06")) %>%
left_join(., Canastas_Reg_2, by = c("REGION", "Periodo"))



Pobreza_Individual_paso2 <- Pobreza_Individual %>%
group_by(CODUSU, NRO_HOGAR, Periodo) %>%
mutate(Adequi_hogar = sum(adequi)) %>%
ungroup()


Pobreza_Individual_paso3 <- Pobreza_Individual_paso2 %>%
mutate(CBA = CBA*Adequi_hogar,
CBT = CBT*Adequi_hogar,
Situacion = case_when(ITF<CBA ~ 'Indigente',
ITF>=CBA & ITF<CBT ~ 'Pobre',
ITF>=CBT ~ 'No.Pobre'))



EPH_provincias<- Pobreza_Individual_paso3 %>%
mutate(
Provincia = case_when(AGLOMERADO== 02 | AGLOMERADO== 03  | AGLOMERADO== 34 | AGLOMERADO== 38 ~ 'Buenos Aires',
AGLOMERADO== 04 | AGLOMERADO== 05 ~ 'Santa Fe',
AGLOMERADO== 06 | AGLOMERADO== 14 ~ 'Entre Ríos',
AGLOMERADO== 07 ~ 'Misiones',
AGLOMERADO== 08 ~ 'Chaco',
AGLOMERADO== 09 | AGLOMERADO== 91 ~ 'Chubut',
AGLOMERADO== 10 ~ 'Mendoza',
AGLOMERADO== 13 | AGLOMERADO== 36 ~ 'Córdoba',
AGLOMERADO== 12  ~ 'Corrientes',
AGLOMERADO== 15 ~ 'Formosa',
AGLOMERADO== 17 ~ 'Neuquén',
AGLOMERADO== 18 ~ 'Santiago del Estero',
AGLOMERADO== 19 ~ 'Jujuy',
AGLOMERADO== 20 ~ 'Santa Cruz',
AGLOMERADO== 22 ~ 'Catamarca',
AGLOMERADO== 23 ~ 'Salta',
AGLOMERADO== 25 ~ 'La Rioja',
AGLOMERADO== 26 ~ 'San Luis',
AGLOMERADO== 27 ~ 'San Juan',
AGLOMERADO== 29 ~ 'Tucumán',
AGLOMERADO== 30 ~ 'La Pampa',
AGLOMERADO== 31 ~ 'Tierra del Fuego',
AGLOMERADO== 32 | AGLOMERADO== 33 ~ 'Ciudad Autónoma de Buenos Aires',
AGLOMERADO== 93 ~ 'Río Negro',
TRUE ~ '0'))


saveRDS(EPH_provincias, "EPH_provincias2.rds")


