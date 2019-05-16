library(tidyverse, warn = FALSE)
library(openxlsx, warn = FALSE)
library(DT)
library(rmarkdown)
library(knitr)
library(kableExtra)
library(formattable)
library(Hmisc)
library(laeken)
library(convey)
library(survey)
library(vardpoor)

EPH_provincias <- readRDS("EPH_provincias2.rds")


# Tasa Incidencia, descomposición por regiones

FGT2<- function(x,wt,alfa,lp,obs) {
each=numeric(obs)

for (i in 1:obs) {
each[i]= (1-x[[i]]/lp[[i]])^ alfa
if (x[[i]] >= lp[[i]]) {
each[i] = 0 }
}

fgt2=(sum(each*wt)/sum(wt))*100

print(fgt2)

}


fgttotalpob=FGT2(EPH_provincias$ITF,EPH_provincias$PONDIH,0,EPH_provincias$CBT,114297)
fgttotalind=FGT2(EPH_provincias$ITF,EPH_provincias$PONDIH,0,EPH_provincias$CBA,114297)


Pobreza_resumen_region <-EPH_provincias  %>%
group_by(Region) %>%
summarise(Tasa_pobreza = sum(PONDIH[Situacion %in% c('Pobre', 'Indigente')],na.rm = TRUE)/
sum(PONDIH,na.rm = TRUE),
Tasa_indigencia = sum(PONDIH[Situacion == 'Indigente'],na.rm = TRUE)/
sum(PONDIH,na.rm = TRUE))

Pobreza_resumen_prov <-EPH_provincias  %>%
group_by(Region,Provincia) %>%
summarise(Tasa_pobreza = sum(PONDIH[Situacion %in% c('Pobre', 'Indigente')],na.rm = TRUE)/
sum(PONDIH,na.rm = TRUE),
Tasa_indigencia = sum(PONDIH[Situacion == 'Indigente'],na.rm = TRUE)/
sum(PONDIH,na.rm = TRUE))

Total_país<-list('TotalPaís',as.numeric( fgttotalpob/100),as.numeric(fgttotalind/100))

names(Total_país)= names(Pobreza_resumen_region)


ggplot(Pobreza_resumen_region, aes(Region,Tasa_pobreza,label = sprintf("%1.1f%%", 100*Tasa_pobreza))) +
geom_bar(aes(fill= "Tasa de Pobreza"),stat="identity")+
geom_text(position = position_stack(vjust = 0.98), size=3)+
geom_bar(aes(y=Tasa_indigencia,fill= "Tasa de Indigencia"),stat="identity")+
geom_text(aes(y=Tasa_indigencia,label = sprintf("%1.1f%%", 100*Tasa_indigencia)),position = position_stack(vjust = 0.93), size=3) +
labs(y = "", x = "Región", caption = "En base a datos obtenidos del INDEC",title="Gráfico 1") +
scale_y_continuous(breaks = seq(0.01,0.42, by = 0.04), labels=scales::percent) +
theme_classic() + theme( axis.text.x = element_text(size = rel(1.3),colour="black")) +
theme(legend.position = c(1, 1), legend.justification = c(1, 1))+
guides(fill = guide_legend(title = NULL))+
theme(plot.title = element_text(
    size = rel(1.3), lineheight = .9,
    face = "bold.italic", colour = "darkblue"
  )) +
theme(axis.title.x = element_text(
     lineheight = .9,
     face = "bold", colour = "black"
  ))




# Tasa de Incidencia, descomposición por Aglomerados

Aglomerados <- read.xlsx("Aglomerados EPH.xlsx")
EPH_provincias_aglo<- EPH_provincias %>% left_join(Aglomerados)



Pobreza_resumen_aglomerado <-EPH_provincias_aglo  %>%
group_by(Nom_Aglo) %>%
summarise(Tasa_pobreza = sum(PONDIH[Situacion %in% c('Pobre', 'Indigente')],na.rm = TRUE)/
sum(PONDIH,na.rm = TRUE),
Tasa_indigencia = sum(PONDIH[Situacion == 'Indigente'],na.rm = TRUE)/
sum(PONDIH,na.rm = TRUE))


ggplot(Pobreza_resumen_aglomerado, aes(reorder(Nom_Aglo,Tasa_pobreza),Tasa_pobreza,label = sprintf("%1.1f%%", 100*Tasa_pobreza))) +
geom_bar(aes(fill= "Tasa de Pobreza"),stat="identity")+
geom_text(position = position_stack(vjust = 1.038,reverse=FALSE), size=3)+
geom_bar(aes(y=Tasa_indigencia,fill= "Tasa de Indigencia"),stat="identity")+
geom_text(aes(y=Tasa_indigencia,label = sprintf("%1.1f%%", 100*Tasa_indigencia)),position = position_stack(vjust = 1.15,reverse=FALSE), size=3)+
labs(y = "", x = "Aglomerado", caption = "En base a datos obtenidos del INDEC",title="Gráfico 2") +
scale_y_continuous(breaks = seq(0.01,0.50, by = 0.04), labels=scales::percent) +
theme_classic()   + coord_flip()+
theme_classic() + theme( axis.text.x = element_text(size = rel(1.3),colour="black")) +
theme(legend.position = c(1, 0), legend.justification = c(1, 0))+
guides(fill = guide_legend(title = NULL))+
theme(plot.title = element_text(
    size = rel(1.3), lineheight = .9,
    face = "bold.italic", colour = "darkblue"
  )) 


# Brecha de pobreza y descomposición regional

FGTtotal=FGT2(EPH_provincias$ITF,EPH_provincias$PONDIH,1,EPH_provincias$CBT,114297)
FGTtotalInd=FGT2(EPH_provincias$ITF,EPH_provincias$PONDIH,1,EPH_provincias$CBA,114297)



matrizregiones=as.matrix(EPH_provincias %>% 
count(REGION, wt= PONDIH)/sum(EPH_provincias$PONDIH) )



baseGBA= filter(EPH_provincias,REGION==01) 
baseCUY=filter(EPH_provincias,REGION==42)
baseNEA=filter(EPH_provincias,REGION==41)
baseNOA= filter(EPH_provincias,REGION==40) 
basePAM=filter(EPH_provincias,REGION==43)
basePAT=filter(EPH_provincias,REGION==44)

FGTregionGBA= FGT2(baseGBA$ITF,baseGBA$PONDIH,1,baseGBA$CBT,dim(baseGBA)[1])
FGTregionCUY= FGT2(baseCUY$ITF,baseCUY$PONDIH,1,baseCUY$CBT,dim(baseCUY)[1])
FGTregionNEA= FGT2(baseNEA$ITF,baseNEA$PONDIH,1,baseNEA$CBT,dim(baseNEA)[1])
FGTregionNOA= FGT2(baseNOA$ITF,baseNOA$PONDIH,1,baseNOA$CBT,dim(baseNOA)[1])
FGTregionPAM= FGT2(basePAM$ITF,basePAM$PONDIH,1,basePAM$CBT,dim(basePAM)[1])
FGTregionPAT= FGT2(basePAT$ITF,basePAT$PONDIH,1,basePAT$CBT,dim(basePAT)[1])


#Contribución de la región GBA al FGT
ContGBA<-(FGTregionGBA*matrizregiones[1,2] / FGTtotal ) * 100


#Contribución de la región CUY al FGT
ContCUY<-(FGTregionCUY*matrizregiones[2,2] / FGTtotal ) * 100

#Contribución de la región NEA al FGT
ContNEA<-(FGTregionNEA*matrizregiones[3,2] / FGTtotal ) * 100

#Contribución de la región NOA al FGT
ContNOA<-(FGTregionNOA*matrizregiones[4,2] / FGTtotal ) * 100


#Contribución de la región PAM al FGT
ContPAM<-(FGTregionPAM*matrizregiones[5,2] / FGTtotal ) * 100

#Contribución de la región PAT al FGT
ContPAT<-(FGTregionPAT*matrizregiones[6,2] / FGTtotal ) * 100

base2<- tibble(Región =c("GBA","Cuyo","Noreste","Nororeste","Pampeana","Patagonia","Total-País"),
"FGT(1)" = c(round(FGTregionGBA,2),round(FGTregionCUY,2),round(FGTregionNEA,2),round(FGTregionNOA,2),round(FGTregionPAM,2),round(FGTregionPAT,2),round(FGTtotal,2)),
"Contribución Regional" = c(round(ContGBA,2),round(ContCUY,2),round(ContNEA,2),round(ContNOA,2),round(ContPAM,2),round(ContPAT,2),100))


# Severidad  de la pobreza y descomposición regional

FGTtotal2<- FGT2(EPH_provincias$ITF,EPH_provincias$PONDIH,2,EPH_provincias$CBT,114297)
FGTtotalInd2<- FGT2(EPH_provincias$ITF,EPH_provincias$PONDIH,2,EPH_provincias$CBA,114297)



matrizregiones2<- as.matrix(EPH_provincias %>% 
count(REGION, wt= PONDIH)/sum(EPH_provincias$PONDIH) )



baseGBA2= filter(EPH_provincias,REGION==01) 
baseCUY2=filter(EPH_provincias,REGION==42)
baseNEA2=filter(EPH_provincias,REGION==41)
baseNOA2= filter(EPH_provincias,REGION==40) 
basePAM2=filter(EPH_provincias,REGION==43)
basePAT2=filter(EPH_provincias,REGION==44)

FGTregionGBA2= FGT2(baseGBA$ITF,baseGBA$PONDIH,2,baseGBA$CBT,dim(baseGBA)[1])
FGTregionCUY2= FGT2(baseCUY$ITF,baseCUY$PONDIH,2,baseCUY$CBT,dim(baseCUY)[1])
FGTregionNEA2= FGT2(baseNEA$ITF,baseNEA$PONDIH,2,baseNEA$CBT,dim(baseNEA)[1])
FGTregionNOA2= FGT2(baseNOA$ITF,baseNOA$PONDIH,2,baseNOA$CBT,dim(baseNOA)[1])
FGTregionPAM2= FGT2(basePAM$ITF,basePAM$PONDIH,2,basePAM$CBT,dim(basePAM)[1])
FGTregionPAT2= FGT2(basePAT$ITF,basePAT$PONDIH,2,basePAT$CBT,dim(basePAT)[1])


#Contribución de la región GBA al FGT
ContGBA2<-(FGTregionGBA2*matrizregiones[1,2] / FGTtotal2 ) * 100


#Contribución de la región CUY al FGT
ContCUY2<-(FGTregionCUY2*matrizregiones[2,2] / FGTtotal2 ) * 100

#Contribución de la región NEA al FGT
ContNEA2<-(FGTregionNEA2*matrizregiones[3,2] / FGTtotal2 ) * 100

#Contribución de la región NOA al FGT
ContNOA2<-(FGTregionNOA2*matrizregiones[4,2] / FGTtotal2 ) * 100


#Contribución de la región PAM al FGT
ContPAM2<-(FGTregionPAM2*matrizregiones[5,2] / FGTtotal2 ) * 100

#Contribución de la región PAT al FGT
ContPAT2<-(FGTregionPAT2*matrizregiones[6,2] / FGTtotal2 ) * 100

base3<- tibble(Región =c("GBA","Cuyo","Noreste","Nororeste","Pampeana","Patagonia","Total-País"),
"FGT(2)" = c(round(FGTregionGBA2,2),round(FGTregionCUY2,2),round(FGTregionNEA2,2),round(FGTregionNOA2,2),round(FGTregionPAM2,2),round(FGTregionPAT2,2),round(FGTtotal2,2)),
"Contribución Regional" = c(round(ContGBA2,2),round(ContCUY2,2),round(ContNEA2,2),round(ContNOA2,2),round(ContPAM2,2),round(ContPAT2,2),100))


# Algunas características habitacionales, rango etario y nivel educativo en el análisis de pobreza e indigencia.


EPH_provincias_2 <- EPH_provincias  %>% 
  mutate(Sexo = as.character(CH04),
         Sexo = case_when(Sexo=="1" ~ "Varones",
                          Sexo=="2" ~ "Mujeres"),
         PP04D_COD = as.character(PP04D_COD),
         PP04D_COD = ifelse(is.na(PP04D_COD),"0",PP04D_COD),
         PP04D_COD = case_when(nchar(PP04D_COD) == 5 ~ PP04D_COD,
                               nchar(PP04D_COD) == 4 ~ paste0("0", PP04D_COD),
                               nchar(PP04D_COD) == 3 ~ paste0("00", PP04D_COD),
                               nchar(PP04D_COD) == 2 ~ paste0("000", PP04D_COD),
                               nchar(PP04D_COD) == 1 ~ paste0("0000", PP04D_COD)),
         CALIFICACION = substr(PP04D_COD, 5, 5),
         CALIFICACION = case_when(CALIFICACION=="1" ~ "Profesionales",
                                  CALIFICACION=="2" ~ "Técnicos",
                                  CALIFICACION=="3" ~ "Operativos",
                                  CALIFICACION=="4" ~ "No Calificados",
                                  TRUE ~ "0"),
         CALIFICACION = factor(CALIFICACION, c("Profesionales", "Técnicos", "Operativos", "No Calificados")),
         JERARQUIA = substr(PP04D_COD, 3, 3),
         JERARQUIA = case_when(JERARQUIA=="0" ~ "Dirección",
                               JERARQUIA=="1" ~ "Cuentapropia",
                               JERARQUIA=="2" ~ "Jefes",
                               JERARQUIA=="3" ~ "Trabajadores Asalariados",
                               TRUE ~ "0"),
         JERARQUIA = factor(JERARQUIA, c("Jefes", "Dirección", "Trabajadores Asalariados", "Cuentapropia")),
         NIVEL_EDUCATIVO = case_when(NIVEL_ED==1 ~ "Primaria incompleta(incluye educación especial)",
                                     NIVEL_ED==2 ~ "Primaria completa",
                                     NIVEL_ED==3 ~ "Secundaria incompleta",
                                     NIVEL_ED==4 ~ "Secundaria completa",
                                     NIVEL_ED==5 ~ "Superior Universitaria incompleta",
                                     NIVEL_ED==6 ~ "Superior Universitaria completa",
                                     NIVEL_ED==7 ~ "Sin Instrucción"),
         NIVEL_EDUCATIVO = factor(NIVEL_EDUCATIVO, levels = c("Sin Instrucción", "Primaria incompleta(incluye educación especial)","Primaria completa" ,"Secundaria incompleta","Secundaria completa", "Superior Universitaria incompleta","Superior Universitaria completa")),
         GRUPO_EDAD = case_when(CH06 >= -1 & CH06 <= 13 ~ "De 13 años o menos",
                                CH06 >= 14 & CH06 <= 29 ~ "De 14 a 29 años",
                                CH06 >= 30 & CH06 <= 49 ~ "De 30 a 49 años",
                                CH06 >= 50 & CH06 <= 64 ~ "De 50 a 64 años",
                                CH06 >= 65  ~ "De 65 años y más"
))

EPH_provincias_3 <- EPH_provincias_2  %>% 
  mutate( jefe = case_when(CH03 == 1 ~ 1,
                            CH03 == 0 | CH03 != 1 ~ 0),
          conyuge = case_when(CH03 == 2 ~ 1,
                            CH03 == 0 | CH03 != 2 ~ 0),
          agua = case_when((IV6 == 1 | IV6 == 2)  ~ 1,
                            (IV6 == 3 | IV6 == 9) ~ 0),
          cuartos = II2,
          miembros= IX_TOT,
          rat_miembros_cuartos = case_when( cuartos != 0 ~   miembros / cuartos,
                                            cuartos ==0 ~ 1),
          matpreca = case_when( IV3==2| IV3 == 3 | IV3 == 4 | IV4 == 6  | IV4 == 7 | IV5 == 2   ~ 1 ,
                                (IV3!=2 & IV3 != 3 & IV3 != 4) |  (IV4 != 6  & IV4 != 7 ) | IV5 != 2  ~ 0),
          bano = case_when ( IV8 == 1 & (IV9 == 1 | IV9 == 2)  & IV10 == 1 ~ 1,
                             ((IV8 == 2 | IV8 ==9) | (IV9 == 3 | IV9 == 9))  | IV10 != 1  ~ 0),
          cloacas = case_when ( IV10 == 1 & IV11 == 1  ~ 1,
                             IV10 != 1 | IV11 != 1   ~ 0 )
               )




EPH_provincias_4 <- EPH_provincias_3 %>%
mutate( Pobre = case_when ( ITF<CBT ~ 1,
                           ITF>=CBT ~ 0 ))



MediasPobreza <- EPH_provincias_4 %>% 
mutate( Indicador_Pob = case_when(Pobre == 1 ~ "Sí",
Pobre == 0 ~ "No")) %>%
group_by(Indicador_Pob) %>%
summarise(Materiales_precarios = wtd.mean(matpreca, PONDIH,  na.rm=TRUE),
Agua = wtd.mean(agua, PONDIH,  na.rm=TRUE),
Cloacas = wtd.mean(cloacas, PONDIH,  na.rm=TRUE),
Miembros_por_cuarto = wtd.mean(rat_miembros_cuartos, PONDIH,  na.rm=TRUE),
Baño = wtd.mean(bano, PONDIH,  na.rm=TRUE)
) %>%  knitr::kable(align = 'c',digits = 2,,
caption = "** Cuadro 5- Proporción de indicadores habitacionales según condición de Pobreza**", col.names=c("¿Es pobre?", "Materiales precarios", "Agua", "Cloacas", "Miembros por cuarto", "Baño"))%>% 
kable_styling("striped", full_width = FALSE)%>%
row_spec(0, bold = T,color="white", background = "lightblue") 
MediasPobreza


Pobreza_resumen_totales <- EPH_provincias_4  %>%
summarise(Total_Pobres = sum(PONDIH[Situacion %in% c('Pobre', 'Indigente')],na.rm = TRUE),
Total_indigentes = sum(PONDIH[Situacion == 'Indigente'],na.rm = TRUE),
Total=sum(PONDIH,na.rm = TRUE)
) 

Pobreza__edad <- EPH_provincias_4 %>%
group_by(GRUPO_EDAD) %>%
summarise(Porcentaje=sum(PONDIH,na.rm = TRUE)/Pobreza_resumen_totales$Total,
Tasa_pobreza = sum(PONDIH[Situacion %in% c('Pobre', 'Indigente')],na.rm = TRUE)/
Pobreza_resumen_totales$Total_Pobres,
Tasa_indigencia = sum(PONDIH[Situacion == 'Indigente'],na.rm = TRUE)/
Pobreza_resumen_totales$Total_indigentes
)

Pobreza_resumen_edad <- EPH_provincias_4 %>%
group_by(GRUPO_EDAD) %>%
summarise(Tasa_pobreza = sum(PONDIH[Situacion %in% c('Pobre', 'Indigente')],na.rm = TRUE)/
sum(PONDIH,na.rm = TRUE),
Tasa_indigencia = sum(PONDIH[Situacion == 'Indigente'],na.rm = TRUE)/
sum(PONDIH,na.rm = TRUE)) %>%  knitr::kable(align = 'c',digits = 2,
caption = "**Cuadro6-Pobreza e Indigencia según Rango Etario**",col.names=c("Rango Etario", "Tasa de pobreza", "Tasa de Indigencia"))%>% 
kable_styling("striped", full_width = FALSE)%>%
row_spec(0, bold = T,color="white", background = "lightblue") 
Pobreza_resumen_edad

Pobreza_resumen_educación <- EPH_provincias_4 %>%
group_by(NIVEL_EDUCATIVO) %>%
summarise(Tasa_pobreza = sum(PONDIH[Situacion %in% c('Pobre', 'Indigente')],na.rm = TRUE)/
sum(PONDIH,na.rm = TRUE),
Tasa_indigencia = sum(PONDIH[Situacion == 'Indigente'],na.rm = TRUE)/
sum(PONDIH,na.rm = TRUE)) %>%  knitr::kable(align = 'c',digits = 2,
caption = "**Cuadro 7-Pobreza e Indigencia según Nivel Educativo**",col.names=c("Nivel Educativo", "Tasa de pobreza", "Tasa de Indigencia"))%>% 
kable_styling("striped", full_width = FALSE)%>%
row_spec(0, bold = T,color="white", background = "lightblue") 
Pobreza_resumen_educación


# Pobreza Multidimensional

## Tasa de incidencia multidimensional


EPH_provincias_5 <-  EPH_provincias_4 %>%
mutate ( indic1 = Pobre, 
 indic2 =case_when (rat_miembros_cuartos > 3 ~ 1,
                            rat_miembros_cuartos <= 3 ~ 0),
indic3 = case_when (matpreca == 1 ~ 1,
                    matpreca == 0 ~ 0),
indic4 = case_when (agua == 0 ~ 1,
                    agua == 1 ~0),
indic5 = case_when (bano == 0 ~ 1,
                    bano == 1 ~0),
indic6 = case_when (cloacas == 0 ~ 1,
                    cloacas == 1 ~0),
npriv = indic1+indic2+indic3+indic4+indic5+indic6,
pobre1 = case_when (npriv >= 1 ~ 1,
                    npriv < 1 ~ 0),
pobre2 = case_when (npriv >= 2 ~ 1,
                    npriv < 2 ~ 0),
pobre3 = case_when (npriv >= 3 ~ 1,
                    npriv < 3 ~ 0),
pobre4 = case_when (npriv >= 4 ~ 1,
                    npriv < 4 ~ 0),
pobre5 = case_when (npriv >= 5 ~ 1,
                    npriv < 5 ~ 0),
pobre6 = case_when (npriv >= 6 ~ 1,
                    npriv < 6 ~ 0)
) 

dimensiones_pob <- EPH_provincias_5 %>%
summarise ( 
pobre_una_dimensión = wtd.mean(EPH_provincias_5$pobre1, EPH_provincias_5$PONDIH,na.rm=TRUE),
pobre_dos_dimensiones= wtd.mean(EPH_provincias_5$pobre2, EPH_provincias_5$PONDIH,na.rm=TRUE),
pobre_tres_dimensiones= wtd.mean(EPH_provincias_5$pobre3, EPH_provincias_5$PONDIH,na.rm=TRUE),
pobre_cuatro_dimensiones= wtd.mean(EPH_provincias_5$pobre4, EPH_provincias_5$PONDIH,na.rm=TRUE),
pobre_cinco_dimensiones= wtd.mean(EPH_provincias_5$pobre5, EPH_provincias_5$PONDIH,na.rm=TRUE),
pobre_seis_dimensiones= wtd.mean(EPH_provincias_5$pobre6, EPH_provincias_5$PONDIH,na.rm=TRUE)
)

        
k_dimensiones <- seq(1,6,1)

base4<- cbind (k_dimensiones, t(100 * dimensiones_pob))

knitr::kable(base4,align = 'c',digits = 2,,
caption = " **Cuadro 8-Tasa de Pobreza Multidimensional   
_Valores alternativos de k_**", 
col.names=c("K-Dimensiones", "Tasa de Pobreza (%)"), row.names=FALSE)%>% 
kable_styling("striped", full_width = FALSE) %>%
row_spec(0, bold = T,color="white", background = "lightblue") 

## Medidas de Pobreza Multidimensional de Alkire y Foster


EPH_provincias_5_des<-svydesign( ids = ~CODUSU , strata = ~Region ,  weights = ~PONDIH , data = EPH_provincias_5 )
EPH_provincias_5_des <- convey_prep(EPH_provincias_5_des)
EPH_provincias_5_des <- update(EPH_provincias_5_des, ITF = as.numeric( ITF ))
EPH_provincias_5_des <- update(EPH_provincias_5_des, nomatpreca = (1-matpreca))
EPH_provincias_5_des <- update(EPH_provincias_5_des, nohacina = (1-indic2))
EPH_provincias_5_des <- update(EPH_provincias_5_des, Region = as.factor(Region))
EPH_provincias_5_des <- update(EPH_provincias_5_des, REGION = as.factor(REGION))


cut2<- list(EPH_provincias_5$CBT,1,1,1)




AF0<- numeric(4)
AF1<- numeric(4)
AF2<- numeric(4)


for (i in seq(0.25,1,0.25)){
af<-svyafcdec( ~ ITF + nohacina + nomatpreca + cloacas ,design= EPH_provincias_5_des ,group= ~REGION , k = i, g = 0, cutoffs = cut2 )
AF0[i*4]<-af$overall[1]
}


for (i in seq(0.25,1,0.25)){
af<-svyafcdec( ~ ITF + nohacina + nomatpreca + cloacas ,design= EPH_provincias_5_des ,group= ~REGION , k = i, g = 1, cutoffs = cut2 )
AF1[i*4]<-af$overall[1]
}

for (i in seq(0.25,1,0.25)){
af<-svyafcdec( ~ ITF + nohacina + nomatpreca + cloacas ,design= EPH_provincias_5_des ,group= ~REGION , k = i, g = 2, cutoffs = cut2 )
AF2[i*4]<-af$overall[1]
}



base8<-data.frame(seq(1,4),AF0,AF1,AF2)
names(base8)<- c("k", "AF(0,k)", "AF(1,k)","AF(2,k)" )

knitr::kable(base8,align = 'c',digits = 2,
caption = " **Cuadro 9- Medidas de Pobreza Multidimensional de Alkire y Foster  
_Valores alternativos de k_**", row.names=FALSE)%>% 
kable_styling("striped", full_width = FALSE) %>%
row_spec(0, bold = T,color="white", background = "lightblue") 


#Contribución Porcentual de la dimensión.


z<-svyafcdec( ~ ITF + nohacina + nomatpreca + cloacas ,design= EPH_provincias_5_des ,group= ~REGION , k = 0.25 , g = 0, cutoffs = cut2 )

AF04<- z$`percentual contribution per dimension`

base10<-data.frame(c("ITF", "Sin Hacinamiento","Ausencia de Materiales Precarios","Cloacas"),AF04)


knitr::kable(base10,align = 'c',digits=4,
caption = " **Cuadro 10- Contribución Porcentual de la dimensión.  
_Medidas de Pobreza Multidimensional de Alkire y Foster_**", row.names=FALSE, col.names=c("Dimensión","Contribución %", "Desvío Estandar"))%>% 
kable_styling("striped", full_width = FALSE) %>%
row_spec(0, bold = T,color="white", background = "lightblue")
