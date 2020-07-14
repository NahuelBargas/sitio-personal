# Datos del Censo Nacional Agropecuario Argentino + Biscale


## Cargar librerías

library(sf)
library(ggplot2)
library(biscale)
library(patchwork)
library(cowplot)
library(openxlsx)
library(extrafont)
library(ggtext)


### Cargar base con datos seleccionados del siguiente archivo:
### http://www.indec.gob.ar/ftp/cuadros/economia/CNA2018_resultados_preliminares.xls

censo<-read.xlsx("https://nahuelbargas.github.io/sitio-personal/biscale/datosagroprov.xlsx")

### Descargar el shapefile del país desde http://www.indec.gov.ar/ftp/cuadros/territorio/codgeo/Codgeo_Pais_x_dpto_con_datos.zip

unzip("Codgeo_Pais_x_dpto_con_datos.zip", exdir=getwd())

argentina<-st_read("pxdptodatosok.shp")


p1<-ggplot(argentina) +
  geom_sf(colour="skyblue")+ theme_void()  # mapa del país completo

## A continuación se quitan las áreas de la Capital Federal, Antártida e Islas del Atlántico Sur

argentina2<-argentina[!(argentina$provincia=="Ciudad Autónoma de Buenos Aires"),]

argentina2<- argentina2[-425,] #Acá quitas la Antártida

argentina2<- argentina2[-511,] #Acá quitas las islas

## Se unen las bases 

argentinacenso<- merge(argentina2,censo, by= "provincia",all.x=TRUE,all.y=TRUE)


## Aplciamos Biscale con la opción quantile
argentinabi<-bi_class(argentinacenso, x =EAP, y =Inseguridad , style = "quantile", dim = 3)

## Mapa sin leyenda, con recuadro de ggtext

mapa <- ggplot() +
  geom_sf(data = argentinabi, aes(fill = bi_class), color = "gray80", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  theme_void()+
  labs(title ="<span style ='color:#0D0887FF; font-family:Cambria;'>Explotaciones agrícolas permanentes</span> y <span style ='color:#CC4678FF;font-family:Constantia;'> reportes de inseguridad</span>.",
       caption="En base a los datos preliminares del Censo Nacional Agropecuario Argentino-2018-INDEC." )+
 theme( plot.caption=element_text(family="Book Antiqua",,hjust=0.5))+
theme(
plot.title = element_textbox_simple(
      size = 13, lineheight = 1,margin=margin(b = 0, t = 4, l =-4 ,r=-4, unit = "pt"),padding=margin(5.5,5.5,5.5,5.5),
      width = grid::unit(13, "cm"),
      r = grid::unit(3, "pt"),
      linetype = 1, # turn on border
      box.color = "#748696", # border color
      fill = "#F0F7FF", # background fill color
    ) )


##Leyenda donde se selecciona el tamaño.

legenda<-  bi_legend(pal = "DkBlue",
                    dim = 3,
                    xlab = "Mayor N° de EAP ",
                    ylab = "Mayor N° de delitos",
                    size = 6.5)

## Generamos el gráfico final


png("Gráfico biscale.png", width=600, height=400,res = 100)

ggdraw() +
  draw_plot(mapa2, 0, 0, 1, 1) +
  draw_plot(legenda, 0.05, 0.15, 0.3, 0.3)+
  draw_plot(p1, 0.6, 0.10, 0.25, 0.25)

dev.off()

