#cargar las librerías

library(tidyverse)
library(highcharter)
library(viridisLite)


# modificar algunas opciones de configuración

hcopts <- getOption("highcharter.lang")


hcopts$drillUpText<- "Volver"
hcopts$downloadCSV <- "Descargar CSV"
hcopts$downloadJPEG <- "Descargar imágen JPEG"
hcopts$downloadPDF <- "Descargar documento PDF"
hcopts$downloadPNG  <- "Descargar imágen PNG"
hcopts$downloadSVG  <- "Descargar imágen SVG"
hcopts$downloadXLS  <- "Descargar XLS"
hcopts$loading  <- "Procesando..."
hcopts$months<-c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
hcopts$noData <- "No existen datos para mostrar"
hcopts$numericSymbols <-c("mil", "M", "G", "T", "P", "E") 
hcopts$printChart <- "Imprimir Gráfico"
hcopts$shortMonths<-c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
hcopts$resetZoom <- "Restablecer zoom"
hcopts$resetZoomTitle <- "Restablecer nivel del zoom 1:1"
hcopts$thousandsSep <- ","
hcopts$viewData <- "Mostrar tabla de datos"


options(highcharter.lang = hcopts)


# colores a utilizar

cols3 <- plasma(3,alpha=0.8,direction=-1)


# el gráfico


bc <- data_frame(
  name = c("Exportaciones", "Importaciones", "Saldo Balanza Comercial"),
  y = c(42176, 34468,7708 ),
  drilldown = tolower(name)
)

ds <- list_parse(bc)
names(ds) <- NULL


hc <- highchart() %>%
  hc_chart(type = "column") %>%
  hc_xAxis(type = "category") %>%
  hc_legend(enabled = FALSE) %>%
  hc_plotOptions(
    series = list(
      boderWidth = 0,
      dataLabels = list(enabled = TRUE)
    )
  ) %>%
  hc_add_series(
    name = "Balanza Comercial, exportaciones e importaciones",
    colorByPoint = TRUE,
    data = ds
  )

impo <- data_frame(
  name = c("Total","Bienes de capital", "Bienes intermedios", "Combustibles y lubricantes", "Piezas y accesorios para bienes de capital","Bienes de consumo","Vehículos automotores de pasajeros","Resto"),
  y = c(34468,5839,11995,3360,7057,4249,1786,182),
  drilldown = tolower(name)
)

expo <- data_frame(
  name = c("Total","Productos primarios", "Manufacturas de origen agropecuario (MOA)", "Manufacturas de origen industrial (MOI)", "Combustibles y energía"),
  y = c(42176, 11379,15692,12179,2926),
  drilldown = tolower(name)
)


pri<-data_frame( 
 name = c(
  "Animales vivos",
  "Pescados y mariscos sin elaborar",
  "Miel",
  "Hortalizas y legumbres sin elaborar",
  "Frutas frescas",
  "Cereales",
  "Semillas y frutos oleaginosos",
  "Tabaco sin elaborar",
  "Lanas sucias",
  "Fibras de algodón",
  "Minerales metalíferos, escorias y cenizas",
  "Resto de productos primarios"),
y = c(25,1026,102,371,561,6662,2033,159,45,78,233,85)
)

moa<-data_frame(
name = c("Carnes y sus preparados",
  "Pescados y mariscos elaborados",
  "Productos lácteos",
  "Otros productos de origen animal",
  "Frutas secas o procesadas",
  "Café, té, yerba mate y especias",
  "Productos de molinería y sus preparaciones",
  "Grasas y aceites",
  "Azúcar, cacao y artículos de confitería",
  "Preparados de hortalizas, legumbres y frutas",
  "Bebidas, líquidos alcohólicos y vinagre",
  "Residuos y desperdicios de la industria alimenticia",
  "Extractos curtientes y tintóreos",
  "Pieles y cueros",
  "Lanas elaboradas",
  "Resto de MOA"),
y=c(2264,197,403,48,81,122,508,3278,163,714,572,6386,164,404,123,265))

moi<-data_frame(
name= c(  "Productos químicos y conexos",
  "Materias plásticas y sus manufacturas",
  "Caucho y sus manufacturas",
  "Manufacturas de cuero, marroquinería, etc.",
  "Papel, cartón, impresos y publicaciones",
  "Textiles y confecciones",
  "Calzado y sus partes componentes",
  "Manufacturas de piedra, yeso, vidrio, etc.",
  "Piedras, metales preciosos y sus manufacturas, monedas",
  "Metales comunes y sus manufacturas",
  "Máquinas y aparatos, material eléctrico",
  "Material de transporte terrestre",
  "Vehículos de navegación aérea, marítima y fluvial",
  "Resto de MOI"),
y= c(2459,572,189,18,323,95,7,100,1643,1300,912,4183,170,207)
)

cye<-data_frame(
name= c("Petróleo crudo",
  "Carburantes",
  "Grasas y aceites lubricantes",
  "Gas de petróleo, otros hidrocarburos gaseosos  y energía eléctrica",
  "Resto de combustibles"),
y= c(982,1348,34,496,67))


bk<- data_frame(
name= c("Máquinas, aparatos y material eléctrico; sus partes", 
"Material de transporte",
"Instrumentos de óptica, precisión, médico-quirúrgico, relojería y música",
"Resto de bienes de capital"),
y=c(4261,742,650,187))

bi<-data_frame(
name=c("Productos del reino vegetal", 
"Productos minerales",
"Productos de industrias químicas y conexas",
"Plástico, caucho y sus manufacturas",
"Pasta de madera, papel,cartón",
"Materias textiles y sus manufacturas",
"Manufacturas de piedra, yeso, cemento, amianto, mica, cerámica y vidrio",
"Metales comunes y sus manufacturas",
"Resto de bienes intermedios"),
y=c(1633,552,4440,1492,591,450,277,1619,942))


cyl<- data_frame(
name=c("Productos minerales",
"Resto de combustibles y lubricantes"),
y=c(3105,255))


pya<- data_frame(
name=c("Máquinas, aparatos y material eléctrico; sus partes",
"Material de transporte",
"Resto de piezas y accesorios"),
y=c(5040,1532,485))

bco<-data_frame(
name=c("Animales vivos y productos del reino animal", 
"Productos del reino vegetal", 
"Productos alimenticios, bebidas y tabaco", 
"Productos de industrias químicas y conexas", 
"Plástico, caucho y sus manufacturas", 
"Materias textiles y sus manufacturas",  
"Calzado, paraguas, fores artifciales y otros", 
"Máquinas, aparatos y material eléctrico; sus partes", 
"Material de transporte", 
"Instrumentos de óptica, precisión, médico-quirúrgico, relojería y música", 
"Mercancías y productos diversos",
"Resto de bienes de consumo"),
y=c(144,226,384,1545,194,314,294,261,165,141,261,322))





dimpo<-list_parse(impo)
dexpo<-list_parse(expo)
dpri<-list_parse(pri)
dmoa<-list_parse(moa)
dmoi<-list_parse(moi)
dcye<-list_parse(cye)
dbk<-list_parse(bk)
dbi<-list_parse(bi)
dcyl<-list_parse(cyl)
dpya<-list_parse(pya)
dbco<-list_parse(bco)





hc %>%
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series = list(
      list(
        id = "exportaciones",
        data = dexpo
      ),
      list(
        id = "importaciones",
        data = dimpo
      ),
      list(
        id = "productos primarios",
        data = dpri
      ) ,
      list(
        id = "manufacturas de origen agropecuario (moa)",
        data = dmoa
      ) ,
      list(
        id = "manufacturas de origen industrial (moi)",
        data = dmoi
      ) ,
      list(
        id = "combustibles y energía",
        data = dcye
      ) ,
      list(
        id = "bienes de capital",
        data = dbk
      ),
      list(
        id = "bienes intermedios",
        data = dbi
      ),
      list(
        id = "combustibles y lubricantes",
        data = dcyl
      ),
      list(
        id = "piezas y accesorios para bienes de capital",
        data = dpya
      ),
      list(
        id = "bienes de consumo",
        data = dbco
      )       
)
  )%>%
hc_tooltip(pointFormat =  "{point.y} millones de dólares") %>%
  hc_colors(cols3)%>%
hc_title(text="Exportaciones por grandes rubros, importaciones por usos y agregados. Acumulado Enero-Agosto 2019",style = list( fontSize = '12px')) %>%
hc_subtitle(text="INDEC - Datos Estimados. Millones de dólares. Cliquear para desplegar.",style = list(color = "#2b908f", fontSize = '10px')) %>%
hc_exporting(enabled = TRUE)