#################################
# ANALISIS ESPACIAL EN R
# Marvin J. Quispe Sedano
# Email: marvinjqs@gmail.com
#################################


#---------------------------------------------------------
# Para limpiar la consola:
# TeclaS :  Ctrl + L

# Para limpiar el workspace:
rm(list = ls())

###############
#  Paquetes   #
###############

#1) PAQUETES ESPACIALES
library(raster) #PARA GESTIONAR IMAGENES RASTER. TAMBIEN APORTA FUENTES A NIVEL DE PAIS O DE TERRITORIOS SOBRE DIVISION TERRITORIAL, CLIMA Y TOPOGRAFIA
library(sp) #ANALISIS ESPACIAL
library(maps) #PARA GENERAR MAPAS, SOBRE TODO DE ESTADOS UNIDOS, AUNQUE TAMBIEN MUNDIALES
library(rgdal) #PARA MANEJAR ARCHIVOS DE LA BIBLIOTECA GDAL (GEOSPATIAL DATA ABSTRACTION LIBRARY)
library(ggplot2) #CREACION DE GRAFICOS DE ALTA CALIDAD


#FIJAR EL DIRECTORIO DE TRABAJO
setwd("C:/Users/marvi/OneDrive/Documents/R-proyectos/GEOESTADISTICA") 

#2) CREAR/CARGAR OBJETOS ESPACIALES EN R


##2.2) DENTRO DE R
###DEL PAQUETE MAPS
dev.new()
dev.new()
map()	#MAPAMUNDI DE BAJA RESOLUCION
dev.new()
map('worldHires','Dominican Republic',col='black',fill=T)
map('worldHires',c('Peru', 'Lima'),col='black',fill=T)
box() #ANADE BORDE AL MAPA ACTUAL

###DEL PAQUETE RASTER
####DATOS ASOCIADOS AL PAQUETE
dop<-getData('GADM', country='PER', level=1)
dom<-getData('GADM', country='PER', level=2)
#getData('SRTM',lat=19,lon=-71) #DEM POR GRANDES TERRITORIOS. TIENE UN INCONVENIENTE: OBLIGA A DESCARGAR ARCHIVOS MUY GRANDES. NO ESTAN POR PAIS, POR TILES. RD SE CONSTRUYE CONCATENANDO DOS TILES. ARCHIVOS GRANDES
dev.new()
sp::spplot(dop)
dev.new()
sp::spplot(dom)
#VISUALIZANDO LOS DATOS Y CREANDO IDENTIFICADORES
head(dop)
summary(dop)
dop@data[["ID"]] <- c(1:26)  #AÑADIENDO UN IDENTIFICADOR NUMÉRICO
head(dop)
dev.new()
sp::spplot(dop['ID'],col.regions = rainbow(26, start = 0, end = 1))
class(dop@data[["NAME_1"]])
dop$NAME_1=factor(dop$NAME_1)
dev.new()
sp::spplot(dop['NAME_1'],col.regions = rainbow(26, start = 0, end = 1))
dev.new()
sp::spplot(dop['NAME_1'],col.regions = sample(rainbow(32),32))
dev.new()
summary(dom)
dom@data[["ID"]] <- c(1:195)
sp::spplot(dom['ID'])

####DATOS DESCARGADOS VIA http Y TRABAJADOS EN LOCAL
setwd(tempdir()) #MUESTRA DONDE ESTA EL DIRECTORIO DE TRABAJO
temp <- tempfile() #ASIGNAR EL NOMBRE DE UN ARCHIVO TEMPORAL AL OBJETO temp
download.file("http://geografiafisica.org/r/maestria_geom/practica_01.zip",temp) #DESCARGA EL ARCHIVO ZIP Y LO NOMBRA COMO TEMPORAL
unzip(temp) #DESCOMPRIME EN EL DIRECTORIO DE TRABAJO
unlink(temp) #DESVINCULA DEL ARCHIVO TEMPORAL Y LO BORRA
dem<-raster(readGDAL("demesp.tif")) #LEE EL TIF DEL DEM DE LA ESPANOLA CON LA FUNCION readGDAL DEL PAQUETE rgdal, EL CUAL SE ENCONTRABA EN EL COMPRIMIDO ZIP, Y LO ASIGNA AL OBJETO dem
dem #MUESTRA LAS CARACTERISTICAS DEL raster DENOMINADO dem
dev.new() #CREA UNA VENTANA GRAFICA NUEVA
plot(dem) #VISUALIZA EL RASTER dem EN UN MAPA  # CAMBIAR NOTACION CIENTIFICA : options(scipen=999) 
raster::zoom(dem) #PARA HACER ZOOM INTERACTIVO MEDIANTE DOS PUNTOS DE UN RECTANGULO IMAGINARIO. HACER CLIC PRIMERO EN UNO DE LOS PUNTOS DEL MAPA Y LUEGO HACER CLIC EN EL SEGUNDO; EL AREA CUBIERTA POR EL RECTANGULO IMAGINARIO SERA LA MOSTRADA EN EL PLOT. COMENTADA PARA EVITAR DETENCION DE SCRIPT
sneyba<-readOGR(dsn='sneyba.kml') #LEE UN ARCHIVO KML CON LIMITES ARBITRARIOS DE LA SIERRA DE NEYBA Y LO ASIGNA AL OBJETO sneyba, EL CUAL SE ENCONTRABA EN EL COMPRIMIDO ZIP
sneyba #MUESTRA LAS CARACTERISTICAS DEL OBJETO ESPACIAL DENOMINADO sneyba
sneyba<-spTransform(sneyba,CRS("+init=epsg:32619")) #TRANSFORMA EL OBJETO ESPACIAL sp DEL SISTEMA DE REFERENCIA ESPACIAL LATLONG/WGS84 AL UTM/WGS84
dev.new()
plot(dem) #VISUALIZA EL RASTER dem EN UN MAPA
plot(sneyba,add=T) #VISUALIZAR EL OBJETO ESPACIAL sneyba, ANADIENDOLO AL MAPA ANTERIOR
rsneyba<-raster::mask(crop(dem,extent(sneyba)),sneyba)  #APLICA UN EXTRACCION POR MASCARA
dev.new()
plot(rsneyba)
valsneyba <- getValues(rsneyba)
head(valsneyba) #RECUPERA LOS PRIMEROS 6 VALORES
mean(valsneyba, na.rm=T)
dev.new()
hist(valsneyba) #HISTOGRAMA DE LOS VALORES DE ALTURA
dev.new()
hist(rsneyba[valsneyba>100]) #HISTOGRAMA DE LOS VALORES DE ALTURA DE MAS DE 100 METROS



#2) CREACION DE MAPAS TEMATICOS EN R


#CARGAR SHP DE PASIVOS MINEROS
setwd('C:/Users/marvi/OneDrive/Escritorio/Nueva carpeta/')
pas_min <- readOGR("Pasivos-Mineros.shp")
plot(pas_min)

#CONVERTIR SHP A DATAFRAME
area.points <- data.frame(pas_min)
head(area.points)

#CREACIÓN DEL MAPA TEMÁTICO

Mapa <- ggplot() + borders("world", colour="black", fill= "Beige") + 
  theme_bw() + xlim(-85,-60) + ylim(-20,2) + 
  xlab("Longitud") + ylab("Latitud") +
  geom_point(aes(x=coords.x1,y=coords.x2, shape = factor(TIPO_COMPO),
                 colour = factor(TIPO_COMPO)), data = area.points)

Mapa




################################
#############################








