#################################
# Analisis de la concentración de CO2
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
install.packages("ggplot2")


#EJERCICIO 1
file_url <-  'ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_mm_mlo.txt'
est_ml <-  read.table(file_url, header = F, stringsAsFactors = F)
head(est_ml)


#EJERCICIO 2
co2_data <- est_ml[, c(1:5)]
head(co2_data)


#EJERCICIO 3
colnames(co2_data) <- c("año","mes", "año_dec", "co2_ppm", "co2_ppm_int")
head(co2_data)


#EJERCICIO 4
co2_data[co2_data$co2_ppm == -99.99,]

co2_data[co2_data$co2_ppm == -99.99, 4] <- NA 
#OTRA FORMA: co2_data[co2_data$co2_ppm == -99.99, ]$co2_ppm <- NA

head(co2_data)


#EJERCICIO 5
str(co2_data)


#EJERCICIO 6
co2_data$date <- as.Date(paste(co2_data$año, 
                               co2_data$mes,
                               '01' ,
                               sep = '-'), format = '%Y-%m-%d')
str(co2_data)


#EJERCICIO 7
summary(co2_data)

co2_data[co2_data$co2_ppm == min(co2_data$co2_ppm, na.rm = T),]
co2_data[co2_data$co2_ppm == max(co2_data$co2_ppm, na.rm = T),]


#EJERCICIO 8

plot(
  co2_data$date,
  co2_data$co2_ppm_int,
  type = 'l',
  xlab = 'Fecha',
  ylab = 'Concentración de CO2 (ppm)',
  main = 'Concentración mensual de CO2 - Mauna Loa'
)

### Linea de tendencia:

reglineal <- lm(co2_data$co2_ppm_int ~ co2_data$date)
summary(reglineal)


plot(
  co2_data$date,
  co2_data$co2_ppm_int,
  type = 'l',
  xlab = 'Fecha',
  ylab = 'Concentración de CO2 (ppm)',
  main = 'Concentración mensual de CO2 - Mauna Loa'
)
abline(reglineal, col = 'blue')


## UNA MEJOR MANERA DE GRAFICAR ES CON EL PAQUETE 'ggplot2'

library(ggplot2)

ggplot(data = co2_data, aes(date, co2_ppm_int)) +
  geom_line() +
  xlab('Fecha') +
  ylab('Concentración de CO2 (ppm)') + 
  ggtitle('Concentración mensual de CO2 - Mauna Loa') +
  stat_smooth(method = lm, color = 'blue')

## Para el año 2018 : 


