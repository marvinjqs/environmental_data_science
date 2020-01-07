# ANÁLISIS Y PROCESAMIENTO DE LOS DATOS DEL SENSOR MQ7
# ----------------------------------------------------
library(openair)
library(dplyr)
library(lubridate)

# LECTURA DE LOS DATOS DEL MONITOREO ----------

setwd("C:/Users/marvi/Desktop/atmosferica/")

data_mq7 <- read.csv("DATA_MQ_FINAL.csv", header = T, 
                     sep = ";", stringsAsFactors = F, encoding = "UTF-8")

names(data_mq7) = c("TEMPERATURA_C", "HUMEDAD_RELATIVA_%", 
                    "PPM_CO_SENSOR1", "PPM_CO_SENSOR2")

# CONVERSIÓN DE UNIDADES DE PPA A ug/m3 --------

K <- (0.082 * (data_mq7$TEMPERATURA_C + 273.15)) / 0.82

data_mq7$ug_m3_CO_SENSOR1 <- (data_mq7$PPM_CO_SENSOR1 * 28.01 * 1000) / K

data_mq7$ug_m3_CO_SENSOR2 <- (data_mq7$PPM_CO_SENSOR2 * 28.01 * 1000) / K

# PROCESAMIENTO DE DATOS -----------

date <- seq(ymd_hm('2019-11-14 20:00'), by = '150 secs', length.out = nrow(data_mq7))

data_mq7 <- cbind(date,data_mq7)

str(data_mq7)

plot.new()

timePlot(data_mq7, pollutant = c("PPM_CO_SENSOR1", "PPM_CO_SENSOR2"),
         name.pol = c("SENSOR 1", "SENSOR 2"), smooth = T,
         xlab = " ", ylab = "ppm CO", cols = c("red","blue"),
         main = "CONCENTRACIÓN DE CO EN INTERIORES - 2019/11/14",
         avg.time = "1 hour", ref.y = list(h = 35, lty = 5))


timePlot(data_mq7, pollutant = c("ug_m3_CO_SENSOR1", "ug_m3_CO_SENSOR2"),
         name.pol = c("SENSOR 1", "SENSOR 2"), smooth = T,
         xlab = " ", ylab = "ug/m3 CO", cols = c("red","blue"),
         main = "CONCENTRACIÓN DE CO EN INTERIORES - 2019/11/14",
         avg.time = "1 hour") 



pairs(data_mq7,
      lower.panel = panel.smooth,
      upper.panel = NULL,
      col = "skyblue3")

plot.new()
scatterPlot(data_mq7, x = "TEMPERATURA_C", y = "PPM_CO_SENSOR2",
            method = "density", col = "jet")


timePlot(data_mq7, pollutant = c("ug_m3_CO_SENSOR2"),
         name.pol = c("SENSOR 2"), smooth = T, 
         xlab = " ", ylab = "ug/m3 CO", cols = c("blue"),
         main = "CONCENTRACIÓN DE CO EN INTERIORES - 2019/11/14",
         avg.time = "1 hour", ref.y = list(h = 35000, lty = 5)) 









