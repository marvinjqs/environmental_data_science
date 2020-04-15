# --------------------------------------------
# PROCESAMIENTO DE DATOS DEL ANA
# Marvin J. Quispe Sedano
# Email: marvinjqs@gmail.com
# --------------------------------------------

library(tidyverse)
library(data.table)
library(openxlsx)

# IMPORTAR EL ARCHIVO DE EXCEL EN FORMATO CSV

setwd("C:/Users/Asus/Desktop/")

df <- read.csv("data-org.csv", 
               header = T, 
               sep = ";", 
               stringsAsFactors = F)


#CAMBIAR A FORMATO FECHA

fecha_horaria <- paste(df$FECHA, df$HORA, sep = " ")

df$FECHA <- as.POSIXct(fecha_horaria, format = "%d/%m/%Y %H:%M")

head(df$FECHA)


#CONTROL DE CALIDAD PARA VALORES HORARIOS Y DIARIOS

df %>% filter( HORA != "00:00")

df %>% filter( HORA != "06:00" & HORA != "10:00" & HORA != "14:00" & HORA != "18:00")


#OBTENER PROMEDIOS DIARIOS COMO MEDIDA PREVENTIVA
# data_prom_d <- aggregate(VALOR ~ FECHA, df, mean)

data_prom_h <- data.frame(df$FECHA,as.numeric(df$VALOR))

names(data_prom_h) <- c("FECHA", "VALOR")

str(data_prom_h)



#COMPLETAR FECHAS DE HORAS FALTANTES CON NA

data_prom_h <- data_prom_h %>%
  complete(FECHA = seq(min(FECHA,na.rm = T), max(FECHA,na.rm = T), by = "1 hour"),
           fill = list(VALOR = NA))



#OBTENER VALORES MENSUALES


data_prom_m <- setDT(data_prom_h)[,lapply(.SD, function(x) if(length(na.omit(x)) >=60)
  mean(x, na.rm=TRUE) else NA_real_) ,
  by = .(Month = format(FECHA, '%m-%Y'))]

data_max_m <- setDT(data_prom_h)[,lapply(.SD, function(x) if(length(na.omit(x)) >=60)
  max(x, na.rm=TRUE) else NA_real_) ,
  by = .(Month = format(FECHA, '%m-%Y'))]

data_min_m <- setDT(data_prom_h)[,lapply(.SD, function(x) if(length(na.omit(x)) >=60)
  min(x, na.rm=TRUE) else NA_real_) ,
  by = .(Month = format(FECHA, '%m-%Y'))]

# --------------------------------------------

#AÑADIR LAS CONSTATES DE ESTACION, OPERADOR, VARIABLE Y UNIDAD DE MEDIDA

est <- df[3,1]
ope <- df[3,2]
var <- df[3,3]
uni <- df[3,7]

#MARCO DE DATOS FINAL

data_final <- data.frame(est, ope, var, uni, data_prom_m, data_min_m[,2], data_max_m[,2])
names(data_final) <- c("ESTACION", "OPERADOR", "VARIABLE", "UNIDAD DE MEDIDA" ,"FECHA", "PROM", "MIN", "MAX")

#GUARDAR ARCHIVO FINAL

  write.xlsx(data_final, "WASSER-WORLD/ANA/DATA/nivelinsM-men-riopallanga.xlsx", row.names = F)
  





