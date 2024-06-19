# -----------------------------------------------------------
# Sección 1: Preparación de datos
# -----------------------------------------------------------

# Limpiar el entorno
rm(list = ls())

## Librerias ------------------
library(pacman)
library(tidyverse) # Paquete grande de manipulacion
library(lubridate) # Paquete para manejo de fechas
library(skimr)     # Paquete para revision de datos
library(stargazer) # Paquete de tablas "bonitas", regs y estad desc
library(dplyr)     # Paquete parte de tidyverse donde esta mutate, select, filter, summarise...
library(rio)       # Paquete de importacion/exportacion de datos
library(gridExtra)
library(patchwork)
library(stats)
library(readxl)
library(writexl)
library(ggplot2)
library(officer)
library(flextable)
library(pscl)
library(corrplot)   # Para el gráfico de correlación
library(forecast)
library(tseries)

library(devtools)
library(fpp3)
library(urca)
library(vars)
library(MTS)
library(tseries) # Importamos la librería de tserires para la aplicación de la prueba de Dickey Fuller.

# Manejo del directorio
getwd()
directorio <- "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/1. Datos"
setwd(directorio)

# Chequeo de los archivos del directorio
dir()
list.files()

## Importacion de los datos ------------------
install_formats() # Cuestiones de importacion de archivos del paquete rio
data <- import("6. Bogota_Promedio_Dias_Act_VAR_2.xlsx")

# -----------------------------------------------------------
# Sección 1: Preparación de datos
# -----------------------------------------------------------

# Realizar diferenciacion, pues es ARIMA 1 integrado 
modelo1 <- auto.arima(data$pm25)
modelo1 # ARIMA(3,1,1)

modelo2 <- auto.arima(data$pm10)
modelo2 # ARIMA(3,1,1) 

modelo3 <- auto.arima(data$no2)
modelo3 # ARIMA(1,0,3) with non-zero mean 

# Diferenciación logarítmica
log_PM_25 <- diff(log(data$pm25), differences = 1)
log_PM_10 <- diff(log(data$pm10), differences = 1)

adf.test(log_PM_25)
adf.test(log_PM_10)
adf.test(data$no2)

library(fUnitRoots)
plot(log_PM_25)

da.ts=ts(data[2:4],start=c(2021,1), freq=1)
plot(da.ts)
acf(log_PM_25,lag=60)
acf(log_PM_10,lag=60)
acf(data$no2,lag=60)


pacf(log_PM_25,lag=12)
pacf(log_PM_10,lag=12)
pacf(data$no2,lag=12)

###Empiezo a revisar modelos a diferentes retrasos
library(fpp3)
library(forecast)
m1=arima(data$no2,order=c(3,0,0), method = "ML")  #Mi hipstesis es que es AR(3)
m1
## retomando el primero:
tsdiag(m1,gof=12)  # model checking 
# Mal modelo? Hay autocorrelacion

##Coeficientes polinomio:
p1=c(1,-m1$coef[1:3]) # polinomio caractermstico
p1
r1=polyroot(p1) # resuelve la  eq polinomio
r1  #Muestra las ramces y los pares conjugados.
Mod(r1)  ##Calcula la solucisn real de las ramces, i=(-1)^1/2, suma del cuadrado de las raices.
k=2*pi/acos(1.277868/3.170682) # Calcula el largo del periodo, hay que tomar el valor de las ramces conjugadas.
k

# -----------------------------------------------------------
# Sección NO2: ARMA(1,3)
# -----------------------------------------------------------

###Empiezo a revisar modelos a diferentes retrasos
library(fpp3)
library(forecast)
m1=arima(data$no2,order=c(1,0,3), method = "ML")  #Mi hipstesis es que es AR(3)
m1
## retomando el primero:
tsdiag(m1,gof=12)  # model checking 
# Mal modelo? Hay autocorrelacion

##Coeficientes polinomio:
p1=c(1,-m1$coef[1:3]) # polinomio caractermstico
p1
r1=polyroot(p1) # resuelve la  eq polinomio
r1  #Muestra las ramces y los pares conjugados.
Mod(r1)  ##Calcula la solucisn real de las ramces, i=(-1)^1/2, suma del cuadrado de las raices.
k=2*pi/acos(0.78/1.1) # Calcula el largo del periodo, hay que tomar el valor de las ramces conjugadas.
k






