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
library(devtools)
library(fpp3)
library(tseries) # Importamos la librería de tserires para la aplicación de la prueba de Dickey Fuller.
library(PerformanceAnalytics)
library(fBasics)
library(tsDyn)
library(urca)
library(vars)
library(MTS)
library(xts)
library(quantmod)
library(stats)
library(fBasics)
options(warn = - 1) 

# Manejo del directorio
getwd()
directorio <- "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/1. Datos"
setwd(directorio)

# Chequeo de los archivos del directorio
dir()
list.files()

## Importacion de los datos ------------------
install_formats() # Cuestiones de importacion de archivos del paquete rio
da <- import("7. Bogota_Promedio_Dias_Act_VECM.xlsx")

# Convertir la base de datos "da" a formato ts
da.ts <- ts(da[2:6], start = as.Date(2021), frequency = 365)
plot(da.ts)

# -----------------------------------------------------------
# Sección 2: Prueba de cointegración
# -----------------------------------------------------------

# Evaluará modelos VAR con hasta 7 retardos.
nivelka=VARselect(da.ts, lag.max = 8, type = "const")
nivelka$selection

# AIC(n)  HQ(n)  SC(n) FPE(n) 
#   7      4      3      7

niv1=VARorder(da.ts)

# [9,]  8 4.0412 4.8637 4.3504   26.2241  0.3957
# Al nivel 8 aumenta el pvalue mas del 5%
# Pero se repite más el 3

# -----------------------------------------------------------

# Aplicamos la prueba de Johansen para la identificación de relaciones lineales
# entre las series, lo que nos indica la condición de cointegración.
johatest=ca.jo(da.ts, type = "trace", K=8, ecdet ="none", spec = "longrun")
summary(johatest)

# r = 0  | 282.01 66.49 70.60 78.87
# Como 282.01 > ... , el rango de la martiz es 0 entonces si estan cointegradas,
# Si hay un equilibrio a largo plazo.
# Hay cointregracion de rango completo.

# -----------------------------------------------------------
# Sección 3: Construcción modelo VECM
# -----------------------------------------------------------

# lag=3 --> Orden autorregresivo
# r=2   --> Nivel de autointegración 

# El nivel de autointegración --> la cantidad de veces que se debe diferenciar 
# la serie para que se vuelva estacionaria.

vecm1 = VECM(da.ts, lag=7, r=4, estim = ("ML"))
summary(vecm1)

# Relacion A largo plazo 
# Error correction term 1
# El pm2.5 consigo mismo tiene un equilibrio en sus resagos.
# El pm2.5 con la tmp tiene un equilibrio en sus resagos.
# El pm2.5 con la radsolar NO tiene un equilibrio en sus resagos.
# El pm2.5 con la ws tiene un equilibrio en sus resagos.

# Error correction term 2
# El pm2.5 tiene una relación consigo mismo marginalmente.
# El pm2.5 tiene una relación con la tmp marginalmente.
# El pm2.5 tiene una relación con la radsolar marginalmente.
# El pm2.5 tiene una relación con la ws marginalmente.

# Intercepto
# Todas las variables participan en el equilibrio de largo plazo.

# El pm2.5 en su 1er rezago tiene correlación consigo mismo, con 1er rezago tmp y con 1er rezago pressure. 
# ...

# -----------------------------------------------------------
# Sección 4: Diagnostico
# -----------------------------------------------------------

# Para ello funcionan con VAR, hay que transformar, pasar de un obj VECM a un obj VAR:
varmod1 = vec2var(johatest, r=1)

# correlación:
ade1 = serial.test(varmod1, lags.pt = 5, type = "BG")
ade1   
# H0: datos independientes
# H1: datos dependientes.

# p-value = 2.109e-15 < 5%, se rechaza H0 y los datos dependientes.
# Existe una dependencia lineal entre los rezagos.

# Modelo de volatilidad autorregresivo condicionados - ARCH
hete1 = arch.test(varmod1, lags.multi = 15, multivariate.only = TRUE)
hete1  
#H0: NO existe efecto ARCH
# H1: SI existe efecto ARCH

# Se rechaza H0, SI existe efecto ARCH hasta el q ésimo rezago
# Hay heterocedasticidad, pues la varianza de los errores no es constante a lo largo del tiempo.
# Justificar por que:
# - Limitaciones del modelo, pues no abarca la heterocedasticidad.

# - Variabilidad natural de las variables: Las variables ambientales tienden a 
#   mostrar variaciones naturales que pueden ser más pronunciadas en ciertos 
#   periodos que en otros. Por ejemplo, la radiación solar y la temperatura 
#   pueden tener variaciones diarias y estacionales significativas, lo que 
#   afecta la dispersión y concentración de PM2.5. Esta variabilidad inherente 
#   puede llevar a cambios en la varianza de los errores del modelo a lo largo del tiempo.

# - Impactos de eventos no controlados: Eventos específicos como incendios 
#   forestales, variaciones climáticas extremas, o actividades humanas no 
#   regulares (como construcciones o tráfico intenso) pueden introducir cambios 
#   abruptos en las condiciones ambientales, lo que se refleja en una mayor 
#   volatilidad en los datos y, por ende, en la heterocedasticidad en el modelo.

# -----------------------------------------------------------
# Sección 5: Analisis Impulso - Respuesta: Impulse response
# -----------------------------------------------------------

m1irf = irf(varmod1, n.ahead = 30, boot = TRUE)
#predicción eje Y: es la var dependiente, acorde el impulso X.
plot(m1irf)
# Como interpretar???

## Prediction:
pred=predict(varmod1, n.ahead = 30, ci = 0.95)
plot(pred)
