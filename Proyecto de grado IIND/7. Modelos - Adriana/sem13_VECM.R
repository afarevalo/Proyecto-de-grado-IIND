##VECM
#intall.packages("tsDyn")
library(tsDyn)
library(vars)
library(urca)
library(forecast)
library(tidyverse)
library(fBasics)
library(PerformanceAnalytics)
library(xts)
library(quantmod)
library(ggplot2)
library(tseries)
library(dygraphs)
library(dplyr)
library(stats)
options(warn = - 1) 
setwd("C:/Users/al.abrego/OneDrive - Universidad de los Andes/Documentos/R/EF_1")

#---------- Datos:
# Serie rendimientos semanales
#---------Parte 1
#--------Obtención Datos:
start<-format(as.Date("2019-05-01"),"%Y-%m-%d")
end<-format(as.Date("2021-07-01"),"%Y-%m-%d")

#--------- Función para bajar precios y generar rendimientos:
pc<-function(simbolo) {
  ##---------Obtener precios de yahoo finance:
  datos<-getSymbols(simbolo,  from=start, to= end, auto.assign = FALSE)
  ##---------eliminar datos faltantes:
  datos<-na.omit(datos)
  ##--------Mantener el precio de interés:
  datos<-datos[,4]
  ##--------Rendimientos simples:
  #rend<-periodReturn(datos, period = "daily", type='arithmetic')                         
  #------ --Para hacer dtos accesibles  GLobal ENv:
  assign(simbolo, datos, envir = .GlobalEnv)
}

#------- Llamar la función para cada activo particular:
pc("VGLT")
pc("JPY=X")
pc("RYLD")

#
in1=`VGLT`
in2= `JPY=X`
in3=RYLD

prices<-merge.xts(in1, in2, in3)
dim(prices)
colnames(prices)<-c("in1",  "in2", "in3")
dygraph(prices, main = "in1",  "in2", "in3") %>%
  dyAxis("y", label = "Prices") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(4, "Set1"))


# ------- Nivel Autorregresivo:
datos=merge.xts(in1, in2,in3, join='inner')%>% na.omit()
nivelk=VARselect(datos, lag.max = 7, type = "const")
nivelk$selection
# O con MTS:
library(MTS)
niv1=VARorder(datos)
##
johatest=ca.jo(datos, type = "trace", K=3, ecdet ="none", spec = "longrun") #K is the number of lags to use in the vector autoregressive model and is set this to the minimum, K=2.
summary(johatest)  #significativo al 5%, hay una relación de cointegración

#--------- Construcción modelo VECM

vecm1 = VECM(datos, lag=3, r=2, estim = ("ML"))
summary(vecm1)

# Diagnóstico: Las pruebas para ello funcionan con VAR, hay que transformar:
varmod1 = vec2var(johatest, r=1)

# correlación:
ade1 = serial.test(varmod1, lags.pt = 5, type = "BG")
ade1   # H0: datos independientes, H1: datos dependientes. 

hete1 = arch.test(varmod1, lags.multi = 15, multivariate.only = TRUE)
hete1  #efecto arch H0: no efecto arch, H1: hay efecto arch


# Análisis Impulso - Respuesta: Impulse response
library(vars)

m1irf = irf(varmod1, n.ahead = 30, boot = TRUE)
plot(m1irf)  #predicción eje Y: es la var dependiente, acorde el impulso X.


# Con OIR (orthogonal- Imp-Resp) Se descompone la matriz de vari-cov a una matriz triangular inferior con elementos positivos diagonales
suma = summary(vecm1)
# suma
choles = t(chol(suma$sigma))
choles  # Ej. un choque en JPY genera un efecto contemporáneo en Rusell, pero no viceversa.
# o, un choque en el rendimiento de los bonos del tesoro genera un efecto contemporáneo en divisa JPY y en Rusell
# Para ver los otros efectos, cambiar el orden de los datos.

#Similar con VAR:
library(vars)
modVAR= VAR(datos, p = 2, type="const")
model_sum=summary(modVAR)
chole = t(chol(model_sum$covres))
chole

## Prediction:
pred=predict(varmod1, n.ahead = 30, ci = 0.95)
plot(pred)









