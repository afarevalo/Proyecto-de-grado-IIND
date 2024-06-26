## Eje ARDL

library(ARDL)
#Otros de manipulación de datos
library(tidyverse) 
library(fields)  
library(forecast)  
library(quantmod) 
library(dygraphs) 
library(dplyr)    
library(ggplot2) 
library(tseries)   
library(tidyverse)   
library(urca)
library(TSstudio)
theme_set(theme_bw())
options(warn = - 1) 

###Función para obtener datos:
start<-format(as.Date("2014-01-01"),"%Y-%m-%d")
end<-format(as.Date("2021-06-30"),"%Y-%m-%d")
#funcion para obtener los precios
precios <-function(simbolo)
{
  ##Obtener precios stocks de Yahoo Finance
  datos <- getSymbols(simbolo, auto.assign = FALSE, from=start, to=end)
  ## Elimar faltantes:
  datos<-na.omit(datos)
  ##mantener columnas con precios maximo, minimo, de cierre y volumen de mercado:
  datos <- datos[,4]
  ##Para hacerlo datos accesibles en el global environment:
  assign(simbolo, datos, envir = .GlobalEnv)
}
##Llamar el activo de interés, pueden ser varios:
precios("SPY")
precios("AMZN")
precios("AAPL")
precios("META")

#Ya que tenemos que trabajar en formato ts
## Juntamos los datos, renombramos las columnas y las visualizamos:
prices<-merge.xts(`SPY`, `AMZN`, `AAPL`, `META`, join = "inner")
dim(prices)
colnames(prices)<-c("SPY", "AMZN", "AAPL", "META")

s1=`SPY`
s2=`AMZN`
s3=`AAPL`
s4=`META`


#Podemos identificar la longitud de cada uno
length(s1)
length(s2)
length(s3)
length(s4)

#Podemos  visualizar la serie de tiempo 

dygraph(prices, main = c("SPY", "AMZN", "AAPL", "META")) %>%
  dyAxis("y", label = "Prices") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(4, "Set1"))

pr_df=as.data.frame(prices)
head(prices)
#Se realiza una búsqueda tipo grid search:
#Selección automatica:
models <- auto_ardl(SPY ~ AMZN + AAPL + META, data = pr_df, max_order = 6)

#Revisemos el top 20 de los mejores modelos según su critrio de información de Akaike
models$top_orders

#Procedemos a construir el modelo de regresión con la mejor combinación.
mod1 <- ardl(SPY ~ AMZN + AAPL + META, data = pr_df, order = c(3,6,4,5))
summary(mod1)

# Para la interpretación, podemos imprimir los rezagos correspondientes de cada variable que explican la respuesta.
mod1$full_formula

##Predicción del primer modelo sin tendencia:
stats::predict(mod1$fitted.values, 10)

autoplot(predict(mod1$fitted.values, h=10))

## Bounds Test:
#Guardamos el mejor modelo obtenido en la sección anterior
modelo <- models$best_model
#Realizamos la prueba de hipotesis
bounds_f_test(modelo, case = 2) # el parametro "case" igual a 2 verifica si existe relaciones a largo termino, con la combinación de (restricted constant, no linear trend).

#Multiplicadores a corto plazo ##sr_ short run
multipliers(modelo, type = "sr")
#Si tuviera cointegración, el siguiente aplicaría:
bounds_f_test(modelo, case = 3)
multipliers(modelo, type = "lr")
a <- resid(modelo)
pacf(a, 30)
checkresiduals(modelo)
