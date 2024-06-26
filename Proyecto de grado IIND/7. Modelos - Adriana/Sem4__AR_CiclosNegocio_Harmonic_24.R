###################################
## AR Models. 
#-----Parte 1: Ciclos de negocio en AR
#-----Parte 2: Reg Arm?nica
setwd("C:/Users/al.abrego/Documents/R/EF_1") 
library(fUnitRoots)
library(forecast)
library(forecast)
library(TSstudio)
library(fpp3)
#########

#-----PARTE 1 : Ciclos de negocio-----------------------
#--Ejemplo clase1:
##rt=0.65rt-1-0.35rt-2+0.15rt-3+at
##(1-0.65L+0.35L^2-0.15L^3)rt=at
p0=c(1,-0.65,0.35)
r0=polyroot(p0)
r0
Mod(r0)  ##Calcula la solucisn real de las ramces, i=(-1)^1/2, suma del cuadrado de las raices.
k0=2*pi/acos(0.48/1.91) # Calcula el largo del periodo, hay que tomar el valor de las ramces conjugadas.
k0  #
#Ej2a
##(1-0.65L+0.35L^2-0.15L^3)rt=at
p0=c(1,-0.65,0.35, -0.15)
r0=polyroot(p0)
r0
Mod(r0)  ##Calcula la solucisn real de las ramces, i=(-1)^1/2, suma del cuadrado de las raices.
k0=2*pi/acos(0.48/1.91) # Calcula el largo del periodo, hay que tomar el valor de las ramces conjugadas.
k0  #

#--Ejemplo clase2:
##rt=0.65rt-1-0.35rt-2+at
##(1-0.65L+0.35L^2)rt=at
p2=c(1,-0.18,0.25, 0.023)
r2=polyroot(p2)
r2
Mod(r2)  ##Calcula la solucisn real de las ramces, i=(-1)^1/2, suma del cuadrado de las raices.
k=2*pi/acos(0.48/1.91) # Calcula el largo del periodo, hay que tomar el valor de las ramces conjugadas.
k  #igual que el otro mitodo, en este caso de ser AR(2)

#--Ejemplo 2 ciclo negocio:
da=read.table("gnp_1.txt",header=T)
tail(da)
G=da$VALUE
plot(G) #no muestra estacionareidad
LG=log(G)
adfTest(LG)
plot(LG) #muestra deriva
gnp=diff(LG) #log returns: diff de ln
adfTest(gnp)
library(fUnitRoots)
plot(gnp) #muestra ya estacionariedad
dim(da)

da.ts=ts(gnp,start=c(1947,1), freq=4)
plot(da.ts)
acf(gnp,lag=60)
pacf(gnp,lag=12) 

###Empiezo a revisar modelos a diferentes retrasos
library(fpp3)
library(forecast)
m1=arima(gnp,order=c(3,0,0), method = "ML")  #Mi hipstesis es que es AR(3)
m1
m1auto=auto.arima(gnp) ##Da otro modelo
m1auto
## retomando el primero:
tsdiag(m1,gof=12)  # model checking 
##Coeficientes polinomio:
p1=c(1,-m1$coef[1:3]) # polinomio caractermstico
p1
r1=polyroot(p1) # resuelve la  eq polinomio
r1  #Muestra las ramces y los pares conjugados.
Mod(r1)  ##Calcula la solucisn real de las ramces, i=(-1)^1/2, suma del cuadrado de las raices.
k=2*pi/acos(1.62/1.83) # Calcula el largo del periodo, hay que tomar el valor de las ramces conjugadas.
k
#######----------------------------------------------------------------------------------------------
##Eq caracteristica: α^3 -0.4α^2 + 0.1α + 0.3 = 0, equivalente a 
p_teor=c(0.3,0.1,-0.4,1)
r_teo=polyroot(p_teor)
r_teo
Mod(r_teo)
k=2*pi/acos(0.4597/0.759) # Calcula el largo del periodo, hay que tomar el valor de las ramces conjugadas.
k
#con polinomio
p_teor1=c(1,-0.4,0.1,0.3)
r_teo1=polyroot(p_teor1)
r_teo1
Mod(r_teo1)
k1=2*pi/acos(0.795944/1.315828)
k1
#
###Otros:
#--Ej  clase:
#1) rt=0.65rt-1-0.35rt-2+at
#1) (1-0.65L+0.35L^2)rt=at
#2) rt=0.25rt-1+0.34rt-2-0.17rt-3+at 




##### Parte 2: Regresi?n Arm?nica
#La regresi?n arm?nica es un m?todo donde se genera una descomposici?n de fourier a los ciclos o estacionalidades largas; es aplicable cuando estos ciclos se presentan continuamente en las series. M?s adelante veremos el an?lisis de estacionalidad mediante ARIMA, pero una alternativa de ello es este m?todo. 
#El m?todo consiste en descomponer la serie en t?rminos de seno() y coseno() que aproximen a la funci?n, semejando su patr?n peri?dico, donde si m es el periodo estacional, los primeros t?rminos de Fourier ser?n:
#  ![image.png](attachment:a2dd7187-981b-4c1b-875f-a171717e0ba2.png)

#As? sucesivamente. Si empleamos datos mensuales, podemos emplear hasta 11 t?rminos o K=11. Para datos trimestrales, como es este caso de ejemplo, se pueden hasta K=2, es decir K no debe ser mayor a la frecuencia m de la serie entre dos.
#Veamos el c?digo:
LG.ts<- ts(LG, start=c(1947,1), frequency = 4)
mod3=auto.arima(LG.ts, xreg=fourier(LG.ts, K=2), seasonal=FALSE, lambda=TRUE)
summary(mod3)
#En este caso particular, vemos que ninguno de lso t?rminos fourier son empleados, el primero para el seno (S1) y el segundo para el coseno C1. Esto indica que los ciclos de negocio decaen en el tiempo en esta serie de tiempo, es decir, no son ciclos permanentes en el periodo de estudio, 
#lo que se ve en la gr?fica de ACF.

#Podemos generar su gr?fico de predicci?n.
autoplot(forecast(mod3, xreg=fourier(LG.ts, K=5, h=4)), include=30)

