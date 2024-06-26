#MMAD-I MIIA. 
#AR Models.
# Identificación Estacionariedad y ciclos
#####################  PRUEBA RAIZ UNITARIA  ####################
library(fUnitRoots)
library(forecast)
#Ejemplo 1 clase: 
##(1-0.3L-0.4L^2)xt=at
p=c(1,-0.3,-0.4)
r=polyroot(p)
r
##Con ec caracteristica
##(-0.4, -0.3, 1) =0
p1=c(-0.4,-0.3,1)
r1=polyroot(p1)
r1
#Ejemplo2 clase: Phi1=0.2; Phi2=-0.4; [xt= 0.2xt-1 - 0.4xt-2 + at] 
##(1-0.2L+0.4L^2)xt=wt
p2=c(1,-0.2, 0.4)
r2=polyroot(p2)
r2
Mod(r2)  ##Calcula la solucion real de las raices, i=(-1)^1/2, suma del cuadrado de las raices.
k0=2*pi/acos(0.25/1.581139) # Calcula el largo del periodo, hay que tomar el valor de las ra?ces conjugadas.
k0  #igual que el otro m?todo, en este caso de ser AR(2)

#Solucion con ecuacion caract
p_teor1=c(0.4,-0.2, 1)
r_teo1=polyroot(p_teor1)
r_teo1
Mod(r_teo1)
k2=2*pi/acos(0.1/0.6324555)
k2 # #Mimso resultado
#######----------------------------------------------------------------------------------------------
##Slide 44:
#(1):Eq caracteristica: α^3 -0.4α^2 + 0.1α + 0.3 = 0, equivalente a 
p_teor=c(0.3,0.1,-0.4,1)
r_teo=polyroot(p_teor)
r_teo
Mod(r_teo)
k=2*pi/acos(/) # (Completar) Calcula el largo del periodo, hay que tomar el valor de las ramces conjugadas.
k  #6.82
#con polinomio
p_teor1=c(1,-0.4,0.1,0.3)
r_teo1=polyroot(p_teor1)
r_teo1
Mod(r_teo1)
k1=2*pi/acos(/)  #completar
k1  #6.82
#
###Otros:
#--Ej  clase:
#1) 𝑟_𝑡=0.5𝑟_(𝑡−1)+0.4𝑟_(𝑡−2)+𝑎_𝑡	
#1) 𝑟_𝑡=0.4𝑟_(𝑡−1)−0.25𝑟_(𝑡−2)+𝑎_𝑡

###################################################--------------------------
library(fUnitRoots)
#--Ejemplo con serie tiempo/ ciclo negocio:
da=read.table("gnp_1.txt",header=T)
tail(da)
G=da$VALUE
plot(G, type="l") #no muestra estacionareidad
LG=log(G) 
adfTest(LG)
plot(LG, type="l") #muestra deriva
gnp=diff(LG) #log returns: diff de ln
adfTest(gnp)
library(fUnitRoots)
plot(gnp, type="l") #muestra ya estacionariedad
dim(da)

da.ts=ts(gnp,start=c(1947,1), freq=4)
plot(da.ts)
##Funciones ACf
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
r1  #Muestra las raices y los pares conjugados.
Mod(r1)  ##Calcula la solucisn real de las ramces, i=(-1)^1/2, suma del cuadrado de las raices.
k=2*pi/acos(/) # Calcula el largo del periodo, hay que tomar el valor de las ramces conjugadas.
k
