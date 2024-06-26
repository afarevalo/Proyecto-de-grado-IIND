# Pronosticos
#Sem 12 y 13: MOdelos VAR y VECM 

### 
# 1) Datos
# 2) Visualización
# 3) Caracterización
# 4) Construcción Modelos y Selección
# 5) Revisión Residuos
# 6) Pronóstico
setwd("C:/Users/al.abrego/OneDrive - Universidad de los Andes/Documentos/R/EF_1")

#install.packages('MTS')
#install.packages("devtools")
library(devtools)
library(fpp3)

#install.packages("MTS_VERSION.tar.gz",repos=NULL,type="source")
library(MTS)

## 1) Datos
##VAR con función VAR()
da=read.table("q-gdp-ukcaus.txt",header=T)
head(da)
gdp=log(da[,3:5])
z=gdp[2:126,]-gdp[1:125,]
head(z)
dim(gdp)
MTSplot(gdp)

##Prueba cointegración:
library(urca)
johatest=ca.jo(gdp, type = "trace", K=2, ecdet ="none", spec = "longrun") #K is the number of lags to use in the vector autoregressive model and is set this to the minimum, K=2.
summary(johatest)
#
uk=diff(gdp$uk,1)
ca=diff((gdp$ca),1)
us=diff(gdp$us,1)
z=cbind.data.frame(uk,ca,us)
head(z)
str(z)
#
### Use the VAR command in MTS: series no tienen que estar cointegradas, pero si ser estacionarias.
m=VARorder(z) #A partir 3er rezago cae significancia del modelo, elegimos p=2.
##Otra paqueteria:
library(vars)
m0=vars::VAR(z, p=2)
summary(m0)
##libreria MTS:
m1=MTS::VAR(z,2)
#Model check:
resi=m1$residuals
mq(resi, adj=18)  #Ljung Box: 18 Porque se ajustan los grados de libertad, de 24 default, hay 18 parámetros en modelo
acf(resi)  #SE VEN VIOLACIONES EN EL RETRASO #4 ENTRE SERIE 1 Y SERIE3
MTSdiag(m1, adj=18)  

##Simplificación modelo, pues hay retrazos insignificantes para el primer vector, principalmente, y en algunos términos de AR(2)
library(MTS)
##
m2=refVAR(m1,thres = 1.96)  #1.96 threshold para un nivel de significancia del 5%. Mejoró el AIC
##Model check:
resi2=m2$residuals
mq(resi2, adj=10)  #Posiblemente una mejora sería ajsutarlo a VAR(4)
acf(resi2)
MTSdiag(m2, adj=10)  #El modelo reduce a 10 los parámetros

## La interpretación de la matriz de correlacion de residuales (CCM at lag 0) 
#es que la tasa de crecimiento de UK y Canada no estan correlacionadas de manera instantanea, ni ninguno.

#Ahora, la ecuación final tridimensional (a partir de las matrices AR1 y AR2 en "mod2"), dice que
#el crecimiento del GDP de (UK, Ca, US): UK no depende de la tasa de crecimiento pasadas de US en presencia de la tasa
#de Canada., pero la tasa GDP de UK depende de las tasas pasadas de Canada.

#Tambien, la tasa GDP de Canada esta dinamicamente relacionada con las tasas GDP de UK y US.
#Similarmente, las tasas de crecimeinto GDP de US dependen de los valores pasados o retrasados (a 1 periodos)
#de UK y Ca. Las series no están dinámicamente relacionadas entre ellas a partir del segundo lag.

##PREDICTION
predm2=VARpred(m2, 8)
ts.plot(predm2$pred)

##Impulse Response
library(MTS)
library(mvtnorm)
#Phi=m2$Phi
#Sig=m2$Sigma

library(vars)
m1irf = irf(m0, n.ahead = 8, boot = TRUE)
plot(m1irf)  #predicción eje Y: es la var dependiente, acorde el impulso X.

m2
#--------------------------------------------------------------------------------------------------
#####VECM MODELS Multivariate Times series ANalysis Tsay
require(fUnitRoots)
require(urca)
require(MTS)
da=read.table("m-bnd.txt")
head(da)
tail(da)

### Co-integration test
bnd=da[,4:5]
colnames(bnd) <- c("Aaa","Baa")
m1=VARorder(bnd)

pacf(bnd[,1])
pacf(bnd[,2])
adfTest(bnd[,1],lags=3)
adfTest(bnd[,2],lags=2)

m2=ca.jo(bnd,K=2,ecdet=c("none"))
summary(m2)

m4=ca.jo(bnd,K=2,ecdet=c("none"),type=c("trace"),spec=c("transitory"))
summary(m4)

##Contegration series:
wt=bnd[,1]-0.886*bnd[,2]  #empleamos los vectores característicos.
adfTest(wt,lags=3,type="c")
##La series no poseen una raíz unitaria.
plot(wt, type="l")  #Grafico de las series cointegradas, las cuales se muestran estacionarias.

############### Estimation of ECM model
m1=ECMvar1(bnd,3,wt) ## Given the co-integrated vector esta función es util cuando se conoce la serie wt de cointegración.


m2=refECMvar1(m1)  ####### Refine the model fit

beta=c(1,-0.886) ### Initial value of co-integration
m3=ECMvar(bnd,3,beta,include.const=F) #### Joint estimation

##Pronostico:
library(vars)
library(tsDyn)
mvecm=VECM(bnd, lag=3, r=1, estim = ("ML")) #R es el rango, rango de cointegración.
summary(mvecm)

varmod_vecm1=vec2var(m4, r=1)
pron3=predict(varmod_vecm1, n.ahead = 8, ci = 0.958)
pron3
##########################################
#
