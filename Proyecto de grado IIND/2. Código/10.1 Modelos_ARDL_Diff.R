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
library(ARDL)
library(urca)
library(TSstudio)
library(quantmod) 
library(fields)
library(dygraphs)
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
da.ts <- ts(da[2:5], start = as.Date(2021), frequency = 365)
plot(da.ts)

# ARDL (Autoregressive Distributed Lag)
# ARML Se puede aplicar, pues todas las series son estacionarias.
# Aplica para i1 e i0.

pm25=diff(da.ts[,1],1)
radsolar=diff(da.ts[,2],1)
ws=diff(da.ts[,3],1)
rh=diff(da.ts[,4],1)
z=cbind.data.frame(pm25,radsolar,ws,rh)
head(z)
str(z)

# -----------------------------------------------------------
# Sección 2: Búsqueda tipo grid search 
# -----------------------------------------------------------

#Selección automatica:
models <- auto_ardl(pm25 ~ radsolar + ws +rh, data = z, lamda = TRUE,max_order = 6)

#Revisemos el top 20 de los mejores modelos según su critrio de información de Akaike
models$top_orders

# -----------------------------------------------------------
# Sección 3: Modelo ARDL 
# -----------------------------------------------------------

#Procedemos a construir el modelo de regresión con la mejor combinación.
mod1 <- ardl(pm25 ~ rh + radsolar + ws, data = z, lamda = TRUE ,order = c(6,6,6,6))
summary(mod1)

# Para la interpretación, podemos imprimir los rezagos correspondientes de cada variable que explican la respuesta.
mod1$full_formula

#Predicción del primer modelo sin tendencia:
stats::predict(mod1$fitted.values, 10)

p_pm25 <- autoplot(predict(mod1$fitted.values, h=10)) +
  geom_line() +
  labs(x = "Tiempo", 
       y = "PM2.5",
       title = "Predicción del PM2.5") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centra y ajusta el título
    axis.title = element_text(size = 14),  # Ajusta el tamaño de las etiquetas de los ejes
    axis.text = element_text(size = 12),   # Ajusta el tamaño del texto de los ejes
    plot.background = element_rect(fill = "white"),  # Cambia el fondo del gráfico
    panel.grid.minor = element_blank()  # Elimina las líneas de cuadrícula menores
  )
p_pm25

# Exportar como PNG
ggsave("24. Predicción del PM2.5 desde 2021 hasta 2024.png", 
       plot = last_plot(),
       path = "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/4. Gráficos", 
       width = 10, height = 6, units = "in", dpi = 300)

# -----------------------------------------------------------
# Sección 3: Bounds Test 
# -----------------------------------------------------------

# Guardamos el mejor modelo obtenido en la sección anterior
modelo <- models$best_model

# Realizamos la prueba de hipotesis
# El parametro "case" igual a 2 verifica si existe relaciones a largo termino, 
# con la combinación de (restricted constant, no linear trend).
bounds_f_test(modelo, case = 2)

# H0:NO Existe un equilibrio a corto plazo
# H1: Existe un equilibrio a corto plazo
# p-value = 1e-06 < 5%, se rechaza H0, Existe un equilibrio a corto plazo.

# Multiplicadores a corto plazo
# sr_ short run
multipliers(modelo, type = "sr")

# Interpretación de los coeficientes:
# - Por cada unidad de mmhg pressure, el PM2.5 disminuye en -0.1544445652 µg/m3, en corto plazo.
# - Por cada unidad de C tmp, el PM2.5 disminuye en -0.8314739740 µg/m3, en corto plazo.
# - Por cada unidad de W/M^2 radsolar, el PM2.5 disminuye en 0.0048481106 µg/m3, en corto plazo.
# - Por cada unidad de M/S ws, el PM2.5 disminuye en -1.6913903054 µg/m3, en corto plazo.

# Como el modelo presenta cointegración, aplica:
bounds_f_test(modelo, case = 3)
# H0:NO Existe un equilibrio a largo plazo
# H1:Existe un equilibrio a largo plazo
# p-value = 1e-06 < 5%, se rechaza H0, Existe un equilibrio a largo plazo.

# Multiplicadores a largo plazo
multipliers(modelo, type = "lr")
# - Por cada unidad de mmhg de pressure, el PM2.5 disminuye en -4.0469894 µg/m3, en corto plazo.
# - Por cada unidad de C de tmp, el PM2.5 disminuye en 0.6307268 µg/m3, en corto plazo.
# - Por cada unidad de W/M^2 de radsolar, el PM2.5 disminuye en 0.0892383 µg/m3, en corto plazo.
# - Por cada unidad de M/S de ws, el PM2.5 disminuye en -3.4238851 µg/m3, en corto plazo.

### Cuanto es el largo plazo???

a <- resid(modelo)
pacf(a, 30)

library(tsDyn)
library(vars)
library(urca)
library(forecast)
library(tidyverse)

checkresiduals(modelo)

# G1: No hay una tendencia obvia en los residuos, lo cual es bueno porque 
# indica que el modelo no tiene sesgo sistemático a lo largo del tiempo. 

# G2: Hay autocorrelación residual.
# Esto Puede subestimar o sobreestimar los coeficientes, el modelo no es tan bueno???

# G3: Normalidad en la distribución de los residuos.

vif(modelo)
# Dado que todos son menores a 10 no hay problemas graves de multicolinealidad en el modelo.
