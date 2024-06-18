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

# Manejo del directorio
getwd()
directorio <- "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/1. Datos"

setwd(directorio)

# Chequeo de los archivos del directorio
dir()
list.files()

## Importacion de los datos ------------------
install_formats() # Cuestiones de importacion de archivos del paquete rio
data <- import("4. Bogota_Promedio_Dias.RDS")

# Estequiometria.
data <- data %>% mutate(no2_new = ((no2/(10^9))*(1000)*(1/((1*0.082*(tmp+273.15))/(pressure/760)))*(46.0055/1)*((10^6)/1)))
data <- data %>% mutate(co_new = (co*28.01*101.325)/(8.314*298.15))
data <- data %>% mutate(so2_new = ((so2/(10^9))*(1000)*(1/((1*0.082*(tmp+273.15))/(pressure/760)))*(64.066/1)*((10^6)/1)))
data <- data %>% mutate(o3_new = ((o3/(10^9))*(1000)*(1/((1*0.082*(tmp+273.15))/(pressure/760)))*(48/1)*((10^6)/1)))

head(data)

# Exprtar data
#write_xlsx(data, "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/1. Datos/7. Bogota_Promedio_Dias_Act.xlsx")

data <- import("7. Bogota_Promedio_Dias_Act.xlsx")

# -----------------------------------------------------------
# Sección 2: Analisis de correlacion entre variables 
# -----------------------------------------------------------

# La correlación de Pearson: Detectar relaciones lineales entre las variables.

# Calculando la matriz de correlación de Pearson
correlation_matrix_pearson <- cor(data[, c("o3", "no2", "pm25", "co", "so2", "pm10", "tmp", "rh", "rain", "radsolar", "pressure", "ws")], method = "pearson")
print("Matriz de Correlación de Pearson:")
print(correlation_matrix_pearson)

# Visualización de la matriz de correlación
corrplot(correlation_matrix_pearson, method = "color", type = "upper", order = "hclust",
         addCoef.col = "black", # Añadir coeficientes de correlación a la gráfica
         tl.col = "black", tl.srt = 45, # Ajustar el color y la rotación de las etiquetas
         diag = FALSE) # No mostrar la diagonal principal
title("Matriz de Correlación de Pearson", col.main = "black", font.main = 4)

# Generar el gráfico
png("C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/4. Gráficos/21. Matriz_de_Correlacion_Pearson.png")
corrplot(correlation_matrix_pearson, method = "color", type = "upper", order = "hclust",
         addCoef.col = "black", # Añadir coeficientes de correlación a la gráfica
         tl.col = "black", tl.srt = 45, # Ajustar el color y la rotación de las etiquetas
         diag = FALSE) # No mostrar la diagonal principal
title("Matriz de Correlación de Pearson", col.main = "black", font.main = 4)
dev.off() # Cerrar el dispositivo de gráficos


# La correlación de Spearman: Detectar relaciones no lineales o monótonas entre las variables.

# Calculando la matriz de correlación de Spearman
correlation_matrix_spearman <- cor(data[, c("o3", "no2", "pm25", "co", "so2", "pm10", "tmp", "rh", "rain", "radsolar", "pressure", "ws")], method = "spearman")
print("Matriz de Correlación de Spearman:")
print(correlation_matrix_spearman)

# Generar el gráfico
png("C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/4. Gráficos/22. Matriz_de_Correlacion_Spearman.png")
corrplot(correlation_matrix_spearman, method = "color", type = "upper", order = "hclust",
         addCoef.col = "black", # Añadir coeficientes de correlación a la gráfica
         tl.col = "black", tl.srt = 45, # Ajustar el color y la rotación de las etiquetas
         diag = FALSE) # No mostrar la diagonal principal
title("Matriz de Correlación de Spearman", col.main = "black", font.main = 4)
dev.off() # Cerrar el dispositivo de gráficos
