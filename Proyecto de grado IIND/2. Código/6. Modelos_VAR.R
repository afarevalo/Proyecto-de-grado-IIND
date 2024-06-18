# -----------------------------------------------------------
# Sección 1: Preparación de datos
# -----------------------------------------------------------

# Limpiar el entorno
rm(list = ls())

## Librerias ------------------
library(tidyverse)
#Incluimos las librerías de modelamiento y manejo de datos.
library(devtools)
library(fpp3)
library(urca)
library(vars)
library(MTS)
library(rio)

# Manejo del directorio
getwd()
directorio <- "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/1. Datos"

## Importacion de los datos ------------------
install_formats() # Cuestiones de importacion de archivos del paquete rio
da <- import("7. Bogota_Promedio_Dias_Act.xlsx")

# Convertir la base de datos "da" a formato ts
da.ts <- ts(da, start = as.Date("2021-01-01"), frequency = 1)

plot(da.ts)
str(da)

# Identificación nivel regresivo y prueba de cointegración

nivelka=VARselect(da, lag.max = 7, type = "const")
nivelka$selection





