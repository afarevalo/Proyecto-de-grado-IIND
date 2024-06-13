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

# Manejo del directorio
getwd()
directorio <- "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/1. Datos"

setwd(directorio)

# Chequeo de los archivos del directorio
dir()
list.files()

## Importacion de los datos ------------------
install_formats() # Cuestiones de importacion de archivos del paquete rio
Guaymaral_Por_Dias <- import("3.1 Guaymaral_Por_Dias.RDS")
MinAmbiente_Por_Dias <- import("3.2 MinAmbiente_Por_Dias.RDS")
Suba_Por_Dias <- import("3.3 Suba_Por_Dias.RDS")
Usaquen_Por_Dias <- import("3.4 Usaquen_Por_Dias.RDS")
Ferias_Por_Dias <- import("3.5 Ferias_Por_Dias.RDS")
SanCristobal_Por_Dias <- import("3.6 SanCristobal_Por_Dias.RDS")
Tunal_Por_Dias <- import("3.7 Tunal_Por_Dias.RDS")
Bolivia_Por_Dias <- import("3.8 Bolivia_Por_Dias.RDS")
Carvajal_Por_Dias <- import("3.9 Carvajal_Por_Dias.RDS")
Fontibon_Por_Dias <- import("3.10 Fontibon_Por_Dias.RDS")
Kennedy_Por_Dias <- import("3.11 Kennedy_Por_Dias.RDS")
PuenteAranda_Por_Dias <- import("3.12 PuenteAranda_Por_Dias.RDS")
Centro_Por_Dias <- import("3.13 Centro_Por_Dias.RDS")

# Lista de todas las bases de datos
list_dfs <- list(Guaymaral_Por_Dias, MinAmbiente_Por_Dias, Suba_Por_Dias, Usaquen_Por_Dias,
                 Ferias_Por_Dias, SanCristobal_Por_Dias, Tunal_Por_Dias, Bolivia_Por_Dias,
                 Carvajal_Por_Dias, Fontibon_Por_Dias, Kennedy_Por_Dias, PuenteAranda_Por_Dias,
                 Centro_Por_Dias)

# Unimos todas las bases en una sola
combined_data <- bind_rows(list_dfs, .id = "source")

# Agrupamos por fecha y calculamos el promedio sin contar NA
averages <- combined_data %>%
  group_by(myday) %>%
  summarise(no2_avg = mean(no2, na.rm = TRUE), .groups = "drop")

# Importamos la base de datos Bogota_Prom_Dias
Bogota_Prom_Dias <- read_excel("2. Datos (2021 -2024) - Diarios.xlsx")

# Realizamos el merge y actualizamos la columna o3 en Bogota_Prom_Dias
Bogota_Prom_Dias_updated <- Bogota_Prom_Dias %>%
  left_join(averages, by = "myday") %>%
  mutate(no2 = coalesce(no2_avg, no2)) %>%
  select(-no2_avg) # Eliminamos la columna auxiliar o3_avg

Bogota_Prom_Dias_updated




