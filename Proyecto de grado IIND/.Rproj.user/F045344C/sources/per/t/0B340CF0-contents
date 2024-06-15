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

# Unir todas las bases de datos en una sola
all_data <- rbind(Guaymaral_Por_Dias, MinAmbiente_Por_Dias, Suba_Por_Dias, 
                  Usaquen_Por_Dias, Ferias_Por_Dias, SanCristobal_Por_Dias, 
                  Tunal_Por_Dias, Bolivia_Por_Dias, Carvajal_Por_Dias, 
                  Fontibon_Por_Dias, Kennedy_Por_Dias, PuenteAranda_Por_Dias, 
                  Centro_Por_Dias)

#view(all_data)
#str(all_data)

# Exprtar data
#write_xlsx(all_data, "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/1. Datos/4. Estaciones_Por_Dias.xlsx")

head(all_data)

# Renombrar la variable "myday" como "day"
all_data$day <- all_data$myday

# Importar base vacia
Bogota_Promedio_Dias <- read_excel("2. Datos (2021 -2024) - Diarios.xlsx")
view(Bogota_Promedio_Dias)

# Calcula los promedios diarios de cada contaminante y otras variables
promedios_diarios <- all_data %>%
  group_by(day) %>%
  summarise(across(.cols = c(o3, no2, pm25, co, so2, pm10, bc, tmp, rh, rain, radsolar, co2, ws), .fns = ~mean(.x, na.rm = TRUE)))

# Asegurándonos de que las fechas están en formato de fecha si no lo están
promedios_diarios$day <- as.Date(promedios_diarios$day)
Bogota_Promedio_Dias$myday <- as.Date(Bogota_Promedio_Dias$myday)

# Renombrar la columna de fecha en promedios_diarios para coincidir con Bogota_Promedio_Dias
promedios_diarios <- rename(promedios_diarios, myday = day)

# Uniendo los datos
Bogota_Promedio_Dias <- left_join(Bogota_Promedio_Dias, promedios_diarios, by = "myday")
head(Bogota_Promedio_Dias)

# Eliminar columnas que terminan en ".x" y están completamente vacías
Bogota_Promedio_Dias <- Bogota_Promedio_Dias %>%
  select(-matches("\\.x$")) %>%
  select_if(~ !all(is.na(.)))

# Verificar el resultado
str(Bogota_Promedio_Dias)
view(Bogota_Promedio_Dias)

# Renombrar variables quitando el sufijo ".y"
Bogota_Promedio_Dias <- Bogota_Promedio_Dias %>%
  rename_with(~ gsub("\\.y$", "", .x), ends_with(".y"))

# Verificar los cambios
names(Bogota_Promedio_Dias)

# Guardar como archivo .RDS:
#saveRDS(Bogota_Promedio_Dias, "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/1. Datos/4. Bogota_Promedio_Dias.rds")
