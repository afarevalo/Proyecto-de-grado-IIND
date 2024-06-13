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

# Importar datos
Guaymaral_Por_Horas <- read_excel("1. Datos (2021 -2024) - Por Horas.xlsx", 
                                           sheet = "Guaymaral")

Guaymaral_Por_Dias <- read_excel("2. Datos (2021 -2024) - Diarios.xlsx")


# Calculo de promedios
promedios_diarios <- Guaymaral_Por_Horas %>%
  group_by(Dia) %>%
  summarise(
    pm10_avg = mean(PM10, na.rm = TRUE),
    tmp_avg = mean(Temperatura, na.rm = TRUE),
    ws_avg = mean(VelViento10M, na.rm = TRUE),
    rh_avg = mean(HR, na.rm = TRUE),
    radsolar_avg = mean(RadSolar, na.rm = TRUE),
    rain_avg = mean(Precipitacion, na.rm = TRUE),
    o3_avg = mean(OZONO, na.rm = TRUE),
    no2_avg = mean(NO2, na.rm = TRUE),
    pm25_avg = mean(PM25, na.rm = TRUE),
    co_avg = mean(CO, na.rm = TRUE),
    so2_avg = mean(SO2, na.rm = TRUE)
  )

Guaymaral_Por_Dias <- Guaymaral_Por_Dias %>%
  left_join(promedios_diarios, by = c("myday" = "Dia")) %>%
  mutate(
    pm10 = if_else(is.na(pm10), pm10_avg, pm10),
    tmp = if_else(is.na(tmp), tmp_avg, tmp),
    ws = if_else(is.na(ws), ws_avg, ws),
    rh = if_else(is.na(rh), rh_avg, rh),
    radsolar = if_else(is.na(radsolar), radsolar_avg, radsolar),
    rain = if_else(is.na(rain), rain_avg, rain),
    o3 = if_else(is.na(o3), o3_avg, o3),
    no2 = if_else(is.na(no2), no2_avg, no2),
    pm25 = if_else(is.na(pm25), pm25_avg, pm25),
    co = if_else(is.na(co), co_avg, co),
    so2 = if_else(is.na(so2), so2_avg, so2)
  ) %>%
  select(-ends_with("_avg"))  # Elimina las columnas de promedios temporales

summary(Guaymaral_Por_Dias)

# Guardar como archivo .RDS:
saveRDS(Guaymaral_Por_Dias, "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/1. Datos/Guaymaral_Por_Dias.rds")

#-------------------------------------------------------------------------------

# Importar datos
MinAmbiente_Por_Horas <- read_excel("1. Datos (2021 -2024) - Por Horas.xlsx", 
                                  sheet = "MinAmbiente")

MinAmbiente_Por_Dias <- read_excel("2. Datos (2021 -2024) - Diarios.xlsx")


# Calculo de promedios
promedios_diarios <- MinAmbiente_Por_Horas %>%
  group_by(Dia) %>%
  summarise(
    pm10_avg = mean(PM10, na.rm = TRUE),
    ws_avg = mean(VelViento, na.rm = TRUE),
    rain_avg = mean(Precipitacion, na.rm = TRUE),
    o3_avg = mean(OZONO, na.rm = TRUE),
    no2_avg = mean(NO2, na.rm = TRUE),
    pm25_avg = mean(PM25, na.rm = TRUE),
    co_avg = mean(CO, na.rm = TRUE),
  )

MinAmbiente_Por_Dias <- MinAmbiente_Por_Dias %>%
  left_join(promedios_diarios, by = c("myday" = "Dia")) %>%
  mutate(
    pm10 = if_else(is.na(pm10), pm10_avg, pm10),
    ws = if_else(is.na(ws), ws_avg, ws),
    rain = if_else(is.na(rain), rain_avg, rain),
    o3 = if_else(is.na(o3), o3_avg, o3),
    no2 = if_else(is.na(no2), no2_avg, no2),
    pm25 = if_else(is.na(pm25), pm25_avg, pm25),
    co = if_else(is.na(co), co_avg, co)
  ) %>%
  select(-ends_with("_avg"))  # Elimina las columnas de promedios temporales

summary(MinAmbiente_Por_Dias)

# Guardar como archivo .RDS:
saveRDS(MinAmbiente_Por_Dias, "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/1. Datos/MinAmbiente_Por_Dias.rds")

#-------------------------------------------------------------------------------







