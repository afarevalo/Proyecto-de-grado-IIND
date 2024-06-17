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

#-------------------------------------------------------------------------------

## Guaymaral

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
    so2_avg = mean(SO2, na.rm = TRUE),
    PresionBaro_avg = mean(PresionBaro, na.rm = TRUE)
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
    pressure = if_else(is.na(pressure), PresionBaro_avg, pressure)
  ) %>%
  select(-ends_with("_avg"))  # Elimina las columnas de promedios temporales

summary(Guaymaral_Por_Dias)

# Guardar como archivo .RDS:
saveRDS(Guaymaral_Por_Dias, "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/1. Datos/3.1 Guaymaral_Por_Dias.rds")

#-------------------------------------------------------------------------------

## MinAmbiente

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
saveRDS(MinAmbiente_Por_Dias, "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/1. Datos/3.2 MinAmbiente_Por_Dias.rds")

#-------------------------------------------------------------------------------

## Suba

# Importar datos
Suba_Por_Horas <- read_excel("1. Datos (2021 -2024) - Por Horas.xlsx", 
                                    sheet = "Suba")

Suba_Por_Dias <- read_excel("2. Datos (2021 -2024) - Diarios.xlsx")

# Calculo de promedios
promedios_diarios <- Suba_Por_Horas %>%
  group_by(Dia) %>%
  summarise(
    pm10_avg = mean(PM10, na.rm = TRUE),
    tmp_avg = mean(Temperatura, na.rm = TRUE),
    ws_avg = mean(VelViento, na.rm = TRUE),
    rain_avg = mean(Precipitacion, na.rm = TRUE),
    o3_avg = mean(OZONO, na.rm = TRUE),
    no2_avg = mean(NO2, na.rm = TRUE),
    pm25_avg = mean(PM25, na.rm = TRUE),
    co_avg = mean(CO, na.rm = TRUE),
    so2_avg = mean(SO2, na.rm = TRUE)
  )

Suba_Por_Dias <- Suba_Por_Dias %>%
  left_join(promedios_diarios, by = c("myday" = "Dia")) %>%
  mutate(
    pm10 = if_else(is.na(pm10), pm10_avg, pm10),
    tmp = if_else(is.na(tmp), tmp_avg, tmp),
    ws = if_else(is.na(ws), ws_avg, ws),
    rain = if_else(is.na(rain), rain_avg, rain),
    o3 = if_else(is.na(o3), o3_avg, o3),
    no2 = if_else(is.na(no2), no2_avg, no2),
    pm25 = if_else(is.na(pm25), pm25_avg, pm25),
    co = if_else(is.na(co), co_avg, co),
    so2 = if_else(is.na(so2), so2_avg, so2)
  ) %>%
  select(-ends_with("_avg"))  # Elimina las columnas de promedios temporales

summary(Suba_Por_Dias)


# Guardar como archivo .RDS:
saveRDS(Suba_Por_Dias, "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/1. Datos/3.3 Suba_Por_Dias.rds")

#-------------------------------------------------------------------------------

## Usaquen

# Importar datos
Usaquen_Por_Horas <- read_excel("1. Datos (2021 -2024) - Por Horas.xlsx", 
                                  sheet = "Usaquen")

Usaquen_Por_Dias <- read_excel("2. Datos (2021 -2024) - Diarios.xlsx")


# Calculo de promedios
promedios_diarios <- Usaquen_Por_Horas %>%
  group_by(Dia) %>%
  summarise(
    pm10_avg = mean(PM10, na.rm = TRUE),
    tmp_avg = mean(Temperatura, na.rm = TRUE),
    ws_avg = mean(VelViento, na.rm = TRUE),
    rain_avg = mean(Precipitacion, na.rm = TRUE),
    o3_avg = mean(OZONO, na.rm = TRUE),
    no2_avg = mean(NO2, na.rm = TRUE),
    pm25_avg = mean(PM25, na.rm = TRUE),
    co_avg = mean(CO, na.rm = TRUE),
    so2_avg = mean(SO2, na.rm = TRUE)
  )

Usaquen_Por_Dias <- Usaquen_Por_Dias %>%
  left_join(promedios_diarios, by = c("myday" = "Dia")) %>%
  mutate(
    pm10 = if_else(is.na(pm10), pm10_avg, pm10),
    tmp = if_else(is.na(tmp), tmp_avg, tmp),
    ws = if_else(is.na(ws), ws_avg, ws),
    rain = if_else(is.na(rain), rain_avg, rain),
    o3 = if_else(is.na(o3), o3_avg, o3),
    no2 = if_else(is.na(no2), no2_avg, no2),
    pm25 = if_else(is.na(pm25), pm25_avg, pm25),
    co = if_else(is.na(co), co_avg, co),
    so2 = if_else(is.na(so2), so2_avg, so2)
  ) %>%
  select(-ends_with("_avg"))  # Elimina las columnas de promedios temporales

summary(Usaquen_Por_Dias)

# Guardar como archivo .RDS:
saveRDS(Usaquen_Por_Dias, "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/1. Datos/3.4 Usaquen_Por_Dias.rds")


#-------------------------------------------------------------------------------

## Guaymaral

# Importar datos
Ferias_Por_Horas <- read_excel("1. Datos (2021 -2024) - Por Horas.xlsx", 
                                  sheet = "Las Ferias")

Ferias_Por_Dias <- read_excel("2. Datos (2021 -2024) - Diarios.xlsx")


# Calculo de promedios
promedios_diarios <- Ferias_Por_Horas %>%
  group_by(Dia) %>%
  summarise(
    pm10_avg = mean(PM10, na.rm = TRUE),
    tmp_avg = mean(Temperatura, na.rm = TRUE),
    ws_avg = mean(VelViento, na.rm = TRUE),
    rh_avg = mean(HR, na.rm = TRUE),
    rain_avg = mean(Precipitacion, na.rm = TRUE),
    o3_avg = mean(OZONO, na.rm = TRUE),
    no2_avg = mean(NO2, na.rm = TRUE),
    pm25_avg = mean(PM25, na.rm = TRUE),
    co_avg = mean(CO, na.rm = TRUE),
    PresionBaro_avg = mean(PresionBaro, na.rm = TRUE)
  )

Ferias_Por_Dias <- Ferias_Por_Dias %>%
  left_join(promedios_diarios, by = c("myday" = "Dia")) %>%
  mutate(
    pm10 = if_else(is.na(pm10), pm10_avg, pm10),
    tmp = if_else(is.na(tmp), tmp_avg, tmp),
    ws = if_else(is.na(ws), ws_avg, ws),
    rh = if_else(is.na(rh), rh_avg, rh),
    rain = if_else(is.na(rain), rain_avg, rain),
    o3 = if_else(is.na(o3), o3_avg, o3),
    no2 = if_else(is.na(no2), no2_avg, no2),
    pm25 = if_else(is.na(pm25), pm25_avg, pm25),
    co = if_else(is.na(co), co_avg, co),
    pressure = if_else(is.na(pressure), PresionBaro_avg, pressure)
  ) %>%
  select(-ends_with("_avg"))  # Elimina las columnas de promedios temporales

summary(Ferias_Por_Dias)

# Guardar como archivo .RDS:
saveRDS(Ferias_Por_Dias, "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/1. Datos/3.5 Ferias_Por_Dias.rds")

#-------------------------------------------------------------------------------

## San Cristobal

# Importar datos
SanCristobal_Por_Horas <- read_excel("1. Datos (2021 -2024) - Por Horas.xlsx", 
                                  sheet = "San Cristobal")

SanCristobal_Por_Dias <- read_excel("2. Datos (2021 -2024) - Diarios.xlsx")


# Calculo de promedios
promedios_diarios <- SanCristobal_Por_Horas %>%
  group_by(Dia) %>%
  summarise(
    pm10_avg = mean(PM10, na.rm = TRUE),
    tmp_avg = mean(Temperatura, na.rm = TRUE),
    ws_avg = mean(VelViento, na.rm = TRUE),
    rh_avg = mean(HR, na.rm = TRUE),
    radsolar_avg = mean(RadSolar, na.rm = TRUE),
    rain_avg = mean(Precipitacion, na.rm = TRUE),
    o3_avg = mean(OZONO, na.rm = TRUE),
    no2_avg = mean(NO2, na.rm = TRUE),
    pm25_avg = mean(PM25, na.rm = TRUE),
    co_avg = mean(CO, na.rm = TRUE),
    bc_avg = mean(BC, na.rm = TRUE)
  )

SanCristobal_Por_Dias <- SanCristobal_Por_Dias %>%
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
    bc = if_else(is.na(bc), bc_avg, bc)
  ) %>%
  select(-ends_with("_avg"))  # Elimina las columnas de promedios temporales

summary(SanCristobal_Por_Dias)

# Guardar como archivo .RDS:
saveRDS(SanCristobal_Por_Dias, "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/1. Datos/3.6 SanCristobal_Por_Dias.rds")

#-------------------------------------------------------------------------------

## Tunal

# Importar datos
Tunal_Por_Horas <- read_excel("1. Datos (2021 -2024) - Por Horas.xlsx", 
                                  sheet = "Tunal")

Tunal_Por_Dias <- read_excel("2. Datos (2021 -2024) - Diarios.xlsx")


# Calculo de promedios
promedios_diarios <- Tunal_Por_Horas %>%
  group_by(Dia) %>%
  summarise(
    pm10_avg = mean(PM10, na.rm = TRUE),
    tmp_avg = mean(Temperatura, na.rm = TRUE),
    ws_avg = mean(VelViento, na.rm = TRUE),
    rh_avg = mean(HR, na.rm = TRUE),
    radsolar_avg = mean(RadSolar, na.rm = TRUE),
    rain_avg = mean(Precipitacion, na.rm = TRUE),
    o3_avg = mean(OZONO, na.rm = TRUE),
    no2_avg = mean(NO2, na.rm = TRUE),
    pm25_avg = mean(PM25, na.rm = TRUE),
    co_avg = mean(CO, na.rm = TRUE),
    so2_avg = mean(SO2, na.rm = TRUE),
    bc_avg = mean(BC, na.rm = TRUE),
    PresionBaro_avg = mean(PresionBaro, na.rm = TRUE)
  )

Tunal_Por_Dias <- Tunal_Por_Dias %>%
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
    so2 = if_else(is.na(so2), so2_avg, so2),
    bc = if_else(is.na(bc), bc_avg, bc),
    pressure = if_else(is.na(pressure), PresionBaro_avg, pressure)
  ) %>%
  select(-ends_with("_avg"))  # Elimina las columnas de promedios temporales

summary(Tunal_Por_Dias)

# Guardar como archivo .RDS:
saveRDS(Tunal_Por_Dias, "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/1. Datos/3.7 Tunal_Por_Dias.rds")

#-------------------------------------------------------------------------------

## Bolivia

# Importar datos
Bolivia_Por_Horas <- read_excel("1. Datos (2021 -2024) - Por Horas.xlsx", 
                              sheet = "Bolivia")

Bolivia_Por_Dias <- read_excel("2. Datos (2021 -2024) - Diarios.xlsx")


# Calculo de promedios
promedios_diarios <- Bolivia_Por_Horas %>%
  group_by(Dia) %>%
  summarise(
    pm10_avg = mean(PM10, na.rm = TRUE),
    o3_avg = mean(OZONO, na.rm = TRUE),
    no2_avg = mean(NO2, na.rm = TRUE),
    pm25_avg = mean(PM25, na.rm = TRUE),
    co_avg = mean(CO, na.rm = TRUE),
    so2_avg = mean(SO2, na.rm = TRUE)
  )

Bolivia_Por_Dias <- Bolivia_Por_Dias %>%
  left_join(promedios_diarios, by = c("myday" = "Dia")) %>%
  mutate(
    pm10 = if_else(is.na(pm10), pm10_avg, pm10),
    o3 = if_else(is.na(o3), o3_avg, o3),
    no2 = if_else(is.na(no2), no2_avg, no2),
    pm25 = if_else(is.na(pm25), pm25_avg, pm25),
    co = if_else(is.na(co), co_avg, co),
    so2 = if_else(is.na(so2), so2_avg, so2)
  ) %>%
  select(-ends_with("_avg"))  # Elimina las columnas de promedios temporales

summary(Bolivia_Por_Dias)

# Guardar como archivo .RDS:
saveRDS(Bolivia_Por_Dias, "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/1. Datos/3.8 Bolivia_Por_Dias.rds")

#-------------------------------------------------------------------------------

## Carvajal

# Importar datos
Carvajal_Por_Horas <- read_excel("1. Datos (2021 -2024) - Por Horas.xlsx", 
                              sheet = "Carvajal")

Carvajal_Por_Dias <- read_excel("2. Datos (2021 -2024) - Diarios.xlsx")

# Calculo de promedios
promedios_diarios <- Carvajal_Por_Horas %>%
  group_by(Dia) %>%
  summarise(
    pm10_avg = mean(PM10, na.rm = TRUE),
    tmp_avg = mean(Temperatura, na.rm = TRUE),
    ws_avg = mean(VelViento, na.rm = TRUE),
    rain_avg = mean(Precipitacion, na.rm = TRUE),
    o3_avg = mean(OZONO, na.rm = TRUE),
    no2_avg = mean(NO2, na.rm = TRUE),
    pm25_avg = mean(PM25, na.rm = TRUE),
    co_avg = mean(CO, na.rm = TRUE),
    so2_avg = mean(SO2, na.rm = TRUE)
  )

Carvajal_Por_Dias <- Carvajal_Por_Dias %>%
  left_join(promedios_diarios, by = c("myday" = "Dia")) %>%
  mutate(
    pm10 = if_else(is.na(pm10), pm10_avg, pm10),
    tmp = if_else(is.na(tmp), tmp_avg, tmp),
    ws = if_else(is.na(ws), ws_avg, ws),
    rain = if_else(is.na(rain), rain_avg, rain),
    o3 = if_else(is.na(o3), o3_avg, o3),
    no2 = if_else(is.na(no2), no2_avg, no2),
    pm25 = if_else(is.na(pm25), pm25_avg, pm25),
    co = if_else(is.na(co), co_avg, co),
    so2 = if_else(is.na(so2), so2_avg, so2)
  ) %>%
  select(-ends_with("_avg"))  # Elimina las columnas de promedios temporales

summary(Carvajal_Por_Dias)

# Guardar como archivo .RDS:
saveRDS(Carvajal_Por_Dias, "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/1. Datos/3.9 Carvajal_Por_Dias.rds")

#-------------------------------------------------------------------------------

## Fontibon

# Importar datos
Fontibon_Por_Horas <- read_excel("1. Datos (2021 -2024) - Por Horas.xlsx", 
                              sheet = "Fontibon")

Fontibon_Por_Dias <- read_excel("2. Datos (2021 -2024) - Diarios.xlsx")

# Calculo de promedios
promedios_diarios <- Fontibon_Por_Horas %>%
  group_by(Dia) %>%
  summarise(
    pm10_avg = mean(PM10, na.rm = TRUE),
    tmp_avg = mean(Temperatura, na.rm = TRUE),
    ws_avg = mean(VelViento, na.rm = TRUE),
    rh_avg = mean(HR, na.rm = TRUE),
    rain_avg = mean(Precipitacion, na.rm = TRUE),
    o3_avg = mean(OZONO, na.rm = TRUE),
    no2_avg = mean(NO2, na.rm = TRUE),
    pm25_avg = mean(PM25, na.rm = TRUE),
    co_avg = mean(CO, na.rm = TRUE),
    so2_avg = mean(SO2, na.rm = TRUE),
    bc_avg = mean(BC, na.rm = TRUE),
    PresionBaro_avg = mean(PresionBaro, na.rm = TRUE)
  )

Fontibon_Por_Dias <- Fontibon_Por_Dias %>%
  left_join(promedios_diarios, by = c("myday" = "Dia")) %>%
  mutate(
    pm10 = if_else(is.na(pm10), pm10_avg, pm10),
    tmp = if_else(is.na(tmp), tmp_avg, tmp),
    ws = if_else(is.na(ws), ws_avg, ws),
    rh = if_else(is.na(rh), rh_avg, rh),
    rain = if_else(is.na(rain), rain_avg, rain),
    o3 = if_else(is.na(o3), o3_avg, o3),
    no2 = if_else(is.na(no2), no2_avg, no2),
    pm25 = if_else(is.na(pm25), pm25_avg, pm25),
    co = if_else(is.na(co), co_avg, co),
    so2 = if_else(is.na(so2), so2_avg, so2),
    bc = if_else(is.na(bc), bc_avg, bc),
    pressure = if_else(is.na(pressure), PresionBaro_avg, pressure)
  ) %>%
  select(-ends_with("_avg"))  # Elimina las columnas de promedios temporales

summary(Fontibon_Por_Dias)

# Guardar como archivo .RDS:
saveRDS(Fontibon_Por_Dias, "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/1. Datos/3.10 Fontibon_Por_Dias.rds")

#-------------------------------------------------------------------------------

## Kennedy

# Importar datos
Kennedy_Por_Horas <- read_excel("1. Datos (2021 -2024) - Por Horas.xlsx", 
                                 sheet = "Kennedy")

Kennedy_Por_Dias <- read_excel("2. Datos (2021 -2024) - Diarios.xlsx")

# Calculo de promedios
promedios_diarios <- Kennedy_Por_Horas %>%
  group_by(Dia) %>%
  summarise(
    pm10_avg = mean(PM10, na.rm = TRUE),
    tmp_avg = mean(Temperatura, na.rm = TRUE),
    ws_avg = mean(VelViento, na.rm = TRUE),
    rh_avg = mean(HR, na.rm = TRUE),
    rain_avg = mean(Precipitacion, na.rm = TRUE),
    radsolar_avg = mean(RadSolar, na.rm = TRUE),
    o3_avg = mean(OZONO, na.rm = TRUE),
    no2_avg = mean(NO2, na.rm = TRUE),
    pm25_avg = mean(PM25, na.rm = TRUE),
    co_avg = mean(CO, na.rm = TRUE),
    so2_avg = mean(SO2, na.rm = TRUE),
    bc_avg = mean(BC, na.rm = TRUE),
    co2_avg = mean(CO2, na.rm = TRUE)
  )

Kennedy_Por_Dias <- Kennedy_Por_Dias %>%
  left_join(promedios_diarios, by = c("myday" = "Dia")) %>%
  mutate(
    pm10 = if_else(is.na(pm10), pm10_avg, pm10),
    tmp = if_else(is.na(tmp), tmp_avg, tmp),
    ws = if_else(is.na(ws), ws_avg, ws),
    rh = if_else(is.na(rh), rh_avg, rh),
    rain = if_else(is.na(rain), rain_avg, rain),
    radsolar = if_else(is.na(radsolar), radsolar_avg, radsolar),
    o3 = if_else(is.na(o3), o3_avg, o3),
    no2 = if_else(is.na(no2), no2_avg, no2),
    pm25 = if_else(is.na(pm25), pm25_avg, pm25),
    co = if_else(is.na(co), co_avg, co),
    so2 = if_else(is.na(so2), so2_avg, so2),
    bc = if_else(is.na(bc), bc_avg, bc),
    co2 = if_else(is.na(co2), co2_avg, co2)
  ) %>%
  select(-ends_with("_avg"))  # Elimina las columnas de promedios temporales

summary(Kennedy_Por_Dias)

# Guardar como archivo .RDS:
saveRDS(Kennedy_Por_Dias, "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/1. Datos/3.11 Kennedy_Por_Dias.rds")

#-------------------------------------------------------------------------------

## Puente Aranda

# Importar datos
PuenteAranda_Por_Horas <- read_excel("1. Datos (2021 -2024) - Por Horas.xlsx", 
                                sheet = "Puente Aranda")

PuenteAranda_Por_Dias <- read_excel("2. Datos (2021 -2024) - Diarios.xlsx")

# Calculo de promedios
promedios_diarios <- PuenteAranda_Por_Horas %>%
  group_by(Dia) %>%
  summarise(
    pm10_avg = mean(PM10, na.rm = TRUE),
    tmp_avg = mean(Temperatura, na.rm = TRUE),
    ws_avg = mean(VelViento, na.rm = TRUE),
    rh_avg = mean(HR, na.rm = TRUE),
    rain_avg = mean(Precipitacion, na.rm = TRUE),
    radsolar_avg = mean(RadSolar, na.rm = TRUE),
    o3_avg = mean(OZONO, na.rm = TRUE),
    no2_avg = mean(NO2, na.rm = TRUE),
    pm25_avg = mean(PM25, na.rm = TRUE),
    co_avg = mean(CO, na.rm = TRUE),
    so2_avg = mean(SO2, na.rm = TRUE),
    bc_avg = mean(BC, na.rm = TRUE),
    PresionBaro_avg = mean(PresionBaro, na.rm = TRUE)
  )

PuenteAranda_Por_Dias <- PuenteAranda_Por_Dias %>%
  left_join(promedios_diarios, by = c("myday" = "Dia")) %>%
  mutate(
    pm10 = if_else(is.na(pm10), pm10_avg, pm10),
    tmp = if_else(is.na(tmp), tmp_avg, tmp),
    ws = if_else(is.na(ws), ws_avg, ws),
    rh = if_else(is.na(rh), rh_avg, rh),
    rain = if_else(is.na(rain), rain_avg, rain),
    radsolar = if_else(is.na(radsolar), radsolar_avg, radsolar),
    o3 = if_else(is.na(o3), o3_avg, o3),
    no2 = if_else(is.na(no2), no2_avg, no2),
    pm25 = if_else(is.na(pm25), pm25_avg, pm25),
    co = if_else(is.na(co), co_avg, co),
    so2 = if_else(is.na(so2), so2_avg, so2),
    bc = if_else(is.na(bc), bc_avg, bc),
    pressure = if_else(is.na(pressure), PresionBaro_avg, pressure)
  ) %>%
  select(-ends_with("_avg"))  # Elimina las columnas de promedios temporales

summary(PuenteAranda_Por_Dias)

# Guardar como archivo .RDS:
saveRDS(PuenteAranda_Por_Dias, "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/1. Datos/3.12 PuenteAranda_Por_Dias.rds")

#-------------------------------------------------------------------------------

## Centro de Alto Rendimiento

# Importar datos
Centro_Por_Horas <- read_excel("1. Datos (2021 -2024) - Por Horas.xlsx", 
                                sheet = "Centro de Alto Rendimiento")

Centro_Por_Dias <- read_excel("2. Datos (2021 -2024) - Diarios.xlsx")

# Calculo de promedios
promedios_diarios <- Centro_Por_Horas %>%
  group_by(Dia) %>%
  summarise(
    pm10_avg = mean(PM10, na.rm = TRUE),
    tmp_avg = mean(Temperatura, na.rm = TRUE),
    ws_avg = mean(VelViento, na.rm = TRUE),
    rh_avg = mean(HR, na.rm = TRUE),
    rain_avg = mean(Precipitacion, na.rm = TRUE),
    radsolar_avg = mean(RadSolar, na.rm = TRUE),
    o3_avg = mean(OZONO, na.rm = TRUE),
    no2_avg = mean(NO2, na.rm = TRUE),
    pm25_avg = mean(PM25, na.rm = TRUE),
    co_avg = mean(CO, na.rm = TRUE),
    so2_avg = mean(SO2, na.rm = TRUE),
    bc_avg = mean(BC, na.rm = TRUE),
    co2_avg = mean(CO2, na.rm = TRUE)
  )

Centro_Por_Dias <- Centro_Por_Dias %>%
  left_join(promedios_diarios, by = c("myday" = "Dia")) %>%
  mutate(
    pm10 = if_else(is.na(pm10), pm10_avg, pm10),
    tmp = if_else(is.na(tmp), tmp_avg, tmp),
    ws = if_else(is.na(ws), ws_avg, ws),
    rh = if_else(is.na(rh), rh_avg, rh),
    rain = if_else(is.na(rain), rain_avg, rain),
    radsolar = if_else(is.na(radsolar), radsolar_avg, radsolar),
    o3 = if_else(is.na(o3), o3_avg, o3),
    no2 = if_else(is.na(no2), no2_avg, no2),
    pm25 = if_else(is.na(pm25), pm25_avg, pm25),
    co = if_else(is.na(co), co_avg, co),
    so2 = if_else(is.na(so2), so2_avg, so2),
    bc = if_else(is.na(bc), bc_avg, bc),
    co2 = if_else(is.na(co2), co2_avg, co2)
  ) %>%
  select(-ends_with("_avg"))  # Elimina las columnas de promedios temporales

summary(Centro_Por_Dias)

# Guardar como archivo .RDS:
saveRDS(Centro_Por_Dias, "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/1. Datos/3.13Centro_Por_Dias.rds")

# -----------------------------------------------------------
# Sección 1: Descarga de las medias de los contaminates 
# -----------------------------------------------------------

library(dplyr)
library(openxlsx)


# Asumiendo que las variables resumen ya están calculadas y almacenadas en dataframes
# Como ejemplo, usaremos summary() directamente sobre los datos procesados
summary_guaymaral <- summary(Guaymaral_Por_Dias)
summary_minambiente <- summary(MinAmbiente_Por_Dias)
summary_suba <- summary(Suba_Por_Dias)
summary_usaquen <- summary(Usaquen_Por_Dias)
summary_ferias <- summary(Ferias_Por_Dias)
summary_SanCristobal <- summary(SanCristobal_Por_Dias)
summary_Tunal <- summary(Tunal_Por_Dias)
summary_Bolivia <- summary(Bolivia_Por_Dias)
summary_Carvajal <- summary(Carvajal_Por_Dias)
summary_Fontibon <- summary(Fontibon_Por_Dias)
summary_Kennedy <- summary(Kennedy_Por_Dias)
summary_PuenteAranda <- summary(PuenteAranda_Por_Dias)
summary_Centro <- summary(Centro_Por_Dias)

# Convertir los resúmenes en dataframes (ajustar según lo necesario)
df_guaymaral <- as.data.frame(t(summary_guaymaral))
df_minambiente <- as.data.frame(t(summary_minambiente))
df_suba <- as.data.frame(t(summary_suba))
df_usaquen <- as.data.frame(t(summary_usaquen))
df_Ferias <- as.data.frame(t(summary_ferias))
df_SanCristobal <- as.data.frame(t(summary_SanCristobal))
df_Tunal <- as.data.frame(t(summary_Tunal))
df_Bolivia <- as.data.frame(t(summary_Bolivia))
df_Carvajal <- as.data.frame(t(summary_Carvajal))
df_Fontibon <- as.data.frame(t(summary_Fontibon))
df_Kennedy <- as.data.frame(t(summary_Kennedy))
df_PuenteAranda <- as.data.frame(t(summary_PuenteAranda))
df_Centro <- as.data.frame(t(summary_Centro))

# Añadir columna para identificar la fuente
df_guaymaral$Fuente <- "Guaymaral"
df_minambiente$Fuente <- "MinAmbiente"
df_suba$Fuente <- "Suba"
df_usaquen$Fuente <- "Usaquen"
df_Ferias$Fuente <- "Ferias"
df_SanCristobal$Fuente <- "SanCristobal"
df_Tunal$Fuente <- "Tunal"
df_Bolivia$Fuente <- "Bolivia"
df_Carvajal$Fuente <- "Carvajal"
df_Fontibon$Fuente <- "Fontibon"
df_Kennedy$Fuente <- "Kennedy"
df_PuenteAranda$Fuente <- "PuenteAranda"
df_Centro$Fuente <- "Centro"

df_suba$Fuente <- "Suba"# Combinar los dataframes
df_total <- bind_rows(df_guaymaral, df_minambiente, df_suba,df_usaquen, df_Ferias, df_SanCristobal, df_Tunal, df_Bolivia, df_Carvajal, df_Fontibon, df_Kennedy, df_PuenteAranda, df_Centro)

# Ruta de guardado
ruta_guardado <- "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/1. Datos/4. Estadisticas_Descriptivas.xlsx"

# Exportar a Excel
write.xlsx(df_total, ruta_guardado)
