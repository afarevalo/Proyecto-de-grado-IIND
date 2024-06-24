# -----------------------------------------------------------
# Sección 1: Preparación de datos
# -----------------------------------------------------------

# Limpiar el entorno
rm(list = ls())

library(rio)
library(sf)
library(ggplot2)
library(pacman)
library(tidyverse)
library(rio)
library(viridis)
library(leaflet)
library(tmaptools)
library(tidycensus)
library(dplyr)
library(sf)
library(ggplot2)
library(readxl)
library(summarytools)
library(officer)
library(plotly)
library(tmaptools)
library(osmdata)
library(tidymodels)

# Manejo del directorio
getwd()
directorio <- "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/1. Datos"

setwd(directorio)

# Chequeo de los archivos del directorio
dir()
list.files()

## Importacion de los datos ------------------
install_formats() # Cuestiones de importacion de archivos del paquete rio
db <- import("9. Mapa_R.xlsx")

# -----------------------------------------------------------
# Sección 2: Mapa
# -----------------------------------------------------------

# Observamos la primera visualización
leaflet() %>%
  addTiles() %>%
  addCircles(lng = db$lng, 
             lat = db$lat)

bog<- st_as_sf(db, coords = c("lng", "lat"),  crs = 4626)
opq(bbox = getbb("Bogotá Colombia"))

 

datos_poligonos <- st_buffer(st_as_sf(bog$geometry), 0)

