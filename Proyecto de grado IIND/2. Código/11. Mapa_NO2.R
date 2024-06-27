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
library(mapview)
library(webshot)
library(RColorBrewer)

# Manejo del directorio
getwd()
directorio <- "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/1. Datos"

setwd(directorio)

# Chequeo de los archivos del directorio
dir()
list.files()

## Importacion de los datos ------------------
install_formats() # Cuestiones de importacion de archivos del paquete rio
db <- import("9. Mapa_R_NO2.xlsx")

# -----------------------------------------------------------
# Sección 2: Mapa
# -----------------------------------------------------------

# Encontramos el queremos que sea el centro del mapa 
latitud_central <- mean(db$lat)
longitud_central <- mean(db$lng)

# Observamos la primera visualización
mymap <- leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addCircles(lng = db$lng, 
             lat = db$lat,
             weight = 2,           # Grosor del borde
             color = "#000000",    # Color del borde
             fillColor = "#0000CD",# color del relleno
             radius = 500,
             fillOpacity = 1,
             opacity = 1)         # Tanaño de relleno
mymap
# Guardar el mapa como PNG
#mapshot(mymap, file = "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/4. Gráficos/36.Mapa_Estaciones.png", quietly = TRUE)


# Escalas de colores.
db <- db %>%
  mutate(color = case_when(puesto == "1" ~ "#FF0000",
                           puesto == "2" ~ "#FF2900",
                           puesto == "3" ~ "#FF2900",
                           puesto == "4" ~ "#FF7B00",
                           puesto == "5" ~ "#FFA500",
                           puesto == "6" ~ "#FFB800",
                           puesto == "7" ~ "#EED200",
                           puesto == "8" ~ "#FFE800",
                           puesto == "9" ~ "#FFFF00",
                           puesto == "10" ~ "#BFFF00",
                           puesto == "11" ~ "#7FFF00",
                           puesto == "12" ~ "#3FFF00",
                           puesto == "13" ~ "#00FF00"
                           ))

# Creamos el plot
mymap <-leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addCircles(lng = db$lng, 
             lat = db$lat, 
             color = "#000000",       # Aplicamos el color del borde
             fillColor = db$color,    # Aplicamos el color de relleno
             fillOpacity = 1,
             weight = 2,              # Definimos el grosor del borde
             opacity = 1,
             radius = 700)
mymap

# Guardar el mapa como PNG
#mapshot(mymap, file = "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/4. Gráficos/33. Mapa_NO2.png", quietly = TRUE)

# -----------------------------------------------------------
# Sección 3: Colores
# -----------------------------------------------------------

# Cargar la librería necesaria
library(RColorBrewer)

# Crear la función de paleta de colores
color_palette <- colorRampPalette(c("green", "yellow", "orange", "red"))

# Generar los 13 colores
colors <- color_palette(13)

# Mostrar los colores
print(colors)


#install.packages("webshot")
#webshot::install_phantomjs()

