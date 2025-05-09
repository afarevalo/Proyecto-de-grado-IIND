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
aqli <- import("10. Mapa_aqli_PM25.xlsx")

# -----------------------------------------------------------
# Sección 2: Mapa
# -----------------------------------------------------------

world_coordinates <- map_data("world")

# Mapa World con Aqli más bonito y con colores
mymap <- ggplot(aqli) +
  geom_map(data = world_coordinates, map = world_coordinates,
           aes(map_id = region)) + 
  geom_map(map=world_coordinates, 
           aes(map_id=name, fill=pm25), color="black") +
  expand_limits(x = world_coordinates$long, y = world_coordinates$lat) +
  labs(title = "Mapa Global de Concentraciones de PM2.5 - 2023",
       x = "Longitud",
       y = "Latitud",
       caption = "Datos de Word Air Quality Report") + 
  scale_fill_continuous(type = "viridis") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centra y ajusta el título
    axis.title = element_text(size = 14),  # Ajusta el tamaño de las etiquetas de los ejes
    axis.text = element_text(size = 12),   # Ajusta el tamaño del texto de los ejes
  )

mymap

# Exportar como PNG
#ggsave("C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/4. Gráficos/32. Mapa Global de Concentraciones de PM2.5 - 2023.png", plot = mymap, width = 10, height = 8, units = "in")

