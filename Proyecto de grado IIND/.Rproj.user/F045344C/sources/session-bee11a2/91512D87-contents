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
library(forecast)
library(tseries)

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
data2 <- import("4. Estaciones_Por_Dias_CO2.xlsx")
data3 <- import("4. Estaciones_Por_Dias_BC.xlsx")

modelo1 <- auto.arima(data$pm25)
modelo1

skim(data)
glimpse(data)
stargazer(data, 
          type = "text",
          title = "Estadisticas Descriptivas")

## Valores atipicos CO
# Calcula el promedio de la variable 'co', excluyendo el valor máximo
media_sin_max <- mean(data$co[data$co != max(data$co)])

# Reemplaza el valor máximo por el promedio calculado
data$co[data$co == max(data$co)] <- media_sin_max

# Verificar los cambios
print(data)

# -----------------------------------------------------------
# Sección 2: Graficos y Análisis
# -----------------------------------------------------------

min(data$myday)
max(data$myday)

# Serie de tiempo CO
data <- data %>% mutate(co_new = (co*28.01*101.325)/(8.314*298.15))
p_co <- ggplot(data, aes(myday, co_new)) +
  geom_line() +
  labs(x = "Tiempo", 
       y = "CO",
       title = "Análisis del CO desde 2021 hasta 2024") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centra y ajusta el título
    axis.title = element_text(size = 14),  # Ajusta el tamaño de las etiquetas de los ejes
    axis.text = element_text(size = 12),   # Ajusta el tamaño del texto de los ejes
    plot.background = element_rect(fill = "white"),  # Cambia el fondo del gráfico
    panel.grid.minor = element_blank()  # Elimina las líneas de cuadrícula menores
  )
p_co

p_co + geom_hline(aes(yintercept = 4), data, color = "red")

# Exportar como PNG
ggsave("1. Análisis del CO desde 2021 hasta 2024.png", 
       plot = last_plot(), 
       path = "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/4. Gráficos", 
       width = 10, height = 6, units = "in", dpi = 300)


# Serie de tiempo NO2
data <- data %>% mutate(no2_new = ((no2/(10^9))*(1000)*(1/((1*0.082*(tmp+273.15))/(pressure/760)))*(46.0055/1)*((10^6)/1)))
p_no2 <- ggplot(data, aes(myday, no2_new)) +
  geom_line() +
  labs(x = "Tiempo", 
       y = "NO2",
       title = "Análisis del NO2 desde 2021 hasta 2024") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centra y ajusta el título
    axis.title = element_text(size = 14),  # Ajusta el tamaño de las etiquetas de los ejes
    axis.text = element_text(size = 12),   # Ajusta el tamaño del texto de los ejes
    plot.background = element_rect(fill = "white"),  # Cambia el fondo del gráfico
    panel.grid.minor = element_blank()  # Elimina las líneas de cuadrícula menores
  )
p_no2

p_no2 + geom_hline(aes(yintercept = 25), data, color = "red")

# Exportar como PNG
ggsave("2. Análisis del NO2 desde 2021 hasta 2024.png", 
       plot = last_plot(), 
       path = "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/4. Gráficos", 
       width = 10, height = 6, units = "in", dpi = 300)

# Serie de tiempo PM2.5
p_pm25 <- ggplot(data, aes(myday, pm25)) +
  geom_line() +
  labs(x = "Tiempo", 
       y = "PM2.5",
       title = "Análisis del PM2.5 desde 2021 hasta 2024") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centra y ajusta el título
    axis.title = element_text(size = 14),  # Ajusta el tamaño de las etiquetas de los ejes
    axis.text = element_text(size = 12),   # Ajusta el tamaño del texto de los ejes
    plot.background = element_rect(fill = "white"),  # Cambia el fondo del gráfico
    panel.grid.minor = element_blank()  # Elimina las líneas de cuadrícula menores
  )
p_pm25
p_pm25 + geom_hline(aes(yintercept = 15), data, color = "red")


library(tseries)
library(forecast)
modelo1 <- auto.arima(pm25)
# mstl descomponer la serie,  

# Exportar como PNG
ggsave("3. Análisis del PM2.5 desde 2021 hasta 2024.png", 
       plot = last_plot(),
       path = "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/4. Gráficos", 
       width = 10, height = 6, units = "in", dpi = 300)

# Crear series de tiempo
ts_pm25 <- ts(data$pm25, frequency = 365)


# Análisis de estacionalidad usando ACF y PACF para PM2.5
acf_pm25 <- Acf(ts_pm25, main="Autocorrelación PM2.5")
pacf_pm25 <- Pacf(ts_pm25, main="Autocorrelación Parcial PM2.5")

# Descomposición estacional para visualizar tendencias y estacionalidad
decomp_pm25 <- stl(ts_pm25, s.window = "periodic")
plot(decomp_pm25, main = "Descomposición de PM2.5")

# Serie de tiempo PM10
p_pm10 <- ggplot(data, aes(myday, pm10)) +
  geom_line() +
  labs(x = "Tiempo", 
       y = "PM10",
       title = "Análisis del PM10 desde 2021 hasta 2024") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centra y ajusta el título
    axis.title = element_text(size = 14),  # Ajusta el tamaño de las etiquetas de los ejes
    axis.text = element_text(size = 12),   # Ajusta el tamaño del texto de los ejes
    plot.background = element_rect(fill = "white"),  # Cambia el fondo del gráfico
    panel.grid.minor = element_blank()  # Elimina las líneas de cuadrícula menores
  )
p_pm10

p_pm10 + geom_hline(aes(yintercept = 45), data, color = "red")

# Exportar como PNG
ggsave("4. Análisis del PM10 desde 2021 hasta 2024.png", 
       plot = last_plot(),
       path = "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/4. Gráficos", 
       width = 10, height = 6, units = "in", dpi = 300)

# Serie de tiempo SO2
data <- data %>% mutate(so2_new = ((so2/(10^9))*(1000)*(1/((1*0.082*(tmp+273.15))/0.74289484))*(64.066/1)*((10^6)/1)))
p_so2 <- ggplot(data, aes(myday, so2_new)) +
  geom_line() +
  labs(x = "Tiempo", 
       y = "SO2",
       title = "Análisis del SO2 desde 2021 hasta 2024") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centra y ajusta el título
    axis.title = element_text(size = 14),  # Ajusta el tamaño de las etiquetas de los ejes
    axis.text = element_text(size = 12),   # Ajusta el tamaño del texto de los ejes
    plot.background = element_rect(fill = "white"),  # Cambia el fondo del gráfico
    panel.grid.minor = element_blank()  # Elimina las líneas de cuadrícula menores
  )
p_so2

p_so2 + geom_hline(aes(yintercept = 40), data, color = "red")

# Exportar como PNG
ggsave("5. Análisis del SO2 desde 2021 hasta 2024.png", 
       plot = last_plot(),
       path = "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/4. Gráficos", 
       width = 10, height = 6, units = "in", dpi = 300)

# Serie de tiempo 03
data <- data %>% mutate(o3_new = ((o3/(10^9))*(1000)*(1/((1*0.082*(tmp+273.15))/0.74289484))*(48/1)*((10^6)/1)))
p_o3 <- ggplot(data, aes(myday, o3_new)) +
  geom_line() +
  labs(x = "Tiempo", 
       y = "O3",
       title = "Análisis del O3 desde 2021 hasta 2024") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centra y ajusta el título
    axis.title = element_text(size = 14),  # Ajusta el tamaño de las etiquetas de los ejes
    axis.text = element_text(size = 12),   # Ajusta el tamaño del texto de los ejes
    plot.background = element_rect(fill = "white"),  # Cambia el fondo del gráfico
    panel.grid.minor = element_blank()  # Elimina las líneas de cuadrícula menores
  )
p_o3

p_o3 + geom_hline(aes(yintercept = 100*(24/8)), data, color = "red")

# Exportar como PNG
ggsave("6. Análisis del O3 desde 2021 hasta 2024.png", 
       plot = last_plot(),
       path = "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/4. Gráficos", 
       width = 10, height = 6, units = "in", dpi = 300)

# Serie de tiempo CO2
min(data2$myday)
max(data2$myday)

p_co2 <- ggplot(data2, aes(myday, co2)) +
  geom_line() +
  labs(x = "Tiempo", 
       y = "co2",
       title = "Análisis del CO2 desde 2021 hasta comienzos del 2022") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centra y ajusta el título
    axis.title = element_text(size = 14),  # Ajusta el tamaño de las etiquetas de los ejes
    axis.text = element_text(size = 12),   # Ajusta el tamaño del texto de los ejes
    plot.background = element_rect(fill = "white"),  # Cambia el fondo del gráfico
    panel.grid.minor = element_blank()  # Elimina las líneas de cuadrícula menores
  )
p_co2

p_co2 + geom_hline(aes(yintercept = 1000), data, color = "red")

# Exportar como PNG
ggsave("7. Análisis del CO2 desde 2021 hasta 2022.png", 
       plot = last_plot(),
       path = "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/4. Gráficos", 
       width = 10, height = 6, units = "in", dpi = 300)

# Serie de tiempo BC
min(data3$myday)
max(data3$myday)

p_bc <- ggplot(data3, aes(myday, bc)) +
  geom_line() +
  labs(x = "Tiempo", 
       y = "BC",
       title = "Análisis del BC desde mediados del 2023 hasta comienzos del 2024") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centra y ajusta el título
    axis.title = element_text(size = 14),  # Ajusta el tamaño de las etiquetas de los ejes
    axis.text = element_text(size = 12),   # Ajusta el tamaño del texto de los ejes
    plot.background = element_rect(fill = "white"),  # Cambia el fondo del gráfico
    panel.grid.minor = element_blank()  # Elimina las líneas de cuadrícula menores
  )
p_bc

# Exportar como PNG
ggsave("8. Análisis del BC desde 2023 hasta 2024.png", 
       plot = last_plot(),
       path = "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/4. Gráficos", 
       width = 10, height = 6, units = "in", dpi = 300)
