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
head(data)
# -----------------------------------------------------------
# Sección 2: Modelo Probit PM2.5
# -----------------------------------------------------------

## Variable de excesos y modelación binaria
# Crear variables de los excedentes
data <- data %>% mutate(ex_pm = case_when(pm25 >= 15 ~ 1,
                                          .default = 0))

# Estimacion de la probabilidad de exceder el AGQ - Modelo PROBIT
# Variable dependiente: ex_pm
# as.factor(X): Definir variable como una categorica
probit_model <- glm(formula = ex_pm ~ rain + tmp + ws + rh + radsolar + as.factor(dow) + as.factor(month) + as.factor(year),
                    data = data, 
                    family = binomial(link = "probit")) 

# Chequeo de los resultados
stargazer(probit_model, type = "text", out = "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/5. Salidas de R/probit_model.doc")

# Pseudo R^2
pR2(probit_model)

# Valor de la función de log-verosimilitud
logLik(probit_model)

# Desvianza residual
deviance(probit_model)

# Modelo nulo
probit_model0 <- glm(formula = ex_pm ~ 1, data = data, family = binomial(link = "probit"))

# Valor de la función de log-verosimilitud
loglik_model <- logLik(probit_model)

# Estadístico de razón de verosimilitud
lrtest <- 2 * (logLik(probit_model) - logLik(probit_model0))
lrtest

# Pseudo R^2 de McFadden
pR2(probit_model, method = "mcfadden")

# AIC
AIC(probit_model)

# BIC
BIC(probit_model)

# Valores predichos en la base de datos
data <- data %>% mutate(ex_pm_hat = probit_model$fitted.values)

#view(data)

p_pm25_prob <- ggplot(data, aes(myday, ex_pm_hat)) +
  geom_line() +
  labs(x = "Tiempo", 
       y = "Probabilidad",
       title = "Evoluación de la probabilidad") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centra y ajusta el título
    axis.title = element_text(size = 14),  # Ajusta el tamaño de las etiquetas de los ejes
    axis.text = element_text(size = 12),   # Ajusta el tamaño del texto de los ejes
    plot.background = element_rect(fill = "white"),  # Cambia el fondo del gráfico
    panel.grid.minor = element_blank()  # Elimina las líneas de cuadrícula menores
  )
p_pm25_prob

# Exportar como PNG
ggsave("9. Evoluación de la probabilidad PM2.5.png", 
       plot = last_plot(),
       path = "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/4. Gráficos", 
       width = 10, height = 6, units = "in", dpi = 300)

p_pm25_prob2 <- ggplot(data, aes(ws, ex_pm_hat)) +
  geom_point() +
  labs(x = "Velocidad del Viento", 
       y = "Probabilidad",
       title = "Evoluación de la probabilidad con Viento - PM2.5") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centra y ajusta el título
    axis.title = element_text(size = 14),  # Ajusta el tamaño de las etiquetas de los ejes
    axis.text = element_text(size = 12),   # Ajusta el tamaño del texto de los ejes
    plot.background = element_rect(fill = "white"),  # Cambia el fondo del gráfico
    panel.grid.minor = element_blank()  # Elimina las líneas de cuadrícula menores
  )
p_pm25_prob2

# Exportar como PNG
ggsave("10. Evoluación de la probabilidad con Viento - PM2.5.png", 
       plot = last_plot(),
       path = "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/4. Gráficos", 
       width = 10, height = 6, units = "in", dpi = 300)


## Manejo de la variable de velocidad del viento ------------
max(data$ws)
min(data$ws)

data <- data %>% mutate(ws_class = case_when(ws < 0.1 ~ 0,
                                             ws >= 0.1 & ws < 0.3 ~ 0.2,
                                             ws >= 0.3 & ws < 0.5 ~ 0.4,
                                             ws >= 0.5 & ws < 0.7 ~ 0.6,
                                             ws >= 0.7 & ws < 0.9 ~ 0.8,
                                             ws >= 0.9 & ws < 1.1 ~ 1.0,
                                             ws >= 1.1 & ws < 1.3 ~ 1.2,
                                             ws >= 1.3 & ws < 1.5 ~ 1.4,
                                             ws >= 1.5 & ws < 1.7 ~ 1.6,
                                             ws >= 1.7 & ws < 1.9 ~ 1.8,
                                             ws >= 1.9 & ws < 2.1 ~ 2.0,
                                             ws >= 2.1 & ws < 2.3 ~ 2.2,
                                             ws >= 2.3 & ws < 2.5 ~ 2.4,
                                             ws >= 2.5 & ws < 2.7 ~ 2.6,
                                             ws >= 2.7 & ws < 2.9 ~ 2.8,
                                             ws >= 2.9 & ws < 3.1 ~ 3.0))


## Funcion Group_by() con summarise()
data_ws_col <- data %>% group_by(ws_class) %>% 
  summarise(mean_prob = mean(ex_pm_hat)) %>% 
  as.tibble()

p_pm25_col <- ggplot(data_ws_col, aes(ws_class, mean_prob)) +
  geom_point() +
  geom_line() +
  labs(x = "Velocidad del Viento", 
       y = "Probabilidad",
       title = "Evoluación de la probabilidad con Viento - PM2.5") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centra y ajusta el título
    axis.title = element_text(size = 14),  # Ajusta el tamaño de las etiquetas de los ejes
    axis.text = element_text(size = 12),   # Ajusta el tamaño del texto de los ejes
    plot.background = element_rect(fill = "white"),  # Cambia el fondo del gráfico
    panel.grid.minor = element_blank()  # Elimina las líneas de cuadrícula menores
  )
p_pm25_col

# Exportar como PNG
ggsave("11. Evoluación de la probabilidad con Viento - PM2.5.png", 
       plot = last_plot(),
       path = "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/4. Gráficos", 
       width = 10, height = 6, units = "in", dpi = 300)

data_ws_col2 <- data %>% group_by(ws_class, month) %>% 
  summarise(mean_prob = mean(ex_pm_hat)) %>% 
  as.tibble()
data_ws_col2 <- data_ws_col2 %>% mutate(month =  as.character(month))

p_pm25_col2 <- ggplot(data_ws_col2, aes(ws_class, mean_prob, col = month)) +
  geom_point() +
  geom_line() +
  labs(x = "Velocidad del Viento", 
       y = "Probabilidad",
       title = "Evoluación de la probabilidad con Viento") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centra y ajusta el título
    axis.title = element_text(size = 14),  # Ajusta el tamaño de las etiquetas de los ejes
    axis.text = element_text(size = 12),   # Ajusta el tamaño del texto de los ejes
    plot.background = element_rect(fill = "white"),  # Cambia el fondo del gráfico
    panel.grid.minor = element_blank()  # Elimina las líneas de cuadrícula menores
  )
p_pm25_col2

# Exportar como PNG
ggsave("12. Evoluación de la probabilidad con Viento - PM2.5.png", 
       plot = last_plot(),
       path = "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/4. Gráficos", 
       width = 10, height = 6, units = "in", dpi = 300)


# -----------------------------------------------------------
# Sección 3: Modelo Probit PM10
# -----------------------------------------------------------

## Variable de excesos y modelación binaria
# Crear variables de los excedentes
data <- data %>% mutate(no2_new = ((no2/(10^9))*(1000)*(1/((1*0.082*(tmp+273.15))/(pressure/760)))*(46.0055/1)*((10^6)/1)))
data <- data %>% mutate(ex_no2 = case_when(no2_new >= 25 ~ 1,
                                          .default = 0))

# Estimacion de la probabilidad de exceder el AGQ - Modelo PROBIT
# Variable dependiente: ex_pm
# as.factor(X): Definir variable como una categorica
probit_model <- glm(formula = ex_no2 ~ rain + tmp + ws + rh + radsolar + as.factor(dow) + as.factor(month) + as.factor(year),
                    data = data, 
                    family = binomial(link = "probit")) 

# Chequeo de los resultados
stargazer(probit_model, type = "text", out = "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/5. Salidas de R/probit_model no2.doc")

# Pseudo R^2
pR2(probit_model)

# Valor de la función de log-verosimilitud
logLik(probit_model)

# Desvianza residual
deviance(probit_model)

# Modelo nulo
probit_model0 <- glm(formula = ex_no2 ~ 1, data = data, family = binomial(link = "probit"))

# Valor de la función de log-verosimilitud
loglik_model <- logLik(probit_model)

# Estadístico de razón de verosimilitud
lrtest <- 2 * (logLik(probit_model) - logLik(probit_model0))
lrtest

# Pseudo R^2 de McFadden
pR2(probit_model, method = "mcfadden")

# AIC
AIC(probit_model)

# BIC
BIC(probit_model)

# Valores predichos en la base de datos
data <- data %>% mutate(ex_no2_hat = probit_model$fitted.values)

#view(data)

p_no2_prob <- ggplot(data, aes(myday, ex_no2_hat)) +
  geom_line() +
  labs(x = "Tiempo", 
       y = "Probabilidad",
       title = "Evoluación de la probabilidad") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centra y ajusta el título
    axis.title = element_text(size = 14),  # Ajusta el tamaño de las etiquetas de los ejes
    axis.text = element_text(size = 12),   # Ajusta el tamaño del texto de los ejes
    plot.background = element_rect(fill = "white"),  # Cambia el fondo del gráfico
    panel.grid.minor = element_blank()  # Elimina las líneas de cuadrícula menores
  )
p_no2_prob

  # Exportar como PNG
ggsave("13. Evoluación de la probabilidad no2.png", 
       plot = last_plot(),
       path = "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/4. Gráficos", 
       width = 10, height = 6, units = "in", dpi = 300)

p_no2_prob2 <- ggplot(data, aes(ws, ex_no2_hat)) +
  geom_point() +
  labs(x = "Velocidad del Viento", 
       y = "Probabilidad",
       title = "Evoluación de la probabilidad con Viento - no2") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centra y ajusta el título
    axis.title = element_text(size = 14),  # Ajusta el tamaño de las etiquetas de los ejes
    axis.text = element_text(size = 12),   # Ajusta el tamaño del texto de los ejes
    plot.background = element_rect(fill = "white"),  # Cambia el fondo del gráfico
    panel.grid.minor = element_blank()  # Elimina las líneas de cuadrícula menores
  )
p_no2_prob2

# Exportar como PNG
ggsave("14. Evoluación de la probabilidad con Viento - no2.png", 
       plot = last_plot(),
       path = "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/4. Gráficos", 
       width = 10, height = 6, units = "in", dpi = 300)


## Manejo de la variable de velocidad del viento ------------
max(data$ws)
min(data$ws)

data <- data %>% mutate(ws_class = case_when(ws < 0.1 ~ 0,
                                             ws >= 0.1 & ws < 0.3 ~ 0.2,
                                             ws >= 0.3 & ws < 0.5 ~ 0.4,
                                             ws >= 0.5 & ws < 0.7 ~ 0.6,
                                             ws >= 0.7 & ws < 0.9 ~ 0.8,
                                             ws >= 0.9 & ws < 1.1 ~ 1.0,
                                             ws >= 1.1 & ws < 1.3 ~ 1.2,
                                             ws >= 1.3 & ws < 1.5 ~ 1.4,
                                             ws >= 1.5 & ws < 1.7 ~ 1.6,
                                             ws >= 1.7 & ws < 1.9 ~ 1.8,
                                             ws >= 1.9 & ws < 2.1 ~ 2.0,
                                             ws >= 2.1 & ws < 2.3 ~ 2.2,
                                             ws >= 2.3 & ws < 2.5 ~ 2.4,
                                             ws >= 2.5 & ws < 2.7 ~ 2.6,
                                             ws >= 2.7 & ws < 2.9 ~ 2.8,
                                             ws >= 2.9 & ws < 3.1 ~ 3.0))


## Funcion Group_by() con summarise()
data_ws_col <- data %>% group_by(ws_class) %>% 
  summarise(mean_prob = mean(ex_no2_hat)) %>% 
  as.tibble()

p_no2_col <- ggplot(data_ws_col, aes(ws_class, mean_prob)) +
  geom_point() +
  geom_line() +
  labs(x = "Velocidad del Viento", 
       y = "Probabilidad",
       title = "Evoluación de la probabilidad con Viento - no2") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centra y ajusta el título
    axis.title = element_text(size = 14),  # Ajusta el tamaño de las etiquetas de los ejes
    axis.text = element_text(size = 12),   # Ajusta el tamaño del texto de los ejes
    plot.background = element_rect(fill = "white"),  # Cambia el fondo del gráfico
    panel.grid.minor = element_blank()  # Elimina las líneas de cuadrícula menores
  )
p_no2_col

# Exportar como PNG
ggsave("15. Evoluación de la probabilidad con Viento - no2.png", 
       plot = last_plot(),
       path = "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/4. Gráficos", 
       width = 10, height = 6, units = "in", dpi = 300)

data_ws_col2 <- data %>% group_by(ws_class, month) %>% 
  summarise(mean_prob = mean(ex_no2_hat)) %>% 
  as.tibble()
data_ws_col2 <- data_ws_col2 %>% mutate(month =  as.character(month))

p_no2_col2 <- ggplot(data_ws_col2, aes(ws_class, mean_prob, col = month)) +
  geom_point() +
  geom_line() +
  labs(x = "Velocidad del Viento", 
       y = "Probabilidad",
       title = "Evoluación de la probabilidad con Viento") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centra y ajusta el título
    axis.title = element_text(size = 14),  # Ajusta el tamaño de las etiquetas de los ejes
    axis.text = element_text(size = 12),   # Ajusta el tamaño del texto de los ejes
    plot.background = element_rect(fill = "white"),  # Cambia el fondo del gráfico
    panel.grid.minor = element_blank()  # Elimina las líneas de cuadrícula menores
  )
p_no2_col2

# Exportar como PNG
ggsave("16. Evoluación de la probabilidad con Viento - no2.png", 
       plot = last_plot(),
       path = "C:/Users/windows/Documents/GitHub/Problem_Set_1/Proyecto-de-grado-IIND/Proyecto de grado IIND/4. Gráficos", 
       width = 10, height = 6, units = "in", dpi = 300)







