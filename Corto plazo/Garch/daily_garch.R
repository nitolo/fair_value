#######################################################
################ RECOLECCIÓN DE DATOS     #############
################ MODELOS BEER DAILY       #############
#######################################################
cat("\014") # Limpiar la consola
while(dev.cur() > 1) dev.off() # Limpiar todos los gráficos
rm(list = ls()) # Limpiar el entorno global

library(openxlsx)
library(dplyr)
library(janitor)
library(lubridate)

procesar_datos <- function(ruta, libro, hoja) {
  # Leer los datos desde el archivo Excel
  df <- read.xlsx(paste0(ruta, libro), sheet = hoja, startRow = 3, colNames = TRUE)
  glimpse(df)
  
  # Leer los nombres de las columnas desde el archivo Excel
  df_names <- read.xlsx(paste0(ruta, libro), sheet = hoja, startRow = 2, colNames = FALSE)
  glimpse(df_names)
  
  # Filtrar solo la primera fila para obtener los nombres de las columnas
  df_names <- df_names[1, ] %>% as.character()
  
  # Asignar los nombres de las columnas al dataframe
  colnames(df) <- df_names
  glimpse(df)
  
  # Convertir la primera columna a formato Date
  if (is.numeric(df[, 1])) {
    df[, 1] <- as.Date(df[, 1], origin = "1899-12-30")
  } else {
    df[, 1] <- as.Date(df[, 1])
  }
  glimpse(df)
  
  # Limpiar los nombres de las columnas
  df <- df %>% clean_names()
  glimpse(df)
  
  # Añadir columnas de año, mes, día y día de la semana
  df <- df %>% 
    mutate(year = year(date),
           month = month(date),
           day = day(date),
           weekday = wday(date))
  
  glimpse(df)
  return(df)
}

# Inputs
ruta <- "Z:/03_Investigaciones_Economicas/3. Modelos/4. Regressions/FX EQUILIBRIUM/"
libro <- "FX_BEER.xlsx"
hoja <- "Data"

# DF definitivo
df <- procesar_datos(ruta, libro, hoja)

library(rugarch)



