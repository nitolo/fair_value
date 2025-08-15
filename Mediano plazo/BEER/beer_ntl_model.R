#######################################################
################ RECOLECCIÓN DE DATOS #############
################ MODELOS  #############
#######################################################

cat("\014") # Limpiar la consola
while(dev.cur() > 1) dev.off() # Limpiar todos los gráficos
rm(list = ls()) # Limpiar el entorno global

library(openxlsx)
library(lubridate)
library(janitor)
library(fixest)
library(tidyverse)
library(zoo)

procesar_datos <- function(ruta, libro, hoja) {
  # Leer los datos desde el archivo Excel
  df <- read.xlsx(paste0(ruta, libro), sheet = hoja, startRow = 3, colNames = TRUE)
  glimpse(df)
  
  # Leer los nombres de las columnas desde el archivo Excel
  df_names <- read.xlsx(paste0(ruta, libro), sheet = hoja, startRow = 2, colNames = FALSE)
  
  # Filtrar solo la primera fila para obtener los nombres de las columnas
  df_names <- df_names[1, ] %>% as.character()
  
  # Asignar los nombres de las columnas al dataframe
  colnames(df) <- df_names
  
  # Convertir la primera columna a formato Date
  if (is.numeric(df[, 1])) {
    df[, 1] <- as.Date(df[, 1], origin = "1899-12-30")
  } else {
    df[, 1] <- as.Date(df[, 1])
  }
  glimpse(df)
  
  # Limpiar los nombres de las columnas
  df <- df %>% clean_names()
  
  # Añadir columnas de año, mes, día y día de la semana
  df <- df %>% 
    mutate(year = year(date),
           month = month(date),
           day = day(date),
           weekday = wday(date))
  
  glimpse(df)
  return(df)
}

procesar_modelos <- function(df, formula, filter_model_data, ruta, hoja
                             , start_year, end_year) {
  # Ajustar el modelo lineal
  m1 <- lm(formula, data = df, subset = year >= filter_model_data)
  summary(m1)
  
  # Guardar la data del modelo
  model_lm_data <- m1$model %>% tail(rango_datos)
  
  # Guardar las predicciones
  predictions <- predict(m1, interval = "prediction", level = 0.95) %>%
    as.data.frame() %>% tail(rango_datos)
  
  # Filtrar los datos de la misma forma para combinarlos
  df_date <- df %>% 
    filter(year >= filter_model_data) %>% 
    select(date) %>% tail(rango_datos)
  
  # Combinar las bases, primero fecha y luego la data
  model_data <- cbind(df_date, model_lm_data, 
                      fitted = predictions[, "fit"],
                      lowerci = predictions[, "lwr"],
                      upperci = predictions[, "upr"])
  
  # Hacer que todos los datos empiecen por 01/mes/year
  model_data <- model_data %>%
    mutate(date = make_date(year(date), month(date), 1)) %>%
    tail(rango_datos)
  glimpse(model_data)
  
  # Ajustar el modelo de efectos fijos
  m2 <- feols(formula, data = df, subset = df$year >= start_year 
              & df$year <= end_year, cluster = "month")
  
  # Guardar los coeficientes
  model_summary <- fixest::coeftable(m2, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10)) %>% 
    as.data.frame()
  
  # Guardar el R cuadrado
  r2 <- fixest::r2(m2, type = "r2") %>% as.data.frame()
  number_rows <- as.numeric(nrow(model_summary) + 2)
  
  # Mostrar los resultados
  etable(m2, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10))
  
  # Cargar el libro de Excel donde están los resultados
  wb <- loadWorkbook(paste0(ruta, "reer_models", ".xlsx"))
  
  # Escribir los datos de los coeficientes
  writeData(wb, hoja, model_summary, rowNames = TRUE)
  # Escribir el R cuadrado debajo de los coeficientes
  writeData(wb, hoja, r2, startRow = number_rows, colNames = FALSE, rowNames = TRUE)
  # Escribir los datos para hacer cálculos en Excel
  writeData(wb, hoja, model_data, startCol = 8)
  
  # Guardar
  saveWorkbook(wb, paste0(ruta, "reer_models", ".xlsx"), overwrite = TRUE)
  
  return(model_data)
}

# Cargar ruta generales 
ruta <- "Z:/03_Investigaciones_Economicas/3. Modelos/4. Regressions/REER/"
libro <- "reer_hispam.xlsx"
rango_datos <- 144 # son los ultimos datos que va a traer los archivos
filter_model_data <- 2009 # la fecha desde la que empieza el filtro

############
# Colombia #
############

hoja       <- "COP"
start_year <- 2009
end_year   <- 2024

# Procesar los datos
df <- procesar_datos(ruta, libro, hoja)

# Here se coloca el procesamiento de datos individual por country.
df <- df %>% 
  mutate(
    gdp_per = gdp/po,
    gdp_per_g = (gdp_per/lag(gdp_per, 12))-1,
    gdp_per_usa = gdp_usa/po_usa,
    gdp_per_usa_g = (gdp_per_usa/lag(gdp_per_usa, 12))-1,
    diff_gdp = gdp_per_g - gdp_per_usa_g,
    #diff_rates = (tpm-cpi) - (tpm_usa-cpi_usa),
    diff_rates = ((1+tpm) / (1+ tpm_usa))-1,
    diff_rates_exante = (tpm-expectation) - (tpm_usa-expectation_usa),
    brent_real = (brent/cpi_usa_index)*100,
    trade_balance = exp - imp,
    trade_balance_sum_12 = rollsum(trade_balance_fob, k = 12, fill = NA, align = "right"),
    trade_balance_mean_12 = rollmean(trade_balance_fob, k = 12, fill = NA, align = "right"),
    debt_real = debt/(cpi_usa_index/100),
    dummy = case_when(
      date >= "2008-01-01" & date <= "2009-12-01" ~ 1, # CRISIS DEL 2008
      date >= "2020-03-01" & date <= "2021-03-01" ~ 1, # COVID
      TRUE ~ 0
    )
  ) 

# Colocar formula

formula <- (log(reer) ~
            # Relative productivity
            + diff_gdp 
            # Terms of trade
            + log(brent)
            # Interest rate differentials
            #+ diff_rates_exante 
            + diff_rates
            # External account
            + trade_balance_sum_12
            # Exogenas
            + dummy 
)

# Ejecutar el modelo

# Supongamos que df y formula ya están definidos
model_data <- procesar_modelos(
    df = df
  , formula = formula
  , filter_model_data = filter_model_data
  , ruta = ruta
  , hoja = hoja
  , start_year = start_year
  , end_year = end_year
)


############
# Chile #
############

hoja       <- "CLP"
start_year <- 2010
end_year   <- 2024

# Procesar los datos
df <- procesar_datos(ruta, libro, hoja)



# Here se coloca el procesamiento de datos individual por country.
base_date = as.Date("2023-12-15")
# Encontrar los valores de cpi_index y cpi_usa_index al 31/12/2023
copper_index_2023 <- df$cobre[df$date == base_date]
brent_index_2023 <- df$brent[df$date == base_date]

# Rebasar las variables cpi_index y cpi_usa_index
df <- df %>%
  mutate(
    copper_index = (cobre / copper_index_2023)*100
    ,brent_index = (brent / brent_index_2023)*100
    , copper_real = (cobre/cpi_usa_index) *100
    , copper_index_real = (copper_index/cpi_usa_index) *100
    , brent_real = (brent/cpi_usa_index)*100
    , brent_index_real = (brent_index/cpi_usa_index)*100
    ,terms_commodities = (copper_index/brent_index)*100
    ,terms_commodities_real = (copper_index_real/brent_index_real)*100
  )

glimpse(df)
df <- df %>% 
  mutate(
    investment = investment*-1,
    gdp_per = gdp/po,
    gdp_per_g = (gdp_per/lag(gdp_per, 12))-1,
    gdp_per_usa = gdp_usa/po_usa,
    gdp_per_usa_g = (gdp_per_usa/lag(gdp_per_usa, 12))-1,
    #diff_gdp = gdp_per_g - gdp_per_usa_g,
    diff_gdp = ((1+gdp_per_g) / (1+gdp_per_usa_g))-1,
    #diff_rates = (tpm-cpi) - (tpm_usa-cpi_usa),
    diff_rates = ((1+tpm) / (1+ tpm_usa))-1,
    diff_rates_exante = (tpm-expectation) - (tpm_usa-expectation_usa),
    diff_rates_no = (tpm) - (tpm_usa),
    copper_real = (cobre/cpi_usa_index) *100,
    brent_real = (brent/cpi_usa_index)*100,
    debt_real = debt/(cpi_usa_index/100),
    current_real = current/(cpi_usa_index/100),
    investment_real = investment/(cpi_usa_index/100),
    basic_balance_real = current_real + (investment_real),
    basic_balance     = current + (investment),
    basic_balance_sum_12 = rollsum(basic_balance, k = 12, fill = NA, align = "right"),
    basic_balance_mean_12 = rollmean(basic_balance, k = 12, fill = NA, align = "right"),
    investment_sum_12 = rollsum(investment, k = 12, fill = NA, align = "right"),
    investment_mean_12 = rollmean(investment, k = 12, fill = NA, align = "right"),
    trade_balance = exp - imp,
    terms_real = (terms/cpi_index)*100,
    dummy = case_when(
      date >= "2008-01-01" & date <= "2009-12-01" ~ 1, # CRISIS DEL 2008
      date >= "2020-03-01" & date <= "2021-03-01" ~ 1, # COVID
      TRUE ~ 0
    ),
    dummy2 = case_when(
      year >= 2022 & year <= 2024 ~ 1, # CRISIS DEL 2008
      TRUE ~ 0)
  ) 


glimpse(df)

formula <- (log(reer) ~ 
              # Relative productivity
              + diff_gdp
            # Terms of trade
            #+ log(terms_real)
            #+ log(terms_commodities)
            + log(cobre)
            #+ log(terms)
            #+ log(copper_real)
            #+ log(brent)
            # Interest rate differentials
            + diff_rates
            #+ diff_rates_exante
            # External account
            + basic_balance_sum_12
            #+ basic_balance_mean_12
            #+ investment_mean_12
            + dummy 
            + dummy2
)

# Ejecutar el modelo

# Supongamos que df y formula ya están definidos
model_data <- procesar_modelos(
  df = df
  , formula = formula
  , filter_model_data = filter_model_data
  , ruta = ruta
  , hoja = hoja
  , start_year = start_year
  , end_year = end_year
)


############
# Peru     #
############

hoja       <- "PEN"
start_year <- 2013
end_year   <- 2025

# Procesar los datos
df <- procesar_datos(ruta, libro, hoja)

df <- df %>% 
  mutate(
    gdp_per = gdp/po,
    gdp_per_g = (gdp_per/lag(gdp_per, 1))-1,
    gdp_per_usa = gdp_usa/po_usa,
    gdp_per_usa_g = (gdp_per_usa/lag(gdp_per_usa, 12))-1,
    #diff_gdp = gdp_per_g - gdp_per_usa_g,
    diff_gdp = ((1+gdp_per_g) / (1+gdp_per_usa_g))-1,
    #diff_rates = (tpm-cpi) - (tpm_usa-cpi_usa),
    diff_rates = ((1+tpm) / (1+ tpm_usa))-1,
    exp_gdp = exp/gdp,
    imp_gdp = imp/gdp,
    debt_real = debt/(cpi_usa_index/100),
    current_real = current/(cpi_usa_index/100),
    current_sum_12 = rollsum(current, k = 12, fill = NA, align = "right"),
    current_mean_12 = rollmean(current, k = 12, fill = NA, align = "right"),
    #investment_sum_12 = rollsum(investment, k = 12, fill = NA, align = "right"),
    #investment_mean_12 = rollmean(investment, k = 12, fill = NA, align = "right"),
    copper_real = (cobre/cpi_usa_index)*100,
    diff_rates_exante = (tpm-expectation) - (tpm_usa-expectation_usa),
    terms_real = (terms/cpi_index)*100,
    trade_balance = exp-imp,
    dummy = case_when(
      date >= "2008-01-01" & date <= "2009-12-01" ~ 1, # CRISIS DEL 2008
      date >= "2020-03-01" & date <= "2021-03-01" ~ 1, # COVID
      TRUE ~ 0
    ),
    dummy2 = case_when(
      date >= "2021-08-01" & date <= max(date) ~ 1, # CASTILLO ENTRA A LA PRESIDENCIA
      TRUE ~ 0
    )
  ) 


glimpse(df)

formula <- (log(reer) ~ 
              # relative productivity
            + diff_gdp 
            # terms of trade
            #+ log(terms)
            + log(cobre)
            # interest rate differentials
            + diff_rates
            #+ diff_rates_exante
            # external account
            #+ current
            + current_sum_12
            #+ current_mean_12
            # structural change
            #+ dummy 
            + dummy2)

# Supongamos que df y formula ya están definidos
model_data <- procesar_modelos(
  df = df
  , formula = formula
  , filter_model_data = filter_model_data
  , ruta = ruta
  , hoja = hoja
  , start_year = start_year
  , end_year = end_year
)


