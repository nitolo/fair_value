#######################################################
################ RECOLECCIÓN DE DATOS     #############
################ MODELOS BEER DAILY       #############
#######################################################
cat("\014") # Limpiar la consola
while(dev.cur() > 1) dev.off() # Limpiar todos los gráficos
rm(list = ls()) # Limpiar el entorno global

library(tsibble)
library(dplyr)
library(tidyverse)
library(lubridate)
library(openxlsx)
library(janitor)
library(fixest)
library(zoo)


rellenar_fechas <- function(df) {
  # Convertir a tsibble y rellenar fechas faltantes
  df_1 <- df %>% 
    as_tsibble(index = date) %>% 
    tsibble::fill_gaps()
  
  # Obtener los nombres de las columnas desde la segunda hasta la última
  column_names <- names(df)[2:ncol(df)]
  
  # Rellenar las fechas faltantes
  df_1 <- df_1 %>% 
    fill(all_of(column_names)) %>% 
    as.data.frame()
  
  # Conocer la última fecha del mes y filtrar
  df_1 <- df_1 %>%
    mutate(last_day = ceiling_date(date, "month") - days(1))
  
  df_month <- df_1 %>%
    filter(date == last_day) %>% 
    mutate(date = make_date(year(date), month(date), 1))
  
  return(df_month)
}

procesar_datos <- function(ruta, libro, hoja, frequency="daily") {
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
  
  # Rellenar fechas faltantes y filtrar últimos días del mes
  
  if (frequency == "daily"){
    df <- rellenar_fechas(df)
    glimpse(df)
  }
  
  
  return(df)
}

# Inputs
ruta <- "Z:/03_Investigaciones_Economicas/3. Modelos/4. Regressions/PASS-THROUGH/"
libro <- "Base macro.xlsx"
hoja <- "Daily"

# DF de los datos diarios a último dato del mes
df <- procesar_datos(ruta, libro, hoja, "daily")
# DF de los datos mensuales

hoja <- "Monthly"
df_1 <- procesar_datos(ruta, libro, hoja, "monthly") %>% 
  mutate(date = make_date(year(date), month(date), 1))

# Unir las bases
data <- df %>% 
  left_join(df_1, by = "date")%>% 
  mutate(
    year = year(date),
    month = month(date),
    day = day(date),
    weekday = wday(date),
  )

df <- data


# Colombia 

data <- df %>% 
  dplyr::select(
    date, cop, dplyr::starts_with("col"), dplyr::starts_with("usa"),
    year, month
    )

date_base = as.Date("2023-12-01")

cpi_2023 <- data$col_cpi[data$date == date_base]
usa_cpi_2023 <- data$usa_cpi[data$date == date_base]
gdp_2023 <- data$col_gdp[data$date == date_base]
usa_gdp_2023 <- data$usa_gdp[data$date == date_base]
terms_2023 <- data$col_terms[data$date == date_base]
usa_terms_2023 <- data$usa_terms[data$date == date_base]
wage_2023 <- data$col_wage_index[data$date == date_base]
usa_wage_2023 <- data$usa_wage_index[data$date == date_base]


data <- data %>% 
  dplyr::mutate(
    col_cpi = (col_cpi/cpi_2023)*100,
    usa_cpi = (usa_cpi/usa_cpi_2023)*100,
    col_gdp = (col_gdp/gdp_2023)*100,
    usa_gdp = (usa_gdp/usa_gdp_2023)*100,
    col_terms = (col_terms/terms_2023)*100,
    usa_terms = (usa_terms/usa_terms_2023)*100,
    col_wage = (col_wage_index/wage_2023)*100,
    usa_wage = (usa_wage_index/usa_wage_2023)*100,
    
    ratio_terms = col_terms/usa_terms,
    ratio_wage = col_wage/usa_wage,
    
    
    po = ((col_po/lag(col_po, 12))-1)*100,
    usa_po = ((usa_po/lag(usa_po, 12))-1)*100,
    
    
    inflation = ((col_cpi/lag(col_cpi, 12))-1)*100,
    inflation_usa = ((usa_cpi/lag(usa_cpi, 12))-1)*100,
    
    diff_inflation = inflation - inflation_usa,
    ratio_cpi = col_cpi/usa_cpi,
    
    
    diff_rates = (col_ti-inflation) - (usa_ti-inflation),
    #diff_rates = col_ti - usa_ti,
    
    gdp = ((col_gdp/lag(col_gdp, 12))-1)*100,
    gdp_usa = ((usa_gdp/lag(usa_gdp, 12))-1)*100,
    diff_gdp = (gdp) - (gdp_usa),
    #diff_gdp = ((1+gdp)/(1+gdp_usa))-1,
    diff_gdp_per_capita = (gdp-po)-(gdp_usa-usa_po),
    
    #ratio_gdp = usa_gdp/col_gdp,
    ratio_gdp = col_gdp/usa_gdp,
    
    terms = ((col_terms/lag(col_terms, 12))-1)*100,
    terms_usa = ((usa_terms/lag(usa_terms, 12))-1)*100,
    
    diff_terms = (terms - terms_usa),
    
    
    
    
    terms_real = (terms-inflation),
    
    commodity = ((usa_brent/lag(usa_brent, 12))-1)*100,
    commodity_real = (commodity-inflation),
    
    reer = (1/(col_reer/100))*100,
    
    productivity = (col_wage/col_ppi)*100,
    dummy = case_when(
      date >= "2008-01-01" & date <= "2009-12-01" ~ 1, # CRISIS DEL 2008
      date >= "2020-03-01" & date <= "2021-03-01" ~ 1, # COVID
      TRUE ~ 0)
    
    
  )

filter_model_data = 2010

formula = (log(reer)~
           #+ ratio_cpi
           #+ ratio_gdp
           + log(ratio_terms)
           + log(ratio_wage)
           #+ log(ratio_gdp)
           + diff_inflation
           #+ diff_gdp_per_capita
           #+ diff_terms
           #+ terms_real
           #+ commodity_real
           #+ dummy
           )

m <- feols(formula
           , subset = data$year >= filter_model_data
           , data = data
           , cluster = "month"
)



etable(m,signif.code = c("***"=0.01, "**"=0.05, "*"=0.10) )

m1 <- lm(formula
         ,data = data
         , subset = year >= filter_model_data
)

summary(m1)

# Guardar la data de este modelo, es more simple
model_lm_data <- m1$model %>% tail(120)

# Guardar el fit y los otros

predictions <- predict(m1, interval = "prediction", level = 0.95) %>% as.data.frame()%>% tail(120)

# Filtrar los datos de la misma forma, para que combinarlos sea mas easy
df_date <- data %>% 
  filter(year >= filter_model_data) %>% 
  select(date) %>% tail(120)

df_fx <- data %>% 
  filter(year >= filter_model_data) %>% 
  select(cop) %>% tail(120)

# Combinar las bases, primero fecha y luego la data
model_data <- cbind(df_date, model_lm_data, 
                    fitted = predictions[, "fit"],
                    lowerci = predictions[, "lwr"],
                    upperci = predictions[, "upr"],
                    df_fx
                    )

# Hacer que todos los datos empiecen por 01/mes/year
model_data <- model_data %>%
  mutate(date = make_date(year(date), month(date), 1))

# Utilizar los ultimos 120 datos, es decir, los ultimos 120 meses
model_data <- tail(model_data, 120)
glimpse(model_data)

# Guardar los coficientes
model_summary <- fixest::coeftable(m, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10)) %>% 
  as.data.frame()

# Guardar el R cuadrado
r2 <- fixest::r2(m, type = "r2") %>% as.data.frame()

# Contar el numero de filas
number_rows <- as.numeric(nrow(model_summary)+2)


# Verificar las variables
print(ruta)
print(model_summary)
print(r2)
print(model_data)

# Cargar el libro de Excel donde estan los resultados

ruta = "Z:/03_Investigaciones_Economicas/3. Modelos/4. Regressions/DEER/"
hoja = "COP"


wb <- loadWorkbook(paste0(ruta, "DEER Models",".xlsx"))


# Verificar si la hoja de salida existe, si no, crearla
if (!(hoja %in% names(wb))) {
  addWorksheet(wb, hoja)
}
# Escribir los datos de los coeficientes
writeData(wb, hoja, model_summary, rowNames = T)
# Escribir el R cuadro debajo de los coeficientes
writeData(wb, hoja, r2, startRow = number_rows, colNames = F, rowNames = T)
# Escribir los datos para hacer calculos en Excel
writeData(wb, hoja, model_data, startCol = 9)

# Guardar
saveWorkbook(wb, paste0(ruta, "DEER Models",".xlsx"), overwrite = TRUE)


##########
# Chile
##########

data <- df %>% 
  dplyr::select(
    date, clp, dplyr::starts_with("chl"), dplyr::starts_with("usa"),
    year, month
  )

date_base = as.Date("2023-12-01")

cpi_2023 <- data$chl_cpi[data$date == date_base]
usa_cpi_2023 <- data$usa_cpi[data$date == date_base]
gdp_2023 <- data$chl_gdp[data$date == date_base]
usa_gdp_2023 <- data$usa_gdp[data$date == date_base]
terms_2023 <- data$chl_terms[data$date == date_base]
usa_terms_2023 <- data$usa_terms[data$date == date_base]
wage_2023 <- data$chl_wage_index[data$date == date_base]
usa_wage_2023 <- data$usa_wage_index[data$date == date_base]


data <- data %>% 
  dplyr::mutate(
    chl_cpi = (chl_cpi/cpi_2023)*100,
    usa_cpi = (usa_cpi/usa_cpi_2023)*100,
    chl_gdp = (chl_gdp/gdp_2023)*100,
    usa_gdp = (usa_gdp/usa_gdp_2023)*100,
    chl_terms = (chl_terms/terms_2023)*100,
    usa_terms = (usa_terms/usa_terms_2023)*100,
    chl_wage = (chl_wage_index/wage_2023)*100,
    usa_wage = (usa_wage_index/usa_wage_2023)*100,
    
    ratio_terms = chl_terms/usa_terms,
    ratio_wage = chl_wage/usa_wage,
    
    po = ((chl_po/lag(chl_po, 12))-1)*100,
    usa_po = ((usa_po/lag(usa_po, 12))-1)*100,
    
    
    inflation = ((chl_cpi/lag(chl_cpi, 12))-1)*100,
    inflation_usa = ((usa_cpi/lag(usa_cpi, 12))-1)*100,
    
    diff_inflation = inflation - inflation_usa,
    ratio_cpi = chl_cpi/usa_cpi,
    
    
    diff_rates = (chl_ti-inflation) - (usa_ti-inflation),
    #diff_rates = chl_ti - usa_ti,
    
    gdp = ((chl_gdp/lag(chl_gdp, 12))-1)*100,
    gdp_usa = ((usa_gdp/lag(usa_gdp, 12))-1)*100,
    diff_gdp = (gdp) - (gdp_usa),
    #diff_gdp = ((1+gdp)/(1+gdp_usa))-1,
    diff_gdp_per_capita = (gdp-po)-(gdp_usa-usa_po),
    
    #ratio_gdp = usa_gdp/chl_gdp,
    ratio_gdp = chl_gdp/usa_gdp,
    
    commodity = ((usa_copper/lag(usa_copper, 12))-1)*100,
    commodity_real = (commodity-inflation_usa),
    
    reer = (1/(chl_reer/100))*100,
    
    productivity = (chl_cpi/chl_ppi)*100,
    dummy = case_when(
      date >= "2008-01-01" & date <= "2009-12-01" ~ 1, # CRISIS DEL 2008
      date >= "2020-03-01" & date <= "2021-03-01" ~ 1, # COVID
      TRUE ~ 0),
    dummy2 = case_when(
      year >= 2022 & year <= 2024 ~ 1, # CRISIS DEL 2008
      TRUE ~ 0)
  ) 
    

filter_model_data = 2010

formula = (log(reer)~
           
           + log(ratio_terms)
           + log(ratio_gdp)
           + diff_inflation
           #+ diff_gdp
           #+ diff_gdp_per_capita
           #+ commodity_real
           #+ dummy
           #+ dummy2
           )

m <- feols(formula
           , subset = data$year >= filter_model_data
           , data = data
           , cluster = "month"
)


etable(m,signif.code = c("***"=0.01, "**"=0.05, "*"=0.10) )

m1 <- lm(formula
         ,data = data
         , subset = year >= filter_model_data
)

summary(m1)

# Guardar la data de este modelo, es more simple
model_lm_data <- m1$model %>% tail(120)

# Guardar el fit y los otros

predictions <- predict(m1, interval = "prediction", level = 0.95) %>% as.data.frame()%>% tail(120)

# Filtrar los datos de la misma forma, para que combinarlos sea mas easy
df_date <- data %>% 
  filter(year >= filter_model_data) %>% 
  select(date) %>% tail(120)

df_fx <- data %>% 
  filter(year >= filter_model_data) %>% 
  select(clp) %>% tail(120)

# Combinar las bases, primero fecha y luego la data
model_data <- cbind(df_date, model_lm_data, 
                    fitted = predictions[, "fit"],
                    lowerci = predictions[, "lwr"],
                    upperci = predictions[, "upr"],
                    df_fx
)

# Hacer que todos los datos empiecen por 01/mes/year
model_data <- model_data %>%
  mutate(date = make_date(year(date), month(date), 1))

# Utilizar los ultimos 120 datos, es decir, los ultimos 120 meses
model_data <- tail(model_data, 120)
glimpse(model_data)

# Guardar los coficientes
model_summary <- fixest::coeftable(m, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10)) %>% 
  as.data.frame()

# Guardar el R cuadrado
r2 <- fixest::r2(m, type = "r2") %>% as.data.frame()

# Contar el numero de filas
number_rows <- as.numeric(nrow(model_summary)+2)


# Verificar las variables
print(ruta)
print(model_summary)
print(r2)
print(model_data)

# Cargar el libro de Excel donde estan los resultados

ruta = "Z:/03_Investigaciones_Economicas/3. Modelos/4. Regressions/DEER/"
hoja = "CLP"

wb <- loadWorkbook(paste0(ruta, "DEER Models",".xlsx"))


# Verificar si la hoja de salida existe, si no, crearla
if (!(hoja %in% names(wb))) {
  addWorksheet(wb, hoja)
}
# Escribir los datos de los coeficientes
writeData(wb, hoja, model_summary, rowNames = T)
# Escribir el R cuadro debajo de los coeficientes
writeData(wb, hoja, r2, startRow = number_rows, colNames = F, rowNames = T)
# Escribir los datos para hacer calculos en Excel
writeData(wb, hoja, model_data, startCol  = 9)

# Guardar
saveWorkbook(wb, paste0(ruta, "DEER Models",".xlsx"), overwrite = TRUE)


##########
# Mexico
##########

data <- df %>% 
  dplyr::select(
    date, mxn, dplyr::starts_with("mex"), dplyr::starts_with("usa"),
    year, month
  )

date_base = as.Date("2023-12-01")

cpi_2023 <- data$mex_cpi[data$date == date_base]
usa_cpi_2023 <- data$usa_cpi[data$date == date_base]
gdp_2023 <- data$mex_gdp[data$date == date_base]
usa_gdp_2023 <- data$usa_gdp[data$date == date_base]
terms_2023 <- data$mex_terms[data$date == date_base]
usa_terms_2023 <- data$usa_terms[data$date == date_base]
wage_2023 <- data$mex_wage_index[data$date == date_base]
usa_wage_2023 <- data$usa_wage_index[data$date == date_base]


data <- data %>% 
  dplyr::mutate(
    mex_cpi = (mex_cpi/cpi_2023)*100,
    usa_cpi = (usa_cpi/usa_cpi_2023)*100,
    mex_gdp = (mex_gdp/gdp_2023)*100,
    usa_gdp = (usa_gdp/usa_gdp_2023)*100,
    mex_terms = (mex_terms/terms_2023)*100,
    usa_terms = (usa_terms/usa_terms_2023)*100,
    mex_wage = (mex_wage_index/wage_2023)*100,
    usa_wage = (usa_wage_index/usa_wage_2023)*100,
    
    ratio_terms = mex_terms/usa_terms,
    ratio_wage = mex_wage/usa_wage,
    
    po = ((mex_po/lag(mex_po, 12))-1)*100,
    usa_po = ((usa_po/lag(usa_po, 12))-1)*100,
    
    
    inflation = ((mex_cpi/lag(mex_cpi, 12))-1)*100,
    inflation_usa = ((usa_cpi/lag(usa_cpi, 12))-1)*100,
    
    diff_inflation = inflation - inflation_usa,
    ratio_cpi = mex_cpi/usa_cpi,
    
    
    diff_rates = (mex_ti-inflation) - (usa_ti-inflation),
    #diff_rates = mex_ti - usa_ti,
    
    gdp = ((mex_gdp/lag(mex_gdp, 12))-1)*100,
    gdp_usa = ((usa_gdp/lag(usa_gdp, 12))-1)*100,
    diff_gdp = (gdp) - (gdp_usa),
    #diff_gdp = ((1+gdp)/(1+gdp_usa))-1,
    diff_gdp_per_capita = (gdp-po)-(gdp_usa-usa_po),
    
    #ratio_gdp = usa_gdp/mex_gdp,
    ratio_gdp = mex_gdp/usa_gdp,
    
    terms = ((mex_terms/lag(mex_terms, 12))-1)*100,
    usa_terms = ((usa_terms/lag(usa_terms, 12))-1)*100,
    
    diff_terms = (terms - usa_terms),
    
    
    terms_real = (terms-inflation),
    
    commodity = ((usa_brent/lag(usa_brent, 12))-1)*100,
    commodity_real = (commodity-inflation),
    
    reer = (1/(mex_reer/100))*100,
    
    productivity = (mex_cpi/mex_ppi)*100,
    dummy = case_when(
      date >= "2008-01-01" & date <= "2009-12-01" ~ 1, # CRISIS DEL 2008
      date >= "2020-03-01" & date <= "2021-03-01" ~ 1, # COVID
      TRUE ~ 0)
    
    
  )


filter_model_data = 2010

formula = (log(reer)~
           + log(ratio_terms)
           + log(ratio_wage)
           #+ diff_gdp
           + diff_inflation
           #+ terms
           #+ dummy
           )

m <- feols(formula
           , subset = data$year >= filter_model_data
           , data = data
           , cluster = "month"
)


etable(m,signif.code = c("***"=0.01, "**"=0.05, "*"=0.10) )

m1 <- lm(formula
         ,data = data
         , subset = year >= filter_model_data
)

summary(m1)

# Guardar la data de este modelo, es more simple
model_lm_data <- m1$model %>% tail(120)

# Guardar el fit y los otros

predictions <- predict(m1, interval = "prediction", level = 0.95) %>% as.data.frame()%>% tail(120)

# Filtrar los datos de la misma forma, para que combinarlos sea mas easy
df_date <- data %>% 
  filter(year >= filter_model_data) %>% 
  select(date) %>% tail(120)

df_fx <- data %>% 
  filter(year >= filter_model_data) %>% 
  select(mxn) %>% tail(120)

# Combinar las bases, primero fecha y luego la data
model_data <- cbind(df_date, model_lm_data, 
                    fitted = predictions[, "fit"],
                    lowerci = predictions[, "lwr"],
                    upperci = predictions[, "upr"],
                    df_fx
)

# Hacer que todos los datos empiecen por 01/mes/year
model_data <- model_data %>%
  mutate(date = make_date(year(date), month(date), 1))

# Utilizar los ultimos 120 datos, es decir, los ultimos 120 meses
model_data <- tail(model_data, 120)
glimpse(model_data)

# Guardar los coficientes
model_summary <- fixest::coeftable(m, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10)) %>% 
  as.data.frame()

# Guardar el R cuadrado
r2 <- fixest::r2(m, type = "r2") %>% as.data.frame()

# Contar el numero de filas
number_rows <- as.numeric(nrow(model_summary)+2)


# Verificar las variables
print(ruta)
print(model_summary)
print(r2)
print(model_data)

# Cargar el libro de Excel donde estan los resultados

ruta = "Z:/03_Investigaciones_Economicas/3. Modelos/4. Regressions/DEER/"
hoja = "MXN"

wb <- loadWorkbook(paste0(ruta, "DEER Models",".xlsx"))


# Verificar si la hoja de salida existe, si no, crearla
if (!(hoja %in% names(wb))) {
  addWorksheet(wb, hoja)
}
# Escribir los datos de los coeficientes
writeData(wb, hoja, model_summary, rowNames = T)
# Escribir el R cuadro debajo de los coeficientes
writeData(wb, hoja, r2, startRow = number_rows, colNames = F, rowNames = T)
# Escribir los datos para hacer calculos en Excel
writeData(wb, hoja, model_data, startCol  = 9)

# Guardar
saveWorkbook(wb, paste0(ruta, "DEER Models",".xlsx"), overwrite = TRUE)


##########
# Perú
##########

data <- df %>% 
  dplyr::select(
    date, pen, dplyr::starts_with("per"), dplyr::starts_with("usa"),
    year, month
  )

date_base = as.Date("2023-12-01")


cpi_2023 <- data$per_cpi[data$date == date_base]
usa_cpi_2023 <- data$usa_cpi[data$date == date_base]
gdp_2023 <- data$per_gdp[data$date == date_base]
usa_gdp_2023 <- data$usa_gdp[data$date == date_base]
terms_2023 <- data$per_terms[data$date == date_base]
usa_terms_2023 <- data$usa_terms[data$date == date_base]
wage_2023 <- data$per_wage_index[data$date == date_base]
usa_wage_2023 <- data$usa_wage_index[data$date == date_base]


data <- data %>% 
  dplyr::mutate(
    per_cpi = (per_cpi/cpi_2023)*100,
    usa_cpi = (usa_cpi/usa_cpi_2023)*100,
    per_gdp = (per_gdp/gdp_2023)*100,
    usa_gdp = (usa_gdp/usa_gdp_2023)*100,
    per_terms = (per_terms/terms_2023)*100,
    usa_terms = (usa_terms/usa_terms_2023)*100,
    per_wage = (per_wage_index/wage_2023)*100,
    usa_wage = (usa_wage_index/usa_wage_2023)*100,
    
    ratio_terms = per_terms/usa_terms,
    ratio_wage = per_wage/usa_wage,
    
    per_gdp_3m = rollmean(per_gdp, k = 3, fill = NA, align = "right"),
    usa_gdp_3m = rollmean(usa_gdp, k = 3, fill = NA, align = "right"),
    
    po_3m = rollmean(per_po, k = 3, fill = NA, align = "right"),
    usa_po_3m = rollmean(usa_po, k = 3, fill = NA, align = "right"),
    
    po = ((per_po/lag(per_po, 12))-1)*100,
    usa_po = ((usa_po/lag(usa_po, 12))-1)*100,
    
    
    inflation = ((per_cpi/lag(per_cpi, 12))-1)*100,
    inflation_usa = ((usa_cpi/lag(usa_cpi, 12))-1)*100,
    
    diff_inflation = inflation - inflation_usa,
    ratio_cpi = per_cpi/usa_cpi,
    
    
    diff_rates = (per_ti-inflation) - (usa_ti-inflation),
    #diff_rates = per_ti - usa_ti,
    
    gdp = ((per_gdp/lag(per_gdp, 12))-1)*100,
    gdp_usa = ((usa_gdp/lag(usa_gdp, 12))-1)*100,
    diff_gdp = (gdp) - (gdp_usa),
    #diff_gdp = ((1+gdp)/(1+gdp_usa))-1,
    diff_gdp_per_capita = (gdp-po)-(gdp_usa-usa_po),
    
    gdp_3m = ((per_gdp_3m/lag(per_gdp_3m, 12))-1)*100,
    gdp_usa_3m = ((usa_gdp_3m/lag(usa_gdp_3m, 12))-1)*100,
    diff_gdp_3m = (gdp_3m) - (gdp_usa_3m),
    #diff_gdp = ((1+gdp)/(1+gdp_usa))-1,
    diff_gdp_per_capita_3m = (gdp_3m-po_3m)-(gdp_usa_3m-usa_po_3m),
    
    
    #ratio_gdp = usa_gdp/per_gdp,
    ratio_gdp = per_gdp/usa_gdp,
    
    terms = ((per_terms/lag(per_terms, 12))-1)*100,
    usa_terms = ((usa_terms/lag(usa_terms, 12))-1)*100,
    
    diff_terms = (terms - usa_terms),
    
    
    terms_real = (terms-inflation),
    
    
    commodity = ((usa_copper/lag(usa_copper, 12))-1)*100,
    commodity_real = (commodity-inflation_usa),
    
    reer = (1/(per_reer/100))*100,
    
    productivity = (per_cpi/per_ppi)*100,
    dummy = case_when(
      date >= "2008-01-01" & date <= "2009-12-01" ~ 1, # CRISIS DEL 2008
      date >= "2020-03-01" & date <= "2021-03-01" ~ 1, # COVID
      TRUE ~ 0),
    ,
    dummy2 = case_when(
      date >= "2021-08-01" & date <= max(date) ~ 1, # CASTILLO ENTRA A LA PRESIDENCIA
      TRUE ~ 0
    )
    
    
  )

filter_model_data = 2010

formula = (log(reer)~
           
           + log(ratio_terms)
           + log(ratio_wage)
           + diff_inflation
           #+ ratio_cpi
           #+ ratio_gdp
           #+ diff_gdp_per_capita
           #+ diff_gdp_per_capita_3m
           #+ diff_terms
           #+ commodity
           #+ commodity_real
           #+ terms_real
           #+ log(usa_copper)
           #+ dummy
           #+ dummy2
           )

m <- feols(formula
           , subset = data$year >= filter_model_data
           , data = data
           , cluster = "month"
)


etable(m,signif.code = c("***"=0.01, "**"=0.05, "*"=0.10) )

m1 <- lm(formula
         ,data = data
         , subset = year >= filter_model_data
)

summary(m1)

# Guardar la data de este modelo, es more simple
model_lm_data <- m1$model %>% tail(120)

# Guardar el fit y los otros

predictions <- predict(m1, interval = "prediction", level = 0.95) %>% as.data.frame()%>% tail(120)

# Filtrar los datos de la misma forma, para que combinarlos sea mas easy
df_date <- data %>% 
  filter(year >= filter_model_data) %>% 
  select(date) %>% tail(120)

df_fx <- data %>% 
  filter(year >= filter_model_data) %>% 
  select(pen) %>% tail(120)

# Combinar las bases, primero fecha y luego la data
model_data <- cbind(df_date, model_lm_data, 
                    fitted = predictions[, "fit"],
                    lowerci = predictions[, "lwr"],
                    upperci = predictions[, "upr"],
                    df_fx
)

# Hacer que todos los datos empiecen por 01/mes/year
model_data <- model_data %>%
  mutate(date = make_date(year(date), month(date), 1))

# Utilizar los ultimos 120 datos, es decir, los ultimos 120 meses
model_data <- tail(model_data, 120)
glimpse(model_data)

# Guardar los coficientes
model_summary <- fixest::coeftable(m, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10)) %>% 
  as.data.frame()

# Guardar el R cuadrado
r2 <- fixest::r2(m, type = "r2") %>% as.data.frame()

# Contar el numero de filas
number_rows <- as.numeric(nrow(model_summary)+2)


# Verificar las variables
print(ruta)
print(model_summary)
print(r2)
print(model_data)

# Cargar el libro de Excel donde estan los resultados

ruta = "Z:/03_Investigaciones_Economicas/3. Modelos/4. Regressions/DEER/"
hoja = "PEN"

wb <- loadWorkbook(paste0(ruta, "DEER Models",".xlsx"))


# Verificar si la hoja de salida existe, si no, crearla
if (!(hoja %in% names(wb))) {
  addWorksheet(wb, hoja)
}
# Escribir los datos de los coeficientes
writeData(wb, hoja, model_summary, rowNames = T)
# Escribir el R cuadro debajo de los coeficientes
writeData(wb, hoja, r2, startRow = number_rows, colNames = F, rowNames = T)
# Escribir los datos para hacer calculos en Excel
writeData(wb, hoja, model_data, startCol  = 9)

# Guardar
saveWorkbook(wb, paste0(ruta, "DEER Models",".xlsx"), overwrite = TRUE)


##########
# Uruguay
##########

data <- df %>% 
  dplyr::select(
    date, uyu, dplyr::starts_with("ury"), dplyr::starts_with("usa"),
    year, month
  )


date_base = as.Date("2023-12-01")


cpi_2023 <- data$ury_cpi[data$date == date_base]
usa_cpi_2023 <- data$usa_cpi[data$date == date_base]
gdp_2023 <- data$ury_gdp[data$date == date_base]
usa_gdp_2023 <- data$usa_gdp[data$date == date_base]
terms_2023 <- data$ury_terms[data$date == date_base]
usa_terms_2023 <- data$usa_terms[data$date == date_base]
wage_2023 <- data$ury_wage_index[data$date == date_base]
usa_wage_2023 <- data$usa_wage_index[data$date == date_base]


data <- data %>% 
  dplyr::mutate(
    ury_cpi = (ury_cpi/cpi_2023)*100,
    usa_cpi = (usa_cpi/usa_cpi_2023)*100,
    ury_gdp = (ury_gdp/gdp_2023)*100,
    usa_gdp = (usa_gdp/usa_gdp_2023)*100,
    ury_terms = (ury_terms/terms_2023)*100,
    usa_terms = (usa_terms/usa_terms_2023)*100,
    ury_wage = (ury_wage_index/wage_2023)*100,
    usa_wage = (usa_wage_index/usa_wage_2023)*100,
    
    ratio_terms = ury_terms/usa_terms,
    ratio_wage = ury_wage/usa_wage,
    
    #po = (ury_po/lag(ury_po, 12))-1,
    usa_po = ((usa_po/lag(usa_po, 12))-1)*100,
    
    
    inflation = ((ury_cpi/lag(ury_cpi, 12))-1)*100,
    inflation_usa = ((usa_cpi/lag(usa_cpi, 12))-1)*100,
    
    diff_inflation = inflation - inflation_usa,
    ratio_cpi = ury_cpi/usa_cpi,
    
    
    diff_rates = (ury_ti-inflation) - (usa_ti-inflation),
    #diff_rates = ury_ti - usa_ti,
    
    gdp = ((ury_gdp/lag(ury_gdp, 12))-1)*100,
    gdp_usa = ((usa_gdp/lag(usa_gdp, 12))-1)*100,
    diff_gdp = (gdp) - (gdp_usa),
    #diff_gdp = ((1+gdp)/(1+gdp_usa))-1,
    #diff_gdp_per_capita = (gdp-po)-(usa_gdp-usa_po),
    
    #ratio_gdp = usa_gdp/ury_gdp,
    ratio_gdp = ury_gdp/usa_gdp,
    
    commodity = ((usa_soybean/lag(usa_soybean, 12))-1)*100,
    commodity_real = (usa_soybean-inflation_usa),
    
    reer = (1/(ury_reer/100))*100,
    
    productivity = (ury_cpi/ury_ppi)*100,
    
    dummy = case_when(
      date >= "2008-01-01" & date <= "2009-12-01" ~ 1, # CRISIS DEL 2008
      date >= "2020-03-01" & date <= "2021-03-01" ~ 1, # COVID
      TRUE ~ 0)
    
    
  )

filter_model_data = 2010

formula = (log(reer)~
           #+ ratio_cpi
           + log(ratio_terms)
           + log(ratio_gdp)
           + diff_inflation
           #+ factor(month)
           #+ commodity_real
           #+ log(usa_soybean)
           #+ terms_real
           #+ dummy 
           )

m <- feols(formula
           , subset = data$year >= filter_model_data
           , data = data
           , cluster = "month"
)


etable(m,signif.code = c("***"=0.01, "**"=0.05, "*"=0.10) )

m1 <- lm(formula
         ,data = data
         , subset = year >= filter_model_data
)

summary(m1)

# Guardar la data de este modelo, es more simple
model_lm_data <- m1$model %>% tail(120)

# Guardar el fit y los otros

predictions <- predict(m1, interval = "prediction", level = 0.95) %>% as.data.frame()%>% tail(120)

# Filtrar los datos de la misma forma, para que combinarlos sea mas easy
df_date <- data %>% 
  filter(year >= filter_model_data) %>% 
  select(date) %>% tail(120)

df_fx <- data %>% 
  filter(year >= filter_model_data) %>% 
  select(uyu) %>% tail(120)

# Combinar las bases, primero fecha y luego la data
model_data <- cbind(df_date, model_lm_data, 
                    fitted = predictions[, "fit"],
                    lowerci = predictions[, "lwr"],
                    upperci = predictions[, "upr"],
                    df_fx
)

# Hacer que todos los datos empiecen por 01/mes/year
model_data <- model_data %>%
  mutate(date = make_date(year(date), month(date), 1))

# Utilizar los ultimos 120 datos, es decir, los ultimos 120 meses
model_data <- tail(model_data, 120)
glimpse(model_data)

# Guardar los coficientes
model_summary <- fixest::coeftable(m, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10)) %>% 
  as.data.frame()

# Guardar el R cuadrado
r2 <- fixest::r2(m, type = "r2") %>% as.data.frame()

# Contar el numero de filas
number_rows <- as.numeric(nrow(model_summary)+2)


# Verificar las variables
print(ruta)
print(model_summary)
print(r2)
print(model_data)

# Cargar el libro de Excel donde estan los resultados

ruta = "Z:/03_Investigaciones_Economicas/3. Modelos/4. Regressions/DEER/"
hoja = "UYU"

wb <- loadWorkbook(paste0(ruta, "DEER Models",".xlsx"))

# Verificar si la hoja de salida existe, si no, crearla
if (!(hoja %in% names(wb))) {
  addWorksheet(wb, hoja)
}
# Escribir los datos de los coeficientes
writeData(wb, hoja, model_summary, rowNames = T)
# Escribir el R cuadro debajo de los coeficientes
writeData(wb, hoja, r2, startRow = number_rows, colNames = F, rowNames = T)
# Escribir los datos para hacer calculos en Excel
writeData(wb, hoja, model_data, startCol  = 9)

# Guardar
saveWorkbook(wb, paste0(ruta, "DEER Models",".xlsx"), overwrite = TRUE)

