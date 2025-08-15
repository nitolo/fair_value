#######################################################
################ RECOLECCIÓN DE DATOS #############
################ MODELOS  #############
#######################################################

cat("\014") # Limpiar la consola
while(dev.cur() > 1) dev.off() # Limpiar todos los gráficos
rm(list = ls()) # Limpiar el entorno global

library(openxlsx)
library(httr)
library(lubridate)
library(readxl)
library(rvest)
library(janitor)
library(fixest)
library(sandwich)
library(tidyverse)
library(mFilter)
library(lmtest)
library(tempdisagg)
library(car)
library(urca)
library(zoo)
library(tseries)
library(scales)
# Cargar las fuentes en el dispositivo gráfico
#loadfonts(device = "win")


############################################
############### ESTIMACIONES ###############
############################################

cat("\014") # Limpiar la consola
while(dev.cur() > 1) dev.off() # Limpiar todos los gráficos
rm(list = ls()) # Limpiar el entorno global

dest_folder <- "Z:/03_Investigaciones_Economicas/2. Monitores/Colombia/Temporales"
ruta <- "Z:/03_Investigaciones_Economicas/3. Modelos/4. Regressions/REER/"
libro <- "reer_hispam.xlsx"

colombia = T
chile    = T
peru     = T
mexico   = T
parametros = T
uruguay    = T

if (parametros) {
  # Lista de formatos de archivo
  formats <- c("png", "svg", "pdf")
  
  filter_model_data <- 2009
  
  # Función para guardar el plot en diferentes formatos
  save_plot <- function(format) {
    ggsave(filename = paste0(ruta_plot, plot_name, ".", format), 
           plot = gg, 
           dpi = 320, 
           width = width, 
           height = height)
  }
  
}

if (colombia){
  hoja <- "COP"

df <- read.xlsx(paste0(ruta, libro), sheet = hoja, startRow = 3, colNames = T)
glimpse(df)

df_names <- read.xlsx(paste0(ruta, libro), sheet = hoja, startRow = 2, colNames = F)
glimpse(df_names)

## filtramos a solo la primera fila. equivale a la fila 2 de excel
df_names <- df_names[1,] %>% as.character()

colnames(df) <- df_names
glimpse(df)

if (is.numeric(df[,1])) {
  # SI EL FORMATO ESTA EN EL FORMATO EXCEL 45170, 45173, ETC
  df[,1] <- as.Date(df[,1], origin = "1899-12-30")
} else {
  # SI YA ESTA EN FORMATO DTTM, RELAX, ESTA OK. SOLO CONVERTIRLO A DATE PARA LUEGO USAR GGPLOT2
  df[,1] <- as.Date(df[,1])
}
# CONFIRMAMOS QUE SE HAYA CAMBIADO
glimpse(df)

df <- df %>% clean_names()

glimpse(df)

df <- df %>% 
  mutate(
    year = year(date),
    month = month(date),
    gdp_per = gdp/po,
    gdp_per_g = (gdp_per/lag(gdp_per, 12))-1,
    gdp_per_usa = gdp_usa/po_usa,
    gdp_per_usa_g = (gdp_per_usa/lag(gdp_per_usa, 12))-1,
    #diff_gdp = gdp_per_g - gdp_per_usa_g,
    diff_gdp = ((1+gdp_per_g) / (1+gdp_per_usa_g))-1,
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


glimpse(df)

formula <- (log(reer) ~
              # Relative productivity
              + diff_gdp 
            # Terms of trade
            + log(brent)
            # Interest rate differentials
            #+ diff_rates_exante 
            + diff_rates
            # External account
            #+ trade_balance
            #+ trade_balance_fob
            + trade_balance_sum_12
            #+ trade_balance_mean_12
            + dummy 
            )

m1 <- lm(formula
         ,data = df
         , subset = year >= filter_model_data
)

summary(m1)

# Guardar la data de este modelo, es more simple
model_lm_data <- m1$model %>% tail(144)

# Guardar el fit y los otros

predictions <- predict(m1, interval = "prediction", level = 0.95) %>% as.data.frame()%>% tail(144)

# Filtrar los datos de la misma forma, para que combinarlos sea mas easy
df_date <- df %>% 
  filter(year >= filter_model_data) %>% 
  select(date) %>% tail(144)

# Combinar las bases, primero fecha y luego la data
model_data <- cbind(df_date, model_lm_data, 
                    fitted = predictions[, "fit"],
                    lowerci = predictions[, "lwr"],
                    upperci = predictions[, "upr"])

# Hacer que todos los datos empiecen por 01/mes/year
model_data <- model_data %>%
  mutate(date = make_date(year(date), month(date), 1))

# Utilizar los ultimos 144 datos, es decir, los ultimos 144 meses
model_data <- tail(model_data, 144)
glimpse(model_data)

m2 <- feols(formula
            , data = df
            , subset = df$year >= 2009
            & df$year <= 2024
            #, se="HC1"
            , cluster = "month"
)

# Guardar los coeficientes
model_summary <- fixest::coeftable(m2, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10)) %>% 
  as.data.frame()

# Guardar el R cuadro
r2 <- fixest::r2(m2, type = "r2") %>% as.data.frame()
number_rows <- as.numeric(nrow(model_summary)+2)

# Mostrar los resultados 
etable(m2, signif.code = c("***"=0.01, "**"=0.05, "*"=0.10))

# Cargar el libro de Excel donde estan los resultados
wb <- loadWorkbook(paste0(ruta, "reer_models_original",".xlsx"))

# Escribir los datos de los coeficientes
writeData(wb, hoja, model_summary, rowNames = T)
# Escribir el R cuadro debajo de los coeficientes
writeData(wb, hoja, r2, startRow = number_rows, colNames = F, rowNames = T)
# Escribir los datos para hacer calculos en Excel
writeData(wb, hoja, model_data, startCol = 8)

# Guardar
saveWorkbook(wb, paste0(ruta, "reer_models_original",".xlsx"), overwrite = TRUE)


}


if (chile){
  hoja <- "CLP"

df <- read.xlsx(paste0(ruta, libro),sheet = hoja, startRow = 3, colNames = T)
glimpse(df)

df_names <- read.xlsx(paste0(ruta, libro),sheet = hoja, startRow = 2, colNames = F)
glimpse(df_names)

## filtramos a solo la primera fila. equivale a la fila 2 de excel
df_names <- df_names[1,] %>% as.character()

colnames(df) <- df_names
glimpse(df)

if (is.numeric(df[,1])) {
  # SI EL FORMATO ESTA EN EL FORMATO EXCEL 45170, 45173, ETC
  df[,1] <- as.Date(df[,1], origin = "1899-12-30")
} else {
  # SI YA ESTA EN FORMATO DTTM, RELAX, ESTA OK. SOLO CONVERTIRLO A DATE PARA LUEGO USAR GGPLOT2
  df[,1] <- as.Date(df[,1])
}
# CONFIRMAMOS QUE SE HAYA CAMBIADO
glimpse(df)

df <- df %>% clean_names()

glimpse(df)
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
    year = year(date),
    month = month(date),
    investment = investment*-1,
    gdp_per = gdp/po,
    gdp_per_g = (gdp_per/lag(gdp_per, 12))-1,
    gdp_per_usa = gdp_usa/po_usa,
    gdp_per_usa_g = (gdp_per_usa/lag(gdp_per_usa, 12))-1,
    #diff_gdp = gdp_per_g - gdp_per_usa_g,
    diff_gdp = ((1+gdp_per_g) / (1+gdp_per_usa_g))-1,
    diff_rates = (tpm-cpi) - (tpm_usa-cpi_usa),
    #diff_rates = ((1+tpm) / (1+ tpm_usa))-1,
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




m1 <- lm(formula
         ,data = df
         , subset = year >= filter_model_data
)




m2 <- feols(formula
            ,data = df
            , subset = df$year >= 2010
            &  df$year <= 2024
            #, se = "HC1"
            , cluster = "month"
)

etable(m2, signif.code = c("***"=0.01, "**"=0.05, "*"=0.10))



summary(m1)

# Guardar la data de este modelo, es more simple
model_lm_data <- m1$model %>% tail(144)

# Guardar el fit y los otros

predictions <- predict(m1, interval = "prediction", level = 0.95) %>% as.data.frame()%>% tail(144)

# Filtrar los datos de la misma forma, para que combinarlos sea mas easy
df_date <- df %>% 
  filter(year >= filter_model_data) %>% 
  select(date) %>% tail(144)

# Combinar las bases, primero fecha y luego la data
model_data <- cbind(df_date, model_lm_data, 
                    fitted = predictions[, "fit"],
                    lowerci = predictions[, "lwr"],
                    upperci = predictions[, "upr"])

# Hacer que todos los datos empiecen por 01/mes/year
model_data <- model_data %>%
  mutate(date = make_date(year(date), month(date), 1))

# Utilizar los ultimos 144 datos, es decir, los ultimos 144 meses
model_data <- tail(model_data, 144)
glimpse(model_data)

# Prueba de autocorrelación
dwtest(m1) # p value <5% significa autocorrelación

# Prueba de heterocedasticidad
bptest(m1)

# Prueba de normalidad
jarque.bera.test(residuals(m1))

# Prueba de multicolinealidad
vif(m1)
modelo <- m1$model
cor <- cor(modelo)
# Prueba de estacionariedad
adf.test(m1$model[,1])# p value >5% significa que no es estacionaria= raiz unitaria  

# Prueba de cointegración
johansen_test <- ca.jo(modelo, type = "trace", ecdet = "none", K = 2)
summary(johansen_test)

m2 <- feols(formula
            ,data = df
            , subset = df$year >= 2010
            &  df$year <= 2024
            #, se = "HC1"
            , cluster = "month"
            )
etable(m2, signif.code = c("***"=0.01, "**"=0.05, "*"=0.10))

# Guardar los coeficientes
model_summary <- fixest::coeftable(m2, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10)) %>% 
  as.data.frame()

# Guardar el R cuadro
r2 <- fixest::r2(m2, type = "r2") %>% as.data.frame()
number_rows <- as.numeric(nrow(model_summary)+2)

# Mostrar los resultados 
etable(m2, signif.code = c("***"=0.01, "**"=0.05, "*"=0.10))

# Cargar el libro de Excel donde estan los resultados
wb <- loadWorkbook(paste0(ruta, "reer_models_original",".xlsx"))

# Escribir los datos de los coeficientes
writeData(wb, hoja, model_summary, rowNames = T)
# Escribir el R cuadro debajo de los coeficientes
writeData(wb, hoja, r2, startRow = number_rows, colNames = F, rowNames = T)
# Escribir los datos para hacer calculos en Excel
writeData(wb, hoja, model_data, startCol = 8)

# Guardar
saveWorkbook(wb, paste0(ruta, "reer_models_original",".xlsx"), overwrite = TRUE)

}

if (peru){
  hoja <- "PEN"

df <- read.xlsx(paste0(ruta, libro),sheet = hoja, startRow = 3, colNames = T)
glimpse(df)

df_names <- read.xlsx(paste0(ruta, libro),sheet = hoja, startRow = 2, colNames = F)
glimpse(df_names)

## filtramos a solo la primera fila. equivale a la fila 2 de excel
df_names <- df_names[1,] %>% as.character()

colnames(df) <- df_names
glimpse(df)

if (is.numeric(df[,1])) {
  # SI EL FORMATO ESTA EN EL FORMATO EXCEL 45170, 45173, ETC
  df[,1] <- as.Date(df[,1], origin = "1899-12-30")
} else {
  # SI YA ESTA EN FORMATO DTTM, RELAX, ESTA OK. SOLO CONVERTIRLO A DATE PARA LUEGO USAR GGPLOT2
  df[,1] <- as.Date(df[,1])
}
# CONFIRMAMOS QUE SE HAYA CAMBIADO
glimpse(df)

df <- df %>% clean_names()

glimpse(df)


df <- df %>% 
  mutate(
    year = year(date),
    month = month(date),
    gdp_per = gdp/po,
    gdp_per_g = (gdp_per/lag(gdp_per, 1))-1,
    gdp_per_usa = gdp_usa/po_usa,
    gdp_per_usa_g = (gdp_per_usa/lag(gdp_per_usa, 12))-1,
    diff_gdp = gdp_per_g - gdp_per_usa_g,
    exp_gdp = exp/gdp,
    imp_gdp = imp/gdp,
    debt_real = debt/(cpi_usa_index/100),
    current_real = current/(cpi_usa_index/100),
    current_sum_12 = rollsum(current, k = 12, fill = NA, align = "right"),
    current_mean_12 = rollmean(current, k = 12, fill = NA, align = "right"),
    #investment_sum_12 = rollsum(investment, k = 12, fill = NA, align = "right"),
    #investment_mean_12 = rollmean(investment, k = 12, fill = NA, align = "right"),
    copper_real = (cobre/cpi_usa_index)*100,
    diff_rates = (tpm-cpi) - (tpm_usa-cpi_usa),
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
            + log(terms)
            #+ log(cobre)
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

m1 <- lm(formula
         ,data = df
         , subset = year >= filter_model_data
)

summary(m1)

# Guardar la data de este modelo, es more simple
model_lm_data <- m1$model %>% tail(142)

# Guardar el fit y los otros

predictions <- predict(m1, interval = "prediction", level = 0.95) %>% as.data.frame()%>% tail(142)

# Filtrar los datos de la misma forma, para que combinarlos sea mas easy
df_date <- df %>% 
  filter(year >= filter_model_data) %>% 
  select(date) %>% tail(142)

# Combinar las bases, primero fecha y luego la data
model_data <- cbind(df_date, model_lm_data, 
                    fitted = predictions[, "fit"],
                    lowerci = predictions[, "lwr"],
                    upperci = predictions[, "upr"])

# Hacer que todos los datos empiecen por 01/mes/year
model_data <- model_data %>%
  mutate(date = make_date(year(date), month(date), 1))

# Utilizar los ultimos 144 datos, es decir, los ultimos 144 meses
model_data <- tail(model_data, 142)
glimpse(model_data)



# Datos del modelo
modelo <- m1$model

# Estimaciones robustas de errores estándar
coeftest(m1, vcov = vcovHC(m1, type = "HC1"))

# Prueba de autocorrelación
dwtest(m1) # p value <5% significa autocorrelación

# Prueba de heterocedasticidad
bptest(m1)

# Prueba de normalidad
jarque.bera.test(residuals(m1))

# Prueba de multicolinealidad
vif(m1)
cor <- cor(modelo)
# Prueba de estacionariedad
adf.test(m1$model[,1])# p value >5% significa que no es estacionaria= raiz unitaria  

# Prueba de cointegración
johansen_test <- ca.jo(modelo, type = "trace", ecdet = "none", K = 2)
summary(johansen_test)


m2 <- feols(formula
            , data = df
            , subset = df$year >= 2013
            #& df$year <= 2020
            #, se = "HC1"
            , cluster= "month"
            )
etable(m2,signif.code = c("***"=0.01, "**"=0.05, "*"=0.10))

# Guardar los coeficientes
model_summary <- fixest::coeftable(m2, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10)) %>% 
  as.data.frame()

# Guardar el R cuadro
r2 <- fixest::r2(m2, type = "r2") %>% as.data.frame()
number_rows <- as.numeric(nrow(model_summary)+2)

# Mostrar los resultados 
etable(m2, signif.code = c("***"=0.01, "**"=0.05, "*"=0.10))

# Cargar el libro de Excel donde estan los resultados
wb <- loadWorkbook(paste0(ruta, "reer_models_original",".xlsx"))

# Escribir los datos de los coeficientes
writeData(wb, hoja, model_summary, rowNames = T)
# Escribir el R cuadro debajo de los coeficientes
writeData(wb, hoja, r2, startRow = number_rows, colNames = F, rowNames = T)
# Escribir los datos para hacer calculos en Excel
writeData(wb, hoja, model_data, startCol = 8)

# Guardar
saveWorkbook(wb, paste0(ruta, "reer_models_original",".xlsx"), overwrite = TRUE)


}

if (mexico){
  hoja <- "MXN"
  
  df <- read.xlsx(paste0(ruta, libro),sheet = hoja, startRow = 3, colNames = T)
  glimpse(df)
  
  df_names <- read.xlsx(paste0(ruta, libro),sheet = hoja, startRow = 2, colNames = F)
  glimpse(df_names)
  
  ## filtramos a solo la primera fila. equivale a la fila 2 de excel
  df_names <- df_names[1,] %>% as.character()
  
  colnames(df) <- df_names
  glimpse(df)
  
  if (is.numeric(df[,1])) {
    # SI EL FORMATO ESTA EN EL FORMATO EXCEL 45170, 45173, ETC
    df[,1] <- as.Date(df[,1], origin = "1899-12-30")
  } else {
    # SI YA ESTA EN FORMATO DTTM, RELAX, ESTA OK. SOLO CONVERTIRLO A DATE PARA LUEGO USAR GGPLOT2
    df[,1] <- as.Date(df[,1])
  }
  # CONFIRMAMOS QUE SE HAYA CAMBIADO
  glimpse(df)
  
  df <- df %>% clean_names()
  
  glimpse(df)
  
  df <- df %>% 
    mutate(
      year = year(date),
      month = month(date),
      investment = investment*-1,
      gdp_per = gdp/po,
      gdp_per_g = (gdp_per/lag(gdp_per, 12))-1,
      gdp_per_usa = gdp_usa/po_usa,
      gdp_per_usa_g = (gdp_per_usa/lag(gdp_per_usa, 12))-1,
      index_wages = (unit_labor/unit_labor_usa)*100,
      diff_gdp = gdp_per_g - gdp_per_usa_g,
      diff_rates = (tpm-cpi) - (tpm_usa-cpi_usa),
      diff_rates_exante = (tpm-expectation) - (tpm_usa-expectation_usa),
      diff_rates_no = (tpm) - (tpm_usa),
      current_real = current/(cpi_usa_index/100),
      investment_real = investment/(cpi_usa_index/100),
      basic_balance  = current + investment,
      basic_balance_real  = current_real + investment_real,
      basic_balance_sum_12 = rollsum(basic_balance, k = 12, fill = NA, align = "right"),
      basic_balance_mean_12 = rollmean(basic_balance, k = 12, fill = NA, align = "right"),
      investment_sum_12 = rollsum(investment, k = 12, fill = NA, align = "right"),
      investment_mean_12 = rollmean(investment, k = 12, fill = NA, align = "right"),
      basic_balance_real_sum_12 = rollsum(basic_balance_real, k = 12, fill = NA, align = "right"),
      basic_balance_real_mean_12 = rollmean(basic_balance_real, k = 12, fill = NA, align = "right"),
      trade_balance = exp - imp, 
      debt_real = debt/(cpi_usa_index/100),
      terms_real = (terms/cpi_index)*100,
      brent_real = (brent/cpi_usa_index)*100,
      unit_labor_usa_real = (unit_labor_usa/cpi_usa_index)*100,
      unit_labor_real = (unit_labor/cpi_index)*100,
      index_wages_real = (unit_labor_real/unit_labor_usa_real)*100,
      diff_labor = ((unit_labor_real/lag(unit_labor_real, 12))-1) - ((unit_labor_usa_real/lag(unit_labor_usa_real, 12))-1),
      dummy = case_when(
        date >= "2008-01-01" & date <= "2009-12-01" ~ 1, # CRISIS DEL 2008
        date >= "2020-03-01" & date <= "2021-03-01" ~ 1, # COVID
        TRUE ~ 0
      )
    ) 
  
  
  glimpse(df)
  
  formula <- (log(reer) ~ 
                # productivity
                #+ log(index_wages_real) 
                + log(index_wages) 
              # terms
              #+ log(brent) 
              + log(terms)
              # interest rate
              + diff_rates 
              #+ diff_rates_exante
              #external account
              #+ basic_balance_real
              + basic_balance_sum_12
              #+ basic_balance_real_sum_12
              #+ investment_sum_12
              #+ basic_balance_mean_12
              #+ investment_real
              #+ log(debt_real)
              + dummy)
  
  m1 <- lm(formula
           ,data = df
          , subset = year >= filter_model_data
  )
  
  summary(m1)
  
  # Guardar la data de este modelo, es more simple
  model_lm_data <- m1$model %>% tail(144)
  
  # Guardar el fit y los otros
  
  predictions <- predict(m1, interval = "prediction", level = 0.95) %>% as.data.frame()%>% tail(144)
  
  # Filtrar los datos de la misma forma, para que combinarlos sea mas easy
  df_date <- df %>% 
    filter(year >= filter_model_data) %>% 
    select(date) %>% tail(144)
  
  # Combinar las bases, primero fecha y luego la data
  model_data <- cbind(df_date, model_lm_data, 
                      fitted = predictions[, "fit"],
                      lowerci = predictions[, "lwr"],
                      upperci = predictions[, "upr"])
  
  # Hacer que todos los datos empiecen por 01/mes/year
  model_data <- model_data %>%
    mutate(date = make_date(year(date), month(date), 1))
  
  # Utilizar los ultimos 144 datos, es decir, los ultimos 144 meses
  model_data <- tail(model_data, 144)
  glimpse(model_data)
  
  
  # Datos del modelo
  modelo <- m1$model
  
  # Estimaciones robustas de errores estándar
  coeftest(m1, vcov = vcovHC(m1, type = "HC1"))
  
  # Prueba de autocorrelación
  dwtest(m1) # p value <5% significa autocorrelación
  
  # Prueba de heterocedasticidad
  bptest(m1)
  
  # Prueba de normalidad
  jarque.bera.test(residuals(m1))
  
  # Prueba de multicolinealidad
  vif(m1)
  cor <- cor(modelo)
  # Prueba de estacionariedad
  adf.test(m1$model[,1])# p value >5% significa que no es estacionaria= raiz unitaria  
  
  # Prueba de cointegración
  johansen_test <- ca.jo(modelo, type = "trace", ecdet = "none", K = 2)
  summary(johansen_test)
  
  
  m2 <- feols(formula
              ,data = df
              , subset = df$year >= 2013
              #& df$year <= 2020
              #,se = "HC1"
              , cluster = "month"
              )
  etable(m2, signif.code = c("***"=0.01, "**"=0.05, "*"=0.10))
  
  # Guardar los coeficientes
  model_summary <- fixest::coeftable(m2, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10)) %>% 
    as.data.frame()
  
  # Guardar el R cuadro
  r2 <- fixest::r2(m2, type = "r2") %>% as.data.frame()
  number_rows <- as.numeric(nrow(model_summary)+2)
  
  # Mostrar los resultados 
  etable(m2, signif.code = c("***"=0.01, "**"=0.05, "*"=0.10))
  
  # Cargar el libro de Excel donde estan los resultados
  wb <- loadWorkbook(paste0(ruta, "reer_models_original",".xlsx"))
  
  # Escribir los datos de los coeficientes
  writeData(wb, hoja, model_summary, rowNames = T)
  # Escribir el R cuadro debajo de los coeficientes
  writeData(wb, hoja, r2, startRow = number_rows, colNames = F, rowNames = T)
  # Escribir los datos para hacer calculos en Excel
  writeData(wb, hoja, model_data, startCol = 8)
  
  # Guardar
  saveWorkbook(wb, paste0(ruta, "reer_models_original",".xlsx"), overwrite = TRUE)
  
}

if (uruguay){
  hoja <- "UYU"
  
  df <- read.xlsx(paste0(ruta, libro),sheet = hoja, startRow = 3, colNames = T)
  glimpse(df)
  
  df_names <- read.xlsx(paste0(ruta, libro),sheet = hoja, startRow = 2, colNames = F)
  glimpse(df_names)
  
  ## filtramos a solo la primera fila. equivale a la fila 2 de excel
  df_names <- df_names[1,] %>% as.character()
  
  colnames(df) <- df_names
  glimpse(df)
  
  if (is.numeric(df[,1])) {
    # SI EL FORMATO ESTA EN EL FORMATO EXCEL 45170, 45173, ETC
    df[,1] <- as.Date(df[,1], origin = "1899-12-30")
  } else {
    # SI YA ESTA EN FORMATO DTTM, RELAX, ESTA OK. SOLO CONVERTIRLO A DATE PARA LUEGO USAR GGPLOT2
    df[,1] <- as.Date(df[,1])
  }
  # CONFIRMAMOS QUE SE HAYA CAMBIADO
  glimpse(df)
  
  df <- df %>% clean_names()
  
  
  glimpse(df)
  
  # Encontrar los valores de cpi_index y cpi_usa_index al 31/12/2023
  date_base = as.Date("2023-12-29")
  cpi_index_2023 <- df$cpi_index[df$date == date_base]
  cpi_usa_index_2023 <- df$cpi_usa_index[df$date == date_base]
  terms_2023 <- df$terms[df$date == date_base]
  reer_2023 <- df$reer[df$date == date_base]
  unit_labor_2023 <- df$unit_labor[df$date == date_base]
  unit_labor_usa_2023 <- df$unit_labor_usa[df$date == date_base]
  
  
  # Rebasear todos los índices
  df <- df %>%
    mutate(
      cpi_index = (cpi_index / cpi_index_2023)*100
      ,cpi_usa_index = (cpi_usa_index / cpi_usa_index_2023)*100
      , terms = (terms / terms_2023)*100
      #, reer = (reer / reer_2023)*100
      , unit_labor = (unit_labor /unit_labor_2023)*100
      , unit_labor_usa = (unit_labor_usa / unit_labor_usa_2023)*100
    )
  
  df <- df %>% 
    mutate(
      year = year(date),
      month = month(date),
      terms_real = (terms/cpi_index)*100,
      index_wages = (unit_labor/unit_labor_usa)*100,
      #soybean_real = soybean/(cpi_usa_index/100),
      #livestock_real = livestock/(cpi_usa_index/100),
      current_real = current/(cpi_usa_index/100),
      current_sum_12 = rollsum(current, k = 12, fill = NA, align = "right"),
      current_mean_12 = rollmean(current, k = 12, fill = NA, align = "right"),
      #debt_real = debt/(cpi_usa_index/100),
      unit_labor_usa_real = (unit_labor_usa/cpi_usa_index)*100,
      unit_labor_real = (unit_labor/cpi_index)*100,
      diff_labor = ((unit_labor_real/lag(unit_labor_real, 12))-1) - ((unit_labor_usa_real/lag(unit_labor_usa_real, 12))-1),
      index_wages_real = (unit_labor_real/unit_labor_usa_real)*100,
      #diff_rates = (tpm-cpi) - (tpm_usa-cpi_usa),
      #diff_rates_exante = (tpm-expectation) - (tpm_usa-expectation_usa),
      dummy = case_when(
        date >= "2008-01-01" & date <= "2009-12-01" ~ 1, # CRISIS DEL 2008
        date >= "2020-03-01" & date <= "2021-03-01" ~ 1, # COVID
        TRUE ~ 0
      )
    ) 
  
  
  glimpse(df)
  
  formula = (log(reer) ~ 
               # productivity
               + log(index_wages)
             # external accounts
             #+ current_sum_12
             #+ log(tourism)
             #+ log(cc_travel_debit)
             + log(cc_travel_credit)
             #+ log(goverment_revenue))
             )
  
  m1 <- lm(formula
           ,data = df
           , subset = year >= filter_model_data
  )
  
  # Guardar la data de este modelo, es more simple
  model_lm_data <- m1$model %>% tail(144)
  
  # Guardar el fit y los otros
  
  predictions <- predict(m1, interval = "prediction", level = 0.95) %>% as.data.frame()%>% tail(144)
  
  # Filtrar los datos de la misma forma, para que combinarlos sea mas easy
  df_date <- df %>% 
    filter(year >= filter_model_data) %>% 
    select(date) %>% tail(144)
  
  # Combinar las bases, primero fecha y luego la data
  model_data <- cbind(df_date, model_lm_data, 
                      fitted = predictions[, "fit"],
                      lowerci = predictions[, "lwr"],
                      upperci = predictions[, "upr"])
  
  # Hacer que todos los datos empiecen por 01/mes/year
  model_data <- model_data %>%
    mutate(date = make_date(year(date), month(date), 1))
  
  # Utilizar los ultimos 144 datos, es decir, los ultimos 144 meses
  model_data <- tail(model_data, 144)
  glimpse(model_data)
  
  summary(m1)
  
  # Prueba de autocorrelación
  dwtest(m1) # p value <5% significa autocorrelación
  
  # Prueba de heterocedasticidad
  bptest(m1)
  # Prueba de normalidad
  jarque.bera.test(residuals(m1))
  # Prueba de multicolinealidad
  vif(m1)
  cor <- cor(modelo)
  # Prueba de estacionariedad
  #adf.test(m1$model[,1])# p value >5% significa que no es estacionaria= raiz unitaria  
  
  # Prueba de cointegración
  johansen_test <- ca.jo(modelo, type = "trace", ecdet = "none", K = 2)
  summary(johansen_test)
  
  
  m2 <- feols(formula
              ,data = df
              , subset = df$year >= 2013
              #& df$year <= 2020
              ,cluster = "month"
              #, se = "HC1"
              )
  etable(m2,signif.code = c("***"=0.01, "**"=0.05, "*"=0.10) )
  
  modelo2 <- etable(m2
                    , digits = 3
                    , signif.code = c("***"=0.01, "**"=0.05, "*"=0.10)
                    , se.below = TRUE
  )
  
  modelo2
  
  # Guardar los coeficientes
  model_summary <- fixest::coeftable(m2, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10)) %>% 
    as.data.frame()
  
  # Guardar el R cuadro
  r2 <- fixest::r2(m2, type = "r2") %>% as.data.frame()
  number_rows <- as.numeric(nrow(model_summary)+2)
  
  # Mostrar los resultados 
  etable(m2, signif.code = c("***"=0.01, "**"=0.05, "*"=0.10))
  
  # Cargar el libro de Excel donde estan los resultados
  wb <- loadWorkbook(paste0(ruta, "reer_models_original",".xlsx"))
  
  # Escribir los datos de los coeficientes
  writeData(wb, hoja, model_summary, rowNames = T)
  # Escribir el R cuadro debajo de los coeficientes
  writeData(wb, hoja, r2, startRow = number_rows, colNames = F, rowNames = T)
  # Escribir los datos para hacer calculos en Excel
  writeData(wb, hoja, model_data, startCol = 8)
  
  # Guardar
  saveWorkbook(wb, paste0(ruta, "reer_models_original",".xlsx"), overwrite = TRUE)
  
  
}







# Filtra los datos para que tengan la misma ventana de tiempo. 
#df<- df %>% 
#  filter(year >=2009) 

# Obtiene los valores ajustados del modelo BEER
#df$fv1 = exp(m2$fitted.values)

# df$fv = exp(m2$coeftable[1,1]
#              + m2$coeftable[2,1]*(df$diff_gdp)
#              + m2$coeftable[3,1]*log(df$brent)
#              + m2$coeftable[4,1]*(df$diff_rates)
#              + m2$coeftable[5,1]*(df$trade_balance_sum_12)
#              + m2$coeftable[6,1]*(df$dummy)
#             )
# 
# 
# 
# 
# glimpse(df)
# 
# # Calcular la diferencia y la desviacion del tipo de cambio real menos fv
# df_1 <- df %>% 
#   dplyr::select(date, reer, fv) %>% 
#   mutate(gap = reer-fv
#          , deviation= (reer - fv) / fv)
# 
# summary(df_1)
# 
# # Calcular el error estándar
# se <- sd(df_1$deviation, na.rm = TRUE)
# 
# ##############################################
# ############## PLOTS #########################
# ##############################################
# 
# # Convertir a formato fecha. De lo contrario, va a molestar
# df_1$date <- as.Date(df_1$date)
# 
# # Obtener la última fecha
# last_date <- max(df_1$date)
# 
# # Obtener el mes de la fecha máxima
# max_month <- month(last_date)
# 
# # Crear un dataframe con los últimos valores
# last_values_df <- df_1 %>%
#   filter(date == last_date) %>%
#   select(-date)
# 
# # Filtrar el dataframe para obtener solo las filas que corresponden al mes máximo
# filtered_dates <- df_1 %>%
#   filter(month(date) == max_month) %>%
#   pull(date)
# 
# # Calcular la fecha de hace diez años
# ten_years <- as.Date(paste0(year(last_date) - 10, "-", format(last_date, "%m"), "-01"))
# 
# # Filtrar datos de los últimos diez años
# df_1 <- df_1 %>%
#   filter(date >= ten_years)
# 
# uno <- as.numeric(last_values_df[,1])
# dos <- as.numeric(last_values_df[,2])
# tres <- as.numeric(last_values_df[,3])
# cuatro <- as.numeric(last_values_df[,4])
# #cinco <- as.numeric(last_values_df[,5])
# 
# col_uno <- "#52C4CC"
# col_dos <- "#99E5FF"
# col_tres <- "#003FFF"
# col_cuatro <- "#836FFF"
# 
# f1 = "Arial Narrow"
# 
# #################################################
# ########## Graficar BEER vs REER ################
# #################################################
# 
# gg <- ggplot(df_1, aes(x=date)) +
#   geom_vline(aes(xintercept=(as.Date("2020-03-31"))), color ="#D2BFAB", linetype="dashed", linewidth = 0.9)+
#   geom_line(aes(y=reer, color="REER"), linewidth = .9) +
#   geom_line(aes(y=fv, color="BEER"), linewidth = .9) +
#   scale_color_manual(values=c("REER"=col_uno, "BEER"=col_dos))+
#   labs(color=" ", y=" ", x= " "
#        , title= paste0("FX REAL: ", hoja)
#   ) +
#   theme_minimal(base_family = f1) +
#   theme(plot.title = element_text(hjust = 0.5, face="bold", size = 15, colour = "#0E2841")
#         , plot.subtitle = element_text(hjust = 0.5, face = "italic", size=12, colour = "#7F7F7F")
#         , axis.line.x = element_line(colour = "black")
#         , axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)
#         , axis.text = element_text(size = 10)
#         , axis.ticks.x = element_line(color = "#212121", size = 0.3)
#         , panel.grid.minor.y = element_blank()
#         , panel.grid.minor.x = element_blank()
#         , panel.grid.major.x = element_blank()
#         #, panel.grid.major.y = element_blank()
#         , plot.margin = margin(t = 2, r= 32, l=-5.5, b=0)
#         , legend.margin = margin(-4)
#         , legend.position="top"
#         , legend.text = element_text(size = 11)
#   )+ 
#   annotate("text", x=last_date, y=uno, label=round(uno,0), hjust=0, vjust=0.5, col=col_uno, size=4, family = f1, fontface = "bold")+
#   annotate("text", x=last_date, y=dos, label=round(dos,0), hjust=0, vjust=0.5, col=col_dos, size=4, family = f1, fontface = "bold")+
#   #annotate("text", x=as.Date("2019-05-31"), y=Inf, vjust = 1, label="Pandemia", col = "#D2BFAB", size=4, family = f1)+
#   #annotate("curve", x = as.Date("2019-05-31"), xend = as.Date("2020-03-31"), y= 255000, yend = 240000, arrow = arrow(length = unit(0.1, "inch"), ), size = 0.9, color = "#D2BFAB") +
#   coord_cartesian(clip = "off", expand = F) +
#   scale_x_date(breaks = c(as.Date(filtered_dates)), date_labels =  "%b%Y")+
#   scale_y_continuous(breaks = seq(90,180,18), limits = c(90, 180))
# 
# 
# print(gg)
# 
# plot_name = paste0( hoja, "_BEER")
# ruta_plot = "Z:/03_Investigaciones_Economicas/3. Modelos/4. Regressions/REER/Plots/"
# 
# # Asignar valores para unificar con todos
# height <- 5.5
# width <- 8
# 
# # GUARDAR TODO
# walk(formats, save_plot)

############################################################
########## Graficar la desviación histórica ################
############################################################
# gg <- ggplot(df_1, aes(x = date)) +
#   geom_line(aes(y=deviation, color="Desviación FV"), linewidth = .9) +
#   geom_hline(aes(yintercept = -se, color = "+/- SE"), linetype = "dashed", linewidth = 0.9) +
#   geom_hline(aes(yintercept = se, color = "+/- SE"), linetype = "dashed", linewidth = 0.9) +
#   scale_color_manual(values = c("Desviación FV" = col_uno, "+/- SE" = "blue")) +
#   labs(title = paste0("Desviación histórica del FV ", hoja)
#        , color = " "
#        , y = " "
#        , x = " "
#   ) +
#   theme_minimal(base_family = f1) +
#   theme(plot.title = element_text(hjust = 0.5, face="bold", size = 15, colour = "#0E2841")
#         , plot.subtitle = element_text(hjust = 0.5, face = "italic", size=12, colour = "#7F7F7F")
#         , axis.line.x = element_line(colour = "black")
#         , axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)
#         , axis.text = element_text(size = 10)
#         , axis.ticks.x = element_line(color = "#212121", size = 0.3)
#         , panel.grid.minor.y = element_blank()
#         , panel.grid.minor.x = element_blank()
#         , panel.grid.major.x = element_blank()
#         #, panel.grid.major.y = element_blank()
#         , plot.margin = margin(t = 2, r= 32, l=-5.5, b=0)
#         , legend.margin = margin(-4)
#         , legend.position="top"
#         , legend.text = element_text(size = 11)
#   )+ 
#   coord_cartesian(clip = "off", expand = F) +
#   annotate("text", x=last_date, y=cuatro, label=percent(cuatro,4), hjust=0, vjust=0.5, col=col_uno, size=4, family = f1, fontface = "bold")+
#   scale_x_date(breaks = c(as.Date(filtered_dates)), date_labels =  "%b%Y")+
#   scale_y_continuous(labels = percent,breaks = seq(-0.25,0.25,0.10), limits = c(-0.25, 0.25))
# 
# print(gg)
# 
# plot_name = paste0( hoja, "_DESVIACION")
# ruta_plot = "Z:/03_Investigaciones_Economicas/3. Modelos/4. Regressions/REER/Plots/"
# 
# # Asignar valores para unificar con todos
# height <- 5.5
# width <- 8
# 
# # GUARDAR TODO
# walk(formats, save_plot)


#write.xlsx(modelo2, paste0(ruta, "beer", hoja,".xlsx"))
# 
# # Filtra los datos para que tengan la misma ventana de tiempo. 
# df<- df %>% 
#   filter(year >=2013) 
# 
# # Obtiene los valores ajustados del modelo BEER
# #df$fv = exp(m2$fitted.values)
# df$fv = exp( m2$coeftable[1,1]
#             + m2$coeftable[2,1]*log(df$index_wages_real)
#             + m2$coeftable[3,1]*log(df$soybean)
#             + m2$coeftable[4,1]*(df$current_mean_12)
#             + m2$coeftable[5,1]*(df$dummy)
#             
#             )
# 

# 
# # Calcular la diferencia y la desviacion del tipo de cambio real menos fv
# df_1 <- df %>% 
#   dplyr::select(date, reer, fv) %>% 
#   mutate(gap = reer-fv
#          , deviation= (reer - fv) / fv)
# 
# summary(df_1)
# 
# # Calcular el error estándar
# se <- sd(df_1$deviation, na.rm = TRUE)
# 
# ##############################################
# ############## PLOTS #########################
# ##############################################
# 
# # Convertir a formato fecha. De lo contrario, va a molestar
# df_1$date <- as.Date(df_1$date)
# 
# # Obtener la última fecha
# last_date <- max(df_1$date)
# 
# # Obtener el mes de la fecha máxima
# max_month <- month(last_date)
# 
# # Crear un dataframe con los últimos valores
# last_values_df <- df_1 %>%
#   filter(date == last_date) %>%
#   select(-date)
# 
# # Filtrar el dataframe para obtener solo las filas que corresponden al mes máximo
# filtered_dates <- df_1 %>%
#   filter(month(date) == max_month) %>%
#   pull(date)
# 
# # Calcular la fecha de hace diez años
# ten_years <- as.Date(paste0(year(last_date) - 10, "-", format(last_date, "%m"), "-01"))
# 
# # Filtrar datos de los últimos diez años
# df_1 <- df_1 %>%
#   filter(date >= ten_years)
# 
# uno <- as.numeric(last_values_df[,1])
# dos <- as.numeric(last_values_df[,2])
# tres <- as.numeric(last_values_df[,3])
# cuatro <- as.numeric(last_values_df[,4])
# 
# col_uno <- "#52C4CC"
# col_dos <- "#99E5FF"
# col_tres <- "#003FFF"
# col_cuatro <- "#836FFF"
# 
# f1 = "Arial Narrow"
# 
# #################################################
# ########## Graficar BEER vs REER ################
# #################################################
# 
# ggplot(df_1, aes(x=date)) +
#   geom_vline(aes(xintercept=(as.Date("2020-03-31"))), color ="#D2BFAB", linetype="dashed", linewidth = 0.9)+
#   geom_line(aes(y=reer, color="REER"), linewidth = .9) +
#   geom_line(aes(y=fv, color="BEER"), linewidth = .9) +
#   scale_color_manual(values=c("REER"=col_uno, "BEER"=col_dos))+
#   labs(color=" ", y=" ", x= " "
#        , title= paste0("FX REAL:", hoja)
#   ) +
#   theme_minimal(base_family = f1) +
#   theme(plot.title = element_text(hjust = 0.5, face="bold", size = 15, colour = "#0E2841")
#         , plot.subtitle = element_text(hjust = 0.5, face = "italic", size=12, colour = "#7F7F7F")
#         , axis.line.x = element_line(colour = "black")
#         , axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)
#         , axis.text = element_text(size = 10)
#         , axis.ticks.x = element_line(color = "#212121", size = 0.3)
#         , panel.grid.minor.y = element_blank()
#         , panel.grid.minor.x = element_blank()
#         , panel.grid.major.x = element_blank()
#         #, panel.grid.major.y = element_blank()
#         , plot.margin = margin(t = 2, r= 32, l=-5.5, b=0)
#         , legend.margin = margin(-4)
#         , legend.position="top"
#         , legend.text = element_text(size = 11)
#   )+ 
#   annotate("text", x=last_date, y=uno, label=round(uno,0), hjust=0, vjust=0.5, col=col_uno, size=4, family = f1, fontface = "bold")+
#   annotate("text", x=last_date, y=dos, label=round(dos,0), hjust=0, vjust=0.5, col=col_dos, size=4, family = f1, fontface = "bold")+
#   #annotate("text", x=as.Date("2019-05-31"), y=Inf, vjust = 1, label="Pandemia", col = "#D2BFAB", size=4, family = f1)+
#   #annotate("curve", x = as.Date("2019-05-31"), xend = as.Date("2020-03-31"), y= 255000, yend = 240000, arrow = arrow(length = unit(0.1, "inch"), ), size = 0.9, color = "#D2BFAB") +
#   coord_cartesian(clip = "off", expand = F) +
#   scale_x_date(breaks = c(as.Date(filtered_dates)), date_labels =  "%b%Y")+
#   scale_y_continuous(breaks = seq(80,130,10), limits = c(80, 130))
# 
# 
# 
# 
# ############################################################
# ########## Graficar la desviación histórica ################
# ############################################################
# ggplot(df_1, aes(x = date)) +
#   geom_line(aes(y=deviation, color="Desviación FV"), linewidth = .9) +
#   geom_hline(aes(yintercept = -se, color = "+/- SE"), linetype = "dashed", linewidth = 0.9) +
#   geom_hline(aes(yintercept = se, color = "+/- SE"), linetype = "dashed", linewidth = 0.9) +
#   scale_color_manual(values = c("Desviación FV" = col_uno, "+/- SE" = "blue")) +
#   labs(title = paste0("Desviación histórica del FV ", hoja)
#        , color = " "
#        , y = " "
#   ) +
#   theme_minimal(base_family = f1) +
#   theme(plot.title = element_text(hjust = 0.5, face="bold", size = 15, colour = "#0E2841")
#         , plot.subtitle = element_text(hjust = 0.5, face = "italic", size=12, colour = "#7F7F7F")
#         , axis.line.x = element_line(colour = "black")
#         , axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)
#         , axis.text = element_text(size = 10)
#         , axis.ticks.x = element_line(color = "#212121", size = 0.3)
#         , panel.grid.minor.y = element_blank()
#         , panel.grid.minor.x = element_blank()
#         , panel.grid.major.x = element_blank()
#         #, panel.grid.major.y = element_blank()
#         , plot.margin = margin(t = 2, r= 32, l=-5.5, b=0)
#         , legend.margin = margin(-4)
#         , legend.position="top"
#         , legend.text = element_text(size = 11)
#   )+ 
#   coord_cartesian(clip = "off", expand = F) +
#   annotate("text", x=last_date, y=cuatro, label=percent(cuatro,4), hjust=0, vjust=0.5, col=col_uno, size=4, family = f1, fontface = "bold")+
#   scale_x_date(breaks = c(as.Date(filtered_dates)), date_labels =  "%b%Y")+
#   scale_y_continuous(labels = percent,breaks = seq(-0.25,0.25,0.05), limits = c(-0.25, 0.25))
# 
