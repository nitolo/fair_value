#######################################################
################ RECOLECCIÓN DE DATOS     #############
################ MODELOS BEER DAILY       #############
#######################################################
cat("\014") # Limpiar la consola
while(dev.cur() > 1) dev.off() # Limpiar todos los gráficos
rm(list = ls()) # Limpiar el entorno global

library(dplyr)
library(fixest)
library(openxlsx)
library(lubridate)
library(extrafont) # New fonts
library(janitor)
# Cargar las fuentes en el dispositivo gráfico
#loadfonts(device = "win")

ruta <- "Z:/03_Investigaciones_Economicas/3. Modelos/4. Regressions/FX EQUILIBRIUM/"
libro <- "FX_BEER.xlsx"
hoja  <- "Data"


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
  mutate(year = lubridate::year(date)
         , month = lubridate::month(date)
         , day = lubridate::day(date)
         , weekday = lubridate::wday(date)
         )


#################################
############ CHILE ##############
#################################

daily_beer_analysis <- function(df, moneda, isocode_country, formula) {
  # Validación de inputs
  if (!is.character(moneda) || length(moneda) != 1) {
    stop("moneda debe ser un string único")
  }
  if (!is.character(isocode_country) || length(isocode_country) != 1) {
    stop("isocode_country debe ser un string único")
  }
  
  tpm_col <- paste0(isocode_country, "_tpm")
  bono_col <- paste0(isocode_country, "_bono_gob_2y")
  
  cols_exist <- list(
    bono = bono_col %in% names(df),
    tpm = tpm_col %in% names(df)
  )
  # Seleccionar columnas relevantes
  
  df_fx <- df %>%
    select(
      date, starts_with(moneda), starts_with(isocode_country), starts_with("usa"),
      brl, ars,ars_mep,
      year, month, day, weekday
    ) %>% 
    mutate(
      diff = if(cols_exist$bono) {
        !!sym(bono_col) - usa_treasury_2y
      } else {
        NA_real_
      }
       #, tpm_diff = !!sym(paste0(isocode_country, "_tpm")) - usa_tpm
       , tpm_diff = if(cols_exist$tpm) {!!sym(tpm_col) - usa_tpm
       } else {NA_real_}
    )
  
  # Colocar last date para hacer las restas con los periodos
  last_date <- max(df_fx$date)
  
  # Plantear los filtros de cada mes. Hacerlo por dias para siempre tener la misma cantidad
  filters <- c(  "year_5"  = 1800
               , "year_4"  = 1440
               , "year_3"  = 1080
               , "year_2"  = 720
               , "year_1"  = 360
               , "month_6" = 180
               , "month_3" = 90
               #, "year_7"  = 2520 
               #, "year_10" = 3600
               #, "year_12" = 4320
               #, "year_15" = 5400
               )
  
  # Crear los dataframes con los cortes específicos y ajustar los modelos
  models <- list()
  models_lm <- list()
  data_dates <- list()
  r2 <- list()
  for (name in names(filters)) {
    df_filtered <- df_fx %>% 
      filter(date >= last_date - lubridate::days(filters[[name]]))
    models[[name]] <- feols(formula, data = df_filtered, cluster = "weekday")
    models_lm[[name]] <- lm(formula, data = df_filtered)
    data_dates[[name]] <- data.frame(date = df_filtered$date)
    r2[[name]] <- fixest::r2(models[[name]], type = "r2") %>% as.data.frame()
    
  }
  
  
  # Crear un nuevo libro de Excel (solo cuando se corre por primera vez)
  #wb <- createWorkbook()
  wb <- loadWorkbook(paste0(ruta, moneda, "_dailybeer.xlsx"))
  
  # Función para verificar si una hoja existe

  # Agregar una hoja para cada modelo y escribir los resultados
  for (name in names(models)) {
    #addWorksheet(wb, name)
    #model_summary <- etable(models[[name]], signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10))
    model_summary <- fixest::coeftable(models[[name]], signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10)) %>% 
      as.data.frame()
    writeData(wb, name, model_summary, rowNames = T)
  }
  
  for (name in names(models_lm)) {
    model_lm_summary <- models_lm[[name]]$model 
    #model_lm_summary <- models_lm[[name]]$model %>% as.data.frame()
    predictions <- predict(models_lm[[name]], interval = "prediction") %>% as.data.frame()
    #model_fit <- models_lm[[name]]$fitted.values
    model_data <- cbind(
      data_dates[[name]], 
      model_lm_summary,
      fitted = predictions[, "fit"],
      lowerci = predictions[, "lwr"],
      upperci = predictions[, "upr"]
    )
    #model_data <- cbind(data_dates[[name]], model_lm_summary,model_fit)
    writeData(wb, name, model_data, startCol = 8)
  }
  for (name in names(r2)) {
    #r2[[name]] <- fixest::r2(models[[name]], type = "r2")
    number_rows <- as.numeric(nrow(model_summary)+2)
    writeData(wb, name, r2[[name]], startRow = number_rows, colNames = F, rowNames = T)
  }

  # Guardar el libro de Excel
  saveWorkbook(wb, paste0(ruta, moneda, "_dailybeer.xlsx"), overwrite = TRUE)
  
  # Devolver los modelos para su uso posterior si es necesario
  #return(models)
  #return(df_filtered)
  return(list(models = models, df_filtered = df_filtered))
  
}

formula_clp <- (log(clp) ~ 
              + log(usa_dxy)
              + log(usa_copper)
              + log(chl_cds_5y)
              #+ usa_treasury_10y
              + log(usa_s_p500)
              + log(chl_ipsa)
              + diff
)
model_clp <- daily_beer_analysis(df, "clp", "chl", formula_clp)
etable(model_clp$models,se.row = T,  signif.code = c("***"=0.01, "**"=0.05, "*"=0.10))
#2years



formula_cop <- (log(cop) ~ 
                + log(usa_dxy)
                + log(usa_brent)
                + log(col_cds_5y)
                #+ usa_treasury_10y
                + log(usa_s_p500)
                + log(col_msci)
                + diff
)
model_cop <- daily_beer_analysis(df, "cop", "col", formula_cop)
etable(model_cop$models, signif.code = c("***"=0.01, "**"=0.05, "*"=0.10))
#6months

formula_mxn <- (log(mxn) ~ 
                #+ log(usa_dxy)
                + log(usa_brent)
                + log(mex_cds_5y)
                #+ usa_treasury_10y
                + log(usa_s_p500)
                + log(mex_bmv_ipc)
                + tpm_diff
                #+ diff
)
model_mxn <- daily_beer_analysis(df, "mxn", "mex", formula_mxn)
etable(model_mxn$models, signif.code = c("***"=0.01, "**"=0.05, "*"=0.10))
# 1year

formula_pen <- (log(pen) ~ 
                + log(usa_dxy)
                + log(usa_copper)
                #+ log(usa_gold)
                + log(per_cds_5y)
                #+ usa_treasury_2y
                #+ log(usa_s_p500)
                #+ log(per_bvl_general)
                + tpm_diff
)
model_pen <- daily_beer_analysis(df, "pen", "per", formula_pen)
etable(model_pen$models, signif.code = c("***"=0.01, "**"=0.05, "*"=0.10))
# 2years

formula_uyu <- (log(uyu) ~ 
                #+ log(usa_dxy)
                + log(usa_soybean)
                + log(usa_livestock)
                #+ log(usa_brent)
                #+ log(usa_commodities_index)
                + log(brl)
                #+ log(ars_mep)
                #+ tpm_diff
)

model_uyu <- daily_beer_analysis(df, "uyu", "ury", formula_uyu)
etable(model_uyu$models, signif.code = c("***"=0.01, "**"=0.05, "*"=0.10))
# 6months




