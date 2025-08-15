#######################################################
################ RECOLECCIÓN DE DATOS     #############
################ MODELOS PCA DE FX        #############
#######################################################
cat("\014") # Limpiar la consola
while(dev.cur() > 1) dev.off() # Limpiar todos los gráficos
rm(list = ls()) # Limpiar el entorno global

# Este enlace es muy bueno para tomar una guia de cómo lo hacen otras industrias
#https://rpubs.com/Joaquin_AR/287787
library(dplyr)
library(fixest)
library(openxlsx)
library(lubridate)
library(janitor)
library(pls)

ruta <- "Z:/03_Investigaciones_Economicas/3. Modelos/4. Regressions/FX PCA/"
libro <- "FX_PCA.xlsx"
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


# PRUEBA

pcr_model <- pcr(log(cop)~., data = df[,-1], scale = TRUE, ncomp=4, validation = "CV")


full_model <- lm(log(cop) ~ ., data = df[-1])

summary(full_model)


backward_model <- step(full_model, direction = "backward")

summary(backward_model)

fitted_values <- fitted(backward_model)



summary(pcr_model)

validationplot(pcr_model, val.type="RMSEP", cex.axis=0.7)

predplot(pcr_model)

pcr_model$Xmeans
exp(pcr_model$fitted.values)

df <- df %>% tail(90)

# Guardar fechas por separado
dates <- df$date

# Eliminar columnas no numéricas (como fechas)
df <- df[, !(names(df) %in% c("date"))]

# Variables dependientes
target_columns <- c("cop", "clp", "mxn", "uyu")

# Aplicar logaritmo natural a las variables dependientes
df[target_columns] <- log(df[target_columns])

# Separar variables dependientes e independientes
y <- df[, target_columns]
X <- df[, !(names(df) %in% target_columns)]



df_s <- base::scale(X, center = T, scale = T)

pca = prcomp(df_s, center=FALSE, scale.=FALSE, rank. = 3) 
summary(pca)




biplot(x = pca, scale = 0, cex = 0.6, col = c("blue4", "brown3"))

pca_x = pca$x # saving the principal components
class(pca_x)
pca_x


# Obtener los loadings
#loadings$PC2 <- abs(loadings$PC2)
#loadings
# Renombrar las columnas para que sean PC1, PC2, etc.
#colnames(loadings) <- paste0("PC", 1:ncol(loadings))

# Ordenar por los valores absolutos más altos en PC1
top_PC1 <- loadings %>%
  dplyr::arrange(desc((PC1))) %>%
  head(10)

# Mostrar los resultados
print(top_PC1)

# col_bono_gob_2y = pca1
# usa_commodities_index = pca2
# per_cds_5y = pca3
# usa_wheat = pca4


m1 <- lm(y$cop ~ pca_x)
summary(m1)

line1= exp(m1$model$`y$cop`)
line2= exp((m1$fitted.values))



# Graficar los valores reales
plot(line1, type = "l", col = "blue", lwd = 2, 
     ylab = "Valor", xlab = "Tiempo", 
     main = "Valores reales vs ajustados")

# Agregar los valores ajustados
lines(line2, col = "red", lwd = 2, lty = 2)


x = data.frame(pca_lm = (m1$fitted.values), pca_pca = (pcr_model$fitted.values))

nrow(m1$fitted.values)

x = m1$fitted.values
m = fitted.values(pcr_model)
m
dim(m)
fitted.values(pcr_model)
g = pcr_model$model

df_s <- as.data.frame(df_s)

pca_x[,1]

df_s$col_bono_gob_2y

m_2 <- lm((df$cop) ~ df_s$col_bono_gob_2y + df_s$usa_commodities_index + df_s$per_cds_5y
          + df_s$usa_wheat)
summary(m_2)

m2 <- lm(log(df$clp) ~ pca_x)
summary(m2)

