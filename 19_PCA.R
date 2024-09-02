rm(list=ls())
#Objetivo unir todos dataset, hacer un merge por dia
# Directorio
setwd("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/PM25")

# Generar la secuencia de fechas desde el 01-01-2015 hasta el 31-07-2024
date <- seq.Date(from = as.Date("2015-01-01"), to = as.Date("2024-06-30"), by = "day")
# Crear el dataframe con la columna 'date'
df_date <- data.frame(date = date)
df_date$date <- strptime(df_date$date, format = "%Y-%m-%d")
nrow(df_date)
### Abrimos todas las estaciones
### 01-OHG --------------------------
data_pm_ohg <- read.csv("./01-OHG_PM25.csv")
data_pm_ohg <- data.frame(date=data_pm_ohg$fecha, OHG = data_pm_ohg$valor)
data_pm_ohg$date  <- strptime(data_pm_ohg$date, format = "%Y-%m-%d")

### 02-BSQ --------------------------
data_pm_BSQ <- read.csv("./02-BSQ_PM25.csv")
data_pm_BSQ <- data.frame(date=data_pm_BSQ$fecha, BSQ = data_pm_BSQ$valor)
data_pm_BSQ$date  <- strptime(data_pm_BSQ$date, format = "%Y-%m-%d")

### 03-CNA --------------------------
data_pm_CNA <- read.csv("./03-CNA_PM25.csv")
data_pm_CNA <- data.frame(date=data_pm_CNA$fecha, CNA = data_pm_CNA$valor)
data_pm_CNA$date  <- strptime(data_pm_CNA$date, format = "%Y-%m-%d")

### 04-PDH --------------------------
data_pm_PDH <- read.csv("./04-PDH_PM25.csv")
data_pm_PDH <- data.frame(date=data_pm_PDH$fecha, PDH = data_pm_PDH$valor)
data_pm_PDH$date  <- strptime(data_pm_PDH$date, format = "%Y-%m-%d")

### 05-FLD --------------------------
data_pm_FLD <- read.csv("./05-FLD_PM25.csv")
data_pm_FLD <- data.frame(date=data_pm_FLD$fecha, FLD = data_pm_FLD$valor)
data_pm_FLD$date  <- strptime(data_pm_FLD$date, format = "%Y-%m-%d")

### 05-PTA --------------------------
data_pm_PTA <- read.csv("./06-PTA_PM25.csv")
data_pm_PTA <- data.frame(date=data_pm_PTA$fecha, PTA = data_pm_PTA$valor)
data_pm_PTA$date  <- strptime(data_pm_PTA$date, format = "%Y-%m-%d")
   

### 07-CDE --------------------------
data_pm_CDE <- read.csv("./07-CDE_PM25.csv")
data_pm_CDE <- data.frame(date=data_pm_CDE$fecha, CDE = data_pm_CDE$valor)
data_pm_CDE$date  <- strptime(data_pm_CDE$date, format = "%Y-%m-%d")       

### 08-QUI --------------------------
data_pm_QUI <- read.csv("./08-QUI_PM25.csv")
data_pm_QUI <- data.frame(date=data_pm_QUI$fecha, QUI = data_pm_QUI$valor)
data_pm_QUI$date  <- strptime(data_pm_QUI$date, format = "%Y-%m-%d")       

##############################

# Lista de dataframes que quieres unir
dataframes <- list(df_date,data_pm_ohg,data_pm_BSQ,data_pm_CNA,
                   data_pm_PDH,data_pm_FLD,data_pm_PTA,data_pm_CDE,
                   data_pm_QUI)

# Usar Reduce para hacer merge secuencial
data_merged <- Reduce(function(x, y) merge(x, y, by = "date", all.x = TRUE), dataframes)

# Guardar
write.csv(data_merged,"D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/otras/merge_Estaciones_PM25/merge_Estaciones_PM25.csv")

########################################################
###########################################################
library(psych)
library(dplyr)
library(ggplot2)
library(reshape2)
library(pvclust) 
pca <- princomp(na.omit(tabla[1:length(tabla)]), cor=TRUE) #PCA en la matriz de covarianza
data_subt <- na.omit(data_merged[2:length(data_merged)])
# Cargar las librerías necesarias
library(tidyverse)

# Cargar los datos
tabla <- read.csv("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/PM25/01-OHG_PM25.csv")

# Convertir la columna de fecha a tipo Date
tabla$date <- as.Date(tabla$date, format = "%Y-%m-%d")

# Eliminar las columnas no numéricas si existen
# (Esto asume que solo quieres realizar PCA sobre las columnas numéricas de estaciones)
data_pca <- tabla %>%
  select(-date) %>% # Elimina la columna de fecha
  na.omit()         # Elimina filas con NA

# Realizar PCA usando la matriz de correlación
pca <- princomp(data_subt, cor = TRUE) 

# Graficar la varianza explicada por cada componente
plot(pca, type = "lines", main = "Gráfica de sedimentación")

# Mostrar los resultados del PCA
summary(pca)


plot (pca, type="l", main="Gráfica de sedimentación")
qplot(c(1:length(pca$sdev)), round(pca$sdev^2/sum(pca$sdev^2),2)) + 
  geom_line() + 
  xlab("PC") + 
  ylab("Varianza explicada") +
  ggtitle("PM25") +
  ylim(0,1) + 
  theme_bw()

qplot(c(1:length(pca$sdev)), round(pca$sdev^2/sum(pca$sdev^2),2)) + 
  geom_line() + 
  xlab("PC") + 
  ylab("Varianza explicada") +
  ggtitle("PM25") +
  ylim(0,1) + 
  theme_bw()



qplot(c(1:length(pca$sdev)), cumsum(round(pca$sdev^2/sum(pca$sdev^2),2)))  + 
  geom_hline( yintercept = 0.7, col="red", linetype="dashed" ) + 
  geom_line() + 
  xlab("PC") + 
  ylab("Varianza acumulada") + 
  ggtitle(PM25) +
  ylim(0, 1.1) + 
  theme_bw()


vari <- principal(na.omit(tabla[2:length(tabla)]), nfactors= PC[i], 
                  rotate="varimax", covar = FALSE)
