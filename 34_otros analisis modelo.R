# Interestyear = 2015
data_estaciones <- read.csv("D:/Josefina/Proyectos/ProyectoChile/dataset/estaciones/diarios/PM25_tot.csv",colClasses = c("FECHA..YYMMDD."  = "character"))
data_estaciones$date <- as.POSIXct(as.character(data_estaciones$FECHA..YYMMDD.), format = "%y%m%d")
#data_estaciones <- data_estaciones[year(data_estaciones$date) == Interestyear,]
data_estaciones <- data_estaciones[complete.cases(data_estaciones$Registros.completos),]
unique(data_estaciones$estacion)
length(unique(data_estaciones$estacion))
data_estaciones_BSQ <- data_estaciones[data_estaciones$estacion=="BSQ",]
data_estaciones_CDE <- data_estaciones[data_estaciones$estacion=="CDE",]
data_estaciones_CER_I <- data_estaciones[data_estaciones$estacion=="CER-I",]
data_estaciones_CER_II <- data_estaciones[data_estaciones$estacion=="CER-II",]
data_estaciones_CNA <- data_estaciones[data_estaciones$estacion=="CNA",]
data_estaciones_FLD <- data_estaciones[data_estaciones$estacion=="FLD",]
data_estaciones_IND <- data_estaciones[data_estaciones$estacion=="IND",]
data_estaciones_OHG <- data_estaciones[data_estaciones$estacion=="OHG",]
data_estaciones_PDH <- data_estaciones[data_estaciones$estacion=="PDH",]
data_estaciones_PTA <- data_estaciones[data_estaciones$estacion=="PTA",]
data_estaciones_QUI <- data_estaciones[data_estaciones$estacion=="QUI",]
data_estaciones_QUI_I <- data_estaciones[data_estaciones$estacion=="QUI-I",]

#data_estaciones_BSQ$date  <- as.POSIXct(as.character(data_estaciones_BSQ$FECHA..YYMMDD.), format = "%y%m%d")

serie_fechas <- data.frame(date =  as.POSIXct(as.character(seq(as.Date("2015-01-01"), as.Date("2024-07-31"), by = "day"), format = "%Y-%m-%d")))#, value= 1)

merge_BSQ <- merge(serie_fechas, data_estaciones_BSQ, by = "date", all.x = TRUE)
merge_CDE <- merge(serie_fechas, data_estaciones_CDE, by = "date", all.x = TRUE)
merge_CER_I <- merge(serie_fechas, data_estaciones_CER_I, by = "date", all.x = TRUE)
merge_CER_II <- merge(serie_fechas, data_estaciones_CER_II, by = "date", all.x = TRUE)
merge_CNA <- merge(serie_fechas, data_estaciones_CNA, by = "date", all.x = TRUE)
merge_FLD <- merge(serie_fechas, data_estaciones_FLD, by = "date", all.x = TRUE)
merge_IND <- merge(serie_fechas, data_estaciones_IND, by = "date", all.x = TRUE)
merge_OHG <- merge(serie_fechas, data_estaciones_OHG, by = "date", all.x = TRUE)
merge_PDH <- merge(serie_fechas, data_estaciones_PDH, by = "date", all.x = TRUE)
merge_PTA <- merge(serie_fechas, data_estaciones_PTA, by = "date", all.x = TRUE)
merge_QUI <- merge(serie_fechas, data_estaciones_QUI, by = "date", all.x = TRUE)
merge_QUI_I <- merge(serie_fechas, data_estaciones_QUI_I, by = "date", all.x = TRUE)

df_rbind_estaciones<- rbind(merge_BSQ,merge_CDE,merge_CER_I,merge_CER_II, merge_CNA,
                            merge_FLD,merge_IND,merge_OHG,merge_PDH,merge_PTA,
                            merge_QUI,merge_QUI_I)

nrow(df_rbind_estaciones) == 517 * 12 # dias del periodo 2023-2024 * 12 estaciones


# elimino todo menos el ultimo merge/rbind
rm(list = setdiff(ls(), "df_rbind_estaciones"))
# Datos de prediccion
#prediccion_modelo <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/Prediccion_2015/Salida/Salida_03-XGB_cv_M1-041024/salida_03-XGB_cv_M1-041024_prediccion_2015.csv")
prediccion_modelo <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/Salidas/SalidasDiarias/Salida_03-XGB_cv_M1-041024/Salida_03-XGB_cv_M1-041024.csv")

prediccion_modelo <- prediccion_modelo[complete.cases(prediccion_modelo$valor_raster),]
prediccion_modelo$date <- as.POSIXct(as.character(prediccion_modelo$date), format = "%Y-%m-%d")


# Ejemplo: Hacer un merge entre puntos y valores_raster basado en dos columnas
data_merge <-df_rbind_estaciones  %>%
  left_join(prediccion_modelo , by = c("date", "estacion")) #%>%  # Merge basado en dos columnas


data_pm <- data_merge[complete.cases(data_merge$Registros.completos),]
data_pm <- data_pm[complete.cases(data_pm$valor_raster),]
data_pm <- data_pm[data_pm$valor_raster>0,]
data_pm<-data_merge
#data_pm$date <- as.POSIXct(data_pm$date, format = "%d/%m/%Y")
# Paso 1: Crear una serie temporal entre 01-01-2015 y 31-12-2015

# Paso 2: Asegurarse de que la columna 'date' en tu dataset original esté en formato Date


# Paso 3: Hacer un merge para unir la serie temporal con el dataset original
# Usamos all.x = TRUE para mantener todas las fechas de 'serie_fechas', incluso si no hay datos en 'data'
data_completo <- merge(data_pm, serie_fechas ,by = "date", all= TRUE)
#write.csv(data_completo, "D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/Prediccion_2015/Salida/Salida_03-XGB_cv_M1-041024/MERGE_salida_03-XGB_cv_M1-041024_prediccion_2015.csv")# Ver los primeros registros del nuevo dataset
data_completo <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/Prediccion_2015/Salida/Salida_03-XGB_cv_M1-041024/MERGE_salida_03-XGB_cv_M1-041024_prediccion_2015.csv")
data_completo$date <- as.POSIXct(data_completo$date, format = "%d/%m/%Y %H:%M")
data_completo$date <- as.Date(data_completo$date)

head(data_completo)



#########################################
#Series temporales por estacion 

# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)
# 365 DIAS * 9 ESTACUIONES ==> 3285
# Supongamos que ya tienes un dataframe llamado 'data' con las columnas 'date', 'estacion', 'Registros.validados' y 'valor_Raster'
data_completo <- data_pm
data_completo <- read.csv("D:/Josefina/Proyectos/ProyectoChile/SP/modelos/salidas/salidasDiarias/Salida_02-RF_cv_M1-131124_SP/Salida_02-RF_cv_M1-131124_SP_MERGED.csv")#,fileEncoding = "UTF-8")
# Convertir explícitamente la columna 'estacion' a UTF-8
data_completo$estacion <- iconv(data_completo$estacion.x, from = "UTF-8", to = "UTF-8")
data_completo$date <- as.POSIXct(data_completo$date, format = "%Y-%m-%d")
data_completo$date <- as.Date(data_completo$date)

# Asegurarse de que 'estacion' sea un factor
data_completo$estacion <- as.factor(data_completo$estacion.x)
data_completo$ID <- as.factor(data_completo$ID)
data_completo <- data_completo[complete.cases(data_completo$estacion),]
data_completo <- data_pm
data_completo <- data_merge_subt
# Crear el gráfico
plot <- ggplot(data_completo, aes(x = date)) +
  # Línea para Registros.validados
  # geom_line(aes(y = Registros.completos, color = "SINCA"), size = 0.5,na.rm = FALSE) +
  geom_line(aes(y = mean, color = "Observados"), size = 0.5,na.rm = FALSE) +
  # Línea para valor_Raster
  geom_line(aes(y = valor_raster, color = "Modelo"), size = 0.5, na.rm = FALSE)+#, linetype = "dashed") +
  # Separar en subplots por estación
  facet_wrap(~ estacion.x, scales = "free_y") +
   # 
  #scale_y_continuous(limits = c(0, 200),breaks = seq(0, 200, by = 50)) +  # Ticks cada 10 en el eje Y
  scale_y_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 20)) +  # Ticks cada 10 en el eje Y
  
  # Títulos y etiquetas
  labs(#title = "Modelo Salida_03-XGB_cv_M1-041024",
       x = "Date",
       y = "PM2.5",
       color = "Variables") +
  # Cambiar los colores de las líneas
  scale_color_manual(values = c("Observados" = "#ef3b2c", "Modelo" = "#3690c0")) +
  
  # Personalización del tema
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot

# Guardar el gráfico en formato PNG
ggsave("D:/Josefina/Proyectos/ProyectoChile/plots/SeriesTemporales/SerieTemp_Modelo Salida_03-XGB_cv_M1-041024.png", plot = plot, width = 10, height = 6, dpi = 500)


data_completo_2 <- data_completo[complete.cases(data_completo$valor_raster),]
data_completo_2 <- data_completo_2[complete.cases(data_completo_2$Registros.validados),]
data_completo_2 <- data_completo_2[complete.cases(data_completo_2$date),]
estacion <- "IND"
data_subst <- data_completo_2[data_completo_2$estacion == estacion,]
length(data_subst$valor_raster)
(length(data_subst$valor_raster)/365)*100


###########################################################################
############################################################################
###############################################################################

# Librerías necesarias
library(ggplot2)
library(dplyr)
library(broom)

# Ajuste del modelo de regresión lineal
modelo <- lm(valor_raster ~ Registros.completos, data = data_pm)

# Calculo de métricas de desempeño
R2 <- summary(modelo)$r.squared
RMSE <- sqrt(mean(residuals(modelo)^2))
Bias <- mean(data_pm$valor_raster - data_pm$Registros.completos)
n <- nrow(data_pm)

# Crear el gráfico con ggplot2
plot <- ggplot(data_pm, aes(x = Registros.completos, y = valor_raster)) +
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 200),breaks = seq(0, 200, by = 50)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 200),breaks = seq(0, 200, by = 50)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "SINCA",
    y = "Prediccion",
    title = "Salida_03-XGB_cv_M1-041024 - Periodo 2015-2024 - Total",
    subtitle = paste(
      "R2 =", round(R2, 3),
      "| RMSE =", round(RMSE, 2),
      "| Bias =", round(Bias, 2),
      "| n =", n
    )
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot
# Mostrar el gráfico
print(plot)

###################################################################################
######################################################################################
# Librerías necesarias
library(ggplot2)
library(dplyr)
library(broom)

# Librerías necesarias
library(ggplot2)
library(dplyr)
library(broom)


# Crear un data frame con métricas para cada estación
# Crear un data frame con métricas para cada estación sin usar broom
unique(data_merge_subt$estacion.x)
##### OHG
data_pm_OHG <- data_pm[data_pm$estacion == "OHG",]
# Ajuste del modelo de regresión lineal
modelo_OHG <- lm(valor_raster ~ Registros.completos, data = data_pm_OHG)
R2_OHG <- summary(modelo_OHG)$r.squared
RMSE_OHG <- sqrt(mean(residuals(modelo_OHG)^2))
Bias_OHG <- mean(data_pm_OHG$valor_raster - data_pm_OHG$Registros.completos)
n_OHG <- nrow(data_pm_OHG)


##### BSQ
data_pm_BSQ <- data_pm[data_pm$estacion == "BSQ",]
# Ajuste del modelo de regresión lineal
modelo_BSQ <- lm(valor_raster ~ Registros.completos, data = data_pm_BSQ)
R2_BSQ <- summary(modelo_BSQ)$r.squared
RMSE_BSQ <- sqrt(mean(residuals(modelo_BSQ)^2))
Bias_BSQ <- mean(data_pm_BSQ$valor_raster - data_pm_BSQ$Registros.completos)
n_BSQ <- nrow(data_pm_BSQ)


##### CNA
data_pm_CNA <- data_pm[data_pm$estacion == "CNA",]
# Ajuste del modelo de regresión lineal
modelo_CNA <- lm(valor_raster ~ Registros.completos, data = data_pm_CNA)
R2_CNA <- summary(modelo_CNA)$r.squared
RMSE_CNA <- sqrt(mean(residuals(modelo_CNA)^2))
Bias_CNA <- mean(data_pm_CNA$valor_raster - data_pm_CNA$Registros.completos)
n_CNA <- nrow(data_pm_CNA)


##### PDH
data_pm_PDH <- data_pm[data_pm$estacion == "PDH",]
# Ajuste del modelo de regresión lineal
modelo_PDH <- lm(valor_raster ~ Registros.completos, data = data_pm_PDH)
R2_PDH <- summary(modelo_PDH)$r.squared
RMSE_PDH <- sqrt(mean(residuals(modelo_PDH)^2))
Bias_PDH <- mean(data_pm_PDH$valor_raster - data_pm_PDH$Registros.completos)
n_PDH <- nrow(data_pm_PDH)


##### FLD
data_pm_FLD <- data_pm[data_pm$estacion == "FLD",]
# Ajuste del modelo de regresión lineal
modelo_FLD <- lm(valor_raster ~ Registros.completos, data = data_pm_FLD)
R2_FLD <- summary(modelo_FLD)$r.squared
RMSE_FLD <- sqrt(mean(residuals(modelo_FLD)^2))
Bias_FLD <- mean(data_pm_FLD$valor_raster - data_pm_FLD$Registros.completos)
n_FLD <- nrow(data_pm_FLD)

##### PTA
data_pm_PTA <- data_pm[data_pm$estacion == "PTA",]
# Ajuste del modelo de regresión lineal
modelo_PTA <- lm(valor_raster ~ Registros.completos, data = data_pm_PTA)
R2_PTA <- summary(modelo_PTA)$r.squared
RMSE_PTA <- sqrt(mean(residuals(modelo_PTA)^2))
Bias_PTA <- mean(data_pm_PTA$valor_raster - data_pm_PTA$Registros.completos)
n_PTA <- nrow(data_pm_PTA)

##### CDE
data_pm_CDE <- data_pm[data_pm$estacion == "CDE",]
# Ajuste del modelo de regresión lineal
modelo_CDE <- lm(valor_raster ~ Registros.completos, data = data_pm_CDE)
R2_CDE <- summary(modelo_CDE)$r.squared
RMSE_CDE <- sqrt(mean(residuals(modelo_CDE)^2))
Bias_CDE <- mean(data_pm_CDE$valor_raster - data_pm_CDE$Registros.completos)
n_CDE <- nrow(data_pm_CDE)


##### QUI
data_pm_QUI <- data_pm[data_pm$estacion == "QUI",]
# Ajuste del modelo de regresión lineal
modelo_QUI <- lm(valor_raster ~ Registros.completos, data = data_pm_QUI)
R2_QUI <- summary(modelo_QUI)$r.squared
RMSE_QUI <- sqrt(mean(residuals(modelo_QUI)^2))
Bias_QUI <- mean(data_pm_QUI$valor_raster - data_pm_QUI$Registros.completos)
n_QUI <- nrow(data_pm_QUI)

##### QUI-I
data_pm_QUI_I <- data_pm[data_pm$estacion == "QUI-I",]
# Ajuste del modelo de regresión lineal
modelo_QUI_I <- lm(valor_raster ~ Registros.completos, data = data_pm_QUI_I)
R2_QUI_I <- summary(modelo_QUI_I)$r.squared
RMSE_QUI_I <- sqrt(mean(residuals(modelo_QUI_I)^2))
Bias_QUI_I <- mean(data_pm_QUI_I$valor_raster - data_pm_QUI_I$Registros.completos)
n_QUI_I <- nrow(data_pm_QUI_I)


##### CER_I
data_pm_CER_I <- data_pm[data_pm$estacion == "CER-I",]
# Ajuste del modelo de regresión lineal
modelo_CER_I <- lm(valor_raster ~ Registros.completos, data = data_pm_CER_I)
R2_CER_I <- summary(modelo_CER_I)$r.squared
RMSE_CER_I <- sqrt(mean(residuals(modelo_CER_I)^2))
Bias_CER_I <- mean(data_pm_CER_I$valor_raster - data_pm_CER_I$Registros.completos)
n_CER_I <- nrow(data_pm_CER_I)

##### CER_II
data_pm_CER_II <- data_pm[data_pm$estacion == "CER-II",]
# Ajuste del modelo de regresión lineal
modelo_CER_II <- lm(valor_raster ~ Registros.completos, data = data_pm_CER_II)
R2_CER_II <- summary(modelo_CER_II)$r.squared
RMSE_CER_II <- sqrt(mean(residuals(modelo_CER_II)^2))
Bias_CER_II <- mean(data_pm_CER_II$valor_raster - data_pm_CER_II$Registros.completos)
n_CER_II <- nrow(data_pm_CER_II)

##### IND
data_pm_IND <- data_pm[data_pm$estacion == "IND",]
# Ajuste del modelo de regresión lineal
modelo_IND <- lm(valor_raster ~ Registros.completos, data = data_pm_IND)
R2_IND <- summary(modelo_IND)$r.squared
RMSE_IND <- sqrt(mean(residuals(modelo_IND)^2))
Bias_IND <- mean(data_pm_IND$valor_raster - data_pm_IND$Registros.completos)
n_IND <- nrow(data_pm_IND) 

########################
#########################
# Crear el gráfico con ggplot2
plot_OHG <- ggplot(data_pm_OHG , aes(x = Registros.completos, y = valor_raster)) +
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#+, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 200),breaks = seq(0, 200, by = 50)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 200),breaks = seq(0, 200, by = 50)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "SINCA",
    y = "Prediccion",
    title = "OHG",
    subtitle = paste(
      "R2 =", round(R2_OHG , 3),
      "| RMSE =", round(RMSE_OHG , 2),
      "| Bias =", round(Bias_OHG , 2),
      "| n =", n_OHG
    )
  ) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.7) +  # Línea 1:1 negra
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot_OHG
########################
#########################
# Crear el gráfico con ggplot2
plot_BSQ <- ggplot(data_pm_BSQ , aes(x = Registros.completos, y = valor_raster)) +
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 200),breaks = seq(0, 200, by = 50)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 200),breaks = seq(0, 200, by = 50)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "SINCA",
    y = "Prediccion",
    title = "BSQ",
    subtitle = paste(
      "R2 =", round(R2_BSQ , 3),
      "| RMSE =", round(RMSE_BSQ , 2),
      "| Bias =", round(Bias_BSQ , 2),
      "| n =", n_BSQ
    )
  ) +
  theme_classic() +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.7) +  # Línea 1:1 negra
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot_OHG
plot_BSQ


########################
#########################
# Crear el gráfico con ggplot2
plot_CNA <- ggplot(data_pm_CNA , aes(x = Registros.completos, y = valor_raster)) +
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 200),breaks = seq(0, 200, by = 50)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 200),breaks = seq(0, 200, by = 50)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "SINCA",
    y = "Prediccion",
    title = "CNA",
    subtitle = paste(
      "R2 =", round(R2_CNA , 3),
      "| RMSE =", round(RMSE_CNA , 2),
      "| Bias =", round(Bias_CNA , 2),
      "| n =", n_CNA
    )
  ) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.7) +  # Línea 1:1 negra
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot_OHG
plot_BSQ
plot_CNA



########################
#########################
# Crear el gráfico con ggplot2
plot_PDH <- ggplot(data_pm_PDH , aes(x = Registros.completos, y = valor_raster)) +
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 200),breaks = seq(0, 200, by = 50)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 200),breaks = seq(0, 200, by = 50)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "SINCA",
    y = "Prediccion",
    title = "PDH",
    subtitle = paste(
      "R2 =", round(R2_PDH , 3),
      "| RMSE =", round(RMSE_PDH , 2),
      "| Bias =", round(Bias_PDH , 2),
      "| n =", n_PDH
    )
  ) +
  theme_classic() +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.7) +  # Línea 1:1 negra
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot_OHG
plot_BSQ
plot_PDH
plot_PDH

########################
#########################
# Crear el gráfico con ggplot2
plot_FLD <- ggplot(data_pm_FLD , aes(x = Registros.completos, y = valor_raster)) +
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 200),breaks = seq(0, 200, by = 50)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 200),breaks = seq(0, 200, by = 50)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "SINCA",
    y = "Prediccion",
    title = "FLD",
    subtitle = paste(
      "R2 =", round(R2_FLD , 3),
      "| RMSE =", round(RMSE_FLD , 2),
      "| Bias =", round(Bias_FLD , 2),
      "| n =", n_FLD
    )
  ) +
  theme_classic() +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.7) +  # Línea 1:1 negra
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot_OHG
plot_BSQ
plot_PDH
plot_FLD



########################
#########################
# Crear el gráfico con ggplot2
plot_PTA <- ggplot(data_pm_PTA , aes(x = Registros.completos, y = valor_raster)) +
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 200),breaks = seq(0, 200, by = 50)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 200),breaks = seq(0, 200, by = 50)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "SINCA",
    y = "Prediccion",
    title = "PTA",
    subtitle = paste(
      "R2 =", round(R2_PTA , 3),
      "| RMSE =", round(RMSE_PTA , 2),
      "| Bias =", round(Bias_PTA , 2),
      "| n =", n_PTA
    )
  ) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.7) +  # Línea 1:1 negra
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot_OHG
plot_BSQ
plot_PDH
plot_FLD
plot_PTA



########################
#########################
# Crear el gráfico con ggplot2
plot_CDE <- ggplot(data_pm_CDE , aes(x = Registros.completos, y = valor_raster)) +
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 200),breaks = seq(0, 200, by = 50)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 200),breaks = seq(0, 200, by = 50)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "SINCA",
    y = "Prediccion",
    title = "CDE",
    subtitle = paste(
      "R2 =", round(R2_CDE , 3),
      "| RMSE =", round(RMSE_CDE , 2),
      "| Bias =", round(Bias_CDE , 2),
      "| n =", n_CDE
    )
  ) +
  theme_classic() +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.7) +  # Línea 1:1 negra
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot_OHG
plot_BSQ
plot_PDH
plot_FLD
plot_PTA
plot_CDE


########################
#########################
# Crear el gráfico con ggplot2
plot_CDE <- ggplot(data_pm_CDE , aes(x = Registros.completos, y = valor_raster)) +
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 200),breaks = seq(0, 200, by = 50)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 200),breaks = seq(0, 200, by = 50)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "SINCA",
    y = "Prediccion",
    title = "CDE",
    subtitle = paste(
      "R2 =", round(R2_CDE , 3),
      "| RMSE =", round(RMSE_CDE , 2),
      "| Bias =", round(Bias_CDE , 2),
      "| n =", n_CDE
    )
  ) +
  theme_classic() +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.7) +  # Línea 1:1 negra
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot_OHG
plot_BSQ
plot_PDH
plot_FLD
plot_PTA
plot_CDE



########################
#########################
# Crear el gráfico con ggplot2
plot_QUI <- ggplot(data_pm_QUI , aes(x = Registros.completos, y = valor_raster)) +
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 200),breaks = seq(0, 200, by = 50)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 200),breaks = seq(0, 200, by = 50)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "SINCA",
    y = "Prediccion",
    title = "QUI",
    subtitle = paste(
      "R2 =", round(R2_QUI , 3),
      "| RMSE =", round(RMSE_QUI , 2),
      "| Bias =", round(Bias_QUI , 2),
      "| n =", n_QUI
    )
  ) +
  theme_classic() +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.7) +  # Línea 1:1 negra
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot_OHG
plot_BSQ
plot_PDH
plot_FLD
plot_PTA
plot_CDE
plot_QUI



########################
#########################
# Crear el gráfico con ggplot2
plot_QUI_I <- ggplot(data_pm_QUI_I , aes(x = Registros.completos, y = valor_raster)) +
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 200),breaks = seq(0, 200, by = 50)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 200),breaks = seq(0, 200, by = 50)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "SINCA",
    y = "Prediccion",
    title = "QUI-I",
    subtitle = paste(
      "R2 =", round(R2_QUI_I , 3),
      "| RMSE =", round(RMSE_QUI_I , 2),
      "| Bias =", round(Bias_QUI_I , 2),
      "| n =", n_QUI_I
    )
  ) +
  theme_classic() +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.7) +  # Línea 1:1 negra
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot_OHG
plot_BSQ
plot_PDH
plot_FLD
plot_PTA
plot_CDE
plot_QUI
plot_QUI_I


########################
#########################
# Crear el gráfico con ggplot2
plot_CER_I <- ggplot(data_pm_CER_I , aes(x = Registros.completos, y = valor_raster)) +
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 200),breaks = seq(0, 200, by = 50)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 200),breaks = seq(0, 200, by = 50)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "SINCA",
    y = "Prediccion",
    title = "CER-I",
    subtitle = paste(
      "R2 =", round(R2_CER_I , 3),
      "| RMSE =", round(RMSE_CER_I , 2),
      "| Bias =", round(Bias_CER_I , 2),
      "| n =", n_CER_I
    )
  ) +
  theme_classic() +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.7) +  # Línea 1:1 negra
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot_OHG
plot_BSQ
plot_PDH
plot_FLD
plot_PTA
plot_CDE
plot_QUI
plot_QUI_I
plot_CER_I


########################
#########################
# Crear el gráfico con ggplot2
plot_CER_II <- ggplot(data_pm_CER_II , aes(x = Registros.completos, y = valor_raster)) +
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 200),breaks = seq(0, 200, by = 50)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 200),breaks = seq(0, 200, by = 50)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "SINCA",
    y = "Prediccion",
    title = "CER-II",
    subtitle = paste(
      "R2 =", round(R2_CER_II , 3),
      "| RMSE =", round(RMSE_CER_II , 2),
      "| Bias =", round(Bias_CER_II , 2),
      "| n =", n_CER_II
    )
  ) +
  theme_classic() +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.7) +  # Línea 1:1 negra
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot_OHG
plot_BSQ
plot_PDH
plot_FLD
plot_PTA
plot_CDE
plot_QUI
plot_QUI_I
plot_CER_I
plot_CER_II



########################
#########################
# Crear el gráfico con ggplot2
plot_IND <- ggplot(data_pm_IND , aes(x = Registros.completos, y = valor_raster)) +
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 200),breaks = seq(0, 200, by = 50)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 200),breaks = seq(0, 200, by = 50)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "SINCA",
    y = "Prediccion",
    title = "IND",
    subtitle = paste(
      "R2 =", round(R2_IND , 3),
      "| RMSE =", round(RMSE_IND , 2),
      "| Bias =", round(Bias_IND , 2),
      "| n =", n_IND
    )
  ) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.7) +  # Línea 1:1 negra
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot_OHG
plot_BSQ
plot_PDH
plot_FLD
plot_PTA
plot_CDE
plot_QUI
plot_QUI_I
plot_CER_I
plot_CER_II
plot_IND

length(unique(data_pm$estacion))

combined_plot_1 <- grid.arrange(plot_OHG, plot_BSQ,plot_PDH,
                              plot_FLD, plot_CNA, plot_PTA, 
                              nrow = 2, ncol = 3)
combined_plot_2 <- grid.arrange(plot_CDE, plot_QUI, plot_QUI_I,
                                plot_CER_I, plot_CER_II, plot_IND,
                                nrow = 2, ncol = 3)
combined_plot


##############################################################################
##################################################################################
# SP
unique(data_merge_subt$estacion.x)
##### Ciudad Universitaria - USP
data_pm_01 <- data_merge_subt[data_merge_subt$estacion.x == "Ciudad Universitaria - USP",]
# Ajuste del modelo de regresión lineal
modelo_01 <- lm(valor_raster ~ mean, data = data_pm_01)
R2_01 <- summary(modelo_01)$r.squared
RMSE_01 <- sqrt(mean(residuals(modelo_01)^2))
Bias_01 <- mean(data_pm_01$valor_raster - data_pm_01$mean)
n_01 <- nrow(data_pm_01)

####################
####################
unique(data_merge_subt$estacion.x)
##### Ciudad Universitaria - USP
data_pm_02 <- data_merge_subt[data_merge_subt$estacion.x == "Guarulhos-Paço Municipal",]
# Ajuste del modelo de regresión lineal
modelo_02 <- lm(valor_raster ~ mean, data = data_pm_02)
R2_02 <- summary(modelo_02)$r.squared
RMSE_02 <- sqrt(mean(residuals(modelo_02)^2))
Bias_02 <- mean(data_pm_02$valor_raster - data_pm_02$mean)
n_02 <- nrow(data_pm_02)


####################
####################
unique(data_merge_subt$estacion.x)
##### Guarulhos - Pimentas
data_pm_03 <- data_merge_subt[data_merge_subt$estacion.x == "Guarulhos - Pimentas",]
# Ajuste del modelo de regresión lineal
modelo_03 <- lm(valor_raster ~ mean, data = data_pm_03)
R2_03 <- summary(modelo_03)$r.squared
RMSE_03 <- sqrt(mean(residuals(modelo_03)^2))
Bias_03 <- mean(data_pm_03$valor_raster - data_pm_03$mean)
n_03 <- nrow(data_pm_03)


####################
####################
unique(data_merge_subt$estacion.x)
##### Ibirapuera
data_pm_04 <- data_merge_subt[data_merge_subt$estacion.x == "Ibirapuera",]
# Ajuste del modelo de regresión lineal
modelo_04 <- lm(valor_raster ~ mean, data = data_pm_04)
R2_04 <- summary(modelo_04)$r.squared
RMSE_04 <- sqrt(mean(residuals(modelo_04)^2))
Bias_04 <- mean(data_pm_04$valor_raster - data_pm_04$mean)
n_04 <- nrow(data_pm_04)


####################
####################
unique(data_merge_subt$estacion.x)
##### Ibirapuera
data_pm_05 <- data_merge_subt[data_merge_subt$estacion.x == "Interlagos",]
# Ajuste del modelo de regresión lineal
modelo_05 <- lm(valor_raster ~ mean, data = data_pm_05)
R2_05 <- summary(modelo_05)$r.squared
RMSE_05 <- sqrt(mean(residuals(modelo_05)^2))
Bias_05 <- mean(data_pm_05$valor_raster - data_pm_05$mean)
n_05 <- nrow(data_pm_05)

####################
####################
unique(data_merge_subt$estacion.x)
##### Itaim Paulista
data_pm_06 <- data_merge_subt[data_merge_subt$estacion.x == "Itaim Paulista",]
# Ajuste del modelo de regresión lineal
modelo_06 <- lm(valor_raster ~ mean, data = data_pm_06)
R2_06 <- summary(modelo_06)$r.squared
RMSE_06 <- sqrt(mean(residuals(modelo_06)^2))
Bias_06 <- mean(data_pm_06$valor_raster - data_pm_06$mean)
n_06 <- nrow(data_pm_06)

####################
####################
unique(data_merge_subt$estacion.x)
##### Itaim Paulista
data_pm_08 <- data_merge_subt[data_merge_subt$estacion.x == "Mauá",]
# Ajuste del modelo de regresión lineal
modelo_08 <- lm(valor_raster ~ mean, data = data_pm_08)
R2_08 <- summary(modelo_08)$r.squared
RMSE_08 <- sqrt(mean(residuals(modelo_08)^2))
Bias_08 <- mean(data_pm_08$valor_raster - data_pm_08$mean)
n_08 <- nrow(data_pm_08)

####################
####################
unique(data_merge_subt$estacion.x)
##### Itaim Paulista
data_pm_09 <- data_merge_subt[data_merge_subt$estacion.x == "Osasco",]
# Ajuste del modelo de regresión lineal
modelo_09 <- lm(valor_raster ~ mean, data = data_pm_09)
R2_09 <- summary(modelo_09)$r.squared
RMSE_09 <- sqrt(mean(residuals(modelo_09)^2))
Bias_09 <- mean(data_pm_09$valor_raster - data_pm_09$mean)
n_09 <- nrow(data_pm_09)

####################
####################
unique(data_merge_subt$estacion.x)
##### Itaim Paulista
data_pm_10 <- data_merge_subt[data_merge_subt$estacion.x == "Parque D.Pedro II",]
# Ajuste del modelo de regresión lineal
modelo_10 <- lm(valor_raster ~ mean, data = data_pm_10)
R2_10 <- summary(modelo_10)$r.squared
RMSE_10 <- sqrt(mean(residuals(modelo_10)^2))
Bias_10 <- mean(data_pm_10$valor_raster - data_pm_10$mean)
n_10 <- nrow(data_pm_10)

####################
####################
####################
unique(data_merge_subt$estacion.x)
##### Pico do Jaraguá
data_pm_11 <- data_merge_subt[data_merge_subt$estacion.x == "Pico do Jaraguá",]
# Ajuste del modelo de regresión lineal
modelo_11 <- lm(valor_raster ~ mean, data = data_pm_11)
R2_11 <- summary(modelo_11)$r.squared
RMSE_11 <- sqrt(mean(residuals(modelo_11)^2))
Bias_11 <- mean(data_pm_11$valor_raster - data_pm_11$mean)
n_11 <- nrow(data_pm_11)


####################
####################
####################
unique(data_merge_subt$estacion.x)
##### Pico do Jaraguá
data_pm_12 <- data_merge_subt[data_merge_subt$estacion.x == "Pinheiros",]
# Ajuste del modelo de regresión lineal
modelo_12 <- lm(valor_raster ~ mean, data = data_pm_12)
R2_12 <- summary(modelo_12)$r.squared
RMSE_12 <- sqrt(mean(residuals(modelo_12)^2))
Bias_12 <- mean(data_pm_12$valor_raster - data_pm_12$mean)
n_12 <- nrow(data_pm_12)

####################
####################
####################
unique(data_merge_subt$estacion.x)
##### Santana
data_pm_14 <- data_merge_subt[data_merge_subt$estacion.x == "Santo Amaro",]
# Ajuste del modelo de regresión lineal
modelo_14 <- lm(valor_raster ~ mean, data = data_pm_14)
R2_14 <- summary(modelo_14)$r.squared
RMSE_14 <- sqrt(mean(residuals(modelo_14)^2))
Bias_14 <- mean(data_pm_14$valor_raster - data_pm_14$mean)
n_14 <- nrow(data_pm_14)

####################
####################
####################
unique(data_merge_subt$estacion.x)
##### Taboão da Serra
data_pm_15 <- data_merge_subt[data_merge_subt$estacion.x == "Taboão da Serra",]
# Ajuste del modelo de regresión lineal
modelo_15 <- lm(valor_raster ~ mean, data = data_pm_15)
R2_15 <- summary(modelo_15)$r.squared
RMSE_15 <- sqrt(mean(residuals(modelo_15)^2))
Bias_15 <- mean(data_pm_15$valor_raster - data_pm_15$mean)
n_15 <- nrow(data_pm_15)

####################
####################
unique(data_merge_subt$estacion.x)
##### Carapicuíba
data_pm_16 <- data_merge_subt[data_merge_subt$estacion.x == "Carapicuíba",]
# Ajuste del modelo de regresión lineal
modelo_16 <- lm(valor_raster ~ mean, data = data_pm_16)
R2_16 <- summary(modelo_16)$r.squared
RMSE_16 <- sqrt(mean(residuals(modelo_16)^2))
Bias_16 <- mean(data_pm_16$valor_raster - data_pm_16$mean)
n_16 <- nrow(data_pm_16)


####################################
########################
#########################
unique(data_pm_01$estacion.x)
# Crear el gráfico con ggplot2
plot_01 <- ggplot(data_pm_01 , aes(x = mean, y = valor_raster)) +
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 20)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 520)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "Observado",
    y = "Prediccion",
    title = "Ciudad Universitaria - USP",
    subtitle = paste(
      "R2 =", round(R2_01 , 3),
      "| RMSE =", round(RMSE_01 , 2),
      "| Bias =", round(Bias_01 , 2),
      "| n =", n_01
    )
  ) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.7) +  # Línea 1:1 negra
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotar etiquetas del eje X
    plot.subtitle = element_text(size = 8)  # Cambiar tamaño del subtítulo
  )
plot_01


####################################
########################
#########################
unique(data_pm_02$estacion.x)
# Crear el gráfico con ggplot2
plot_02 <- ggplot(data_pm_02 , aes(x = mean, y = valor_raster)) +
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 20)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 520)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "Observado",
    y = "Prediccion",
    title = "Guarulhos-Paço Municipal",
    subtitle = paste(
      "R2 =", round(R2_02 , 3),
      "| RMSE =", round(RMSE_02 , 2),
      "| Bias =", round(Bias_02 , 2),
      "| n =", n_02
    )
  ) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.7) +  # Línea 1:1 negra
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotar etiquetas del eje X
    plot.subtitle = element_text(size = 8)  # Cambiar tamaño del subtítulo
  )
plot_02


########################
#########################
unique(data_pm_03$estacion.x)
# Crear el gráfico con ggplot2
plot_03 <- ggplot(data_pm_03 , aes(x = mean, y = valor_raster)) +
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 20)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 520)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "Observado",
    y = "Prediccion",
    title = "Guarulhos - Pimentas",
    subtitle = paste(
      "R2 =", round(R2_03 , 3),
      "| RMSE =", round(RMSE_03 , 2),
      "| Bias =", round(Bias_03 , 2),
      "| n =", n_03
    )
  ) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.7) +  # Línea 1:1 negra
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotar etiquetas del eje X
    plot.subtitle = element_text(size = 8)  # Cambiar tamaño del subtítulo
  )
plot_03

########################
#########################
unique(data_pm_04$estacion.x)
# Crear el gráfico con ggplot2
plot_04 <- ggplot(data_pm_04 , aes(x = mean, y = valor_raster)) +
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 20)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 520)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "Observado",
    y = "Prediccion",
    title = "Ibirapuera",
    subtitle = paste(
      "R2 =", round(R2_04 , 3),
      "| RMSE =", round(RMSE_04 , 2),
      "| Bias =", round(Bias_04 , 2),
      "| n =", n_04
    )
  ) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.7) +  # Línea 1:1 negra
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotar etiquetas del eje X
    plot.subtitle = element_text(size = 8)  # Cambiar tamaño del subtítulo
  )
plot_04


########################
#########################
unique(data_pm_05$estacion.x)
# Crear el gráfico con ggplot2
plot_05 <- ggplot(data_pm_05 , aes(x = mean, y = valor_raster)) +
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 20)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 520)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "Observado",
    y = "Prediccion",
    title = "Interlagos",
    subtitle = paste(
      "R2 =", round(R2_05 , 3),
      "| RMSE =", round(RMSE_05 , 2),
      "| Bias =", round(Bias_05 , 2),
      "| n =", n_05
    )
  ) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.7) +  # Línea 1:1 negra
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotar etiquetas del eje X
    plot.subtitle = element_text(size = 8)  # Cambiar tamaño del subtítulo
  )
plot_05


########################
#########################
unique(data_pm_06$estacion.x)
# Crear el gráfico con ggplot2
plot_06 <- ggplot(data_pm_06 , aes(x = mean, y = valor_raster)) +
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 20)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 520)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "Observado",
    y = "Prediccion",
    title = "Itaim Paulista",
    subtitle = paste(
      "R2 =", round(R2_06 , 3),
      "| RMSE =", round(RMSE_06 , 2),
      "| Bias =", round(Bias_06 , 2),
      "| n =", n_06
    )
  ) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.7) +  # Línea 1:1 negra
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotar etiquetas del eje X
    plot.subtitle = element_text(size = 8)  # Cambiar tamaño del subtítulo
  )
plot_06


########################
#########################
unique(data_pm_07$estacion.x)
# Crear el gráfico con ggplot2
plot_07 <- ggplot(data_pm_07 , aes(x = mean, y = valor_raster)) +
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 20)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 520)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "Observado",
    y = "Prediccion",
    title = "Marginal Tietê - Ponte dos Remédios",
    subtitle = paste(
      "R2 =", round(R2_07 , 3),
      "| RMSE =", round(RMSE_07 , 2),
      "| Bias =", round(Bias_07 , 2),
      "| n =", n_07
    )
  ) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.7) +  # Línea 1:1 negra
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotar etiquetas del eje X
    plot.subtitle = element_text(size = 8)  # Cambiar tamaño del subtítulo
  )
plot_07


########################
#########################
unique(data_pm_08$estacion.x)
# Crear el gráfico con ggplot2
plot_08 <- ggplot(data_pm_08 , aes(x = mean, y = valor_raster)) +
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 20)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 520)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "Observado",
    y = "Prediccion",
    title = "Mauá",
    subtitle = paste(
      "R2 =", round(R2_08 , 3),
      "| RMSE =", round(RMSE_08 , 2),
      "| Bias =", round(Bias_08 , 2),
      "| n =", n_08
    )
  ) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.7) +  # Línea 1:1 negra
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotar etiquetas del eje X
    plot.subtitle = element_text(size = 8)  # Cambiar tamaño del subtítulo
  )
plot_08


########################
#########################
unique(data_pm_09$estacion.x)
# Crear el gráfico con ggplot2
plot_09 <- ggplot(data_pm_09 , aes(x = mean, y = valor_raster)) +
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 20)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 20)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "Observado",
    y = "Prediccion",
    title = "Osasco",
    subtitle = paste(
      "R2 =", round(R2_09 , 3),
      "| RMSE =", round(RMSE_09 , 2),
      "| Bias =", round(Bias_09 , 2),
      "| n =", n_09
    )
  ) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.7) +  # Línea 1:1 negra
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotar etiquetas del eje X
    plot.subtitle = element_text(size = 8)  # Cambiar tamaño del subtítulo
  )
plot_09


########################
#########################
unique(data_pm_10$estacion.x)
# Crear el gráfico con ggplot2
plot_10 <- ggplot(data_pm_10 , aes(x = mean, y = valor_raster)) +
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 20)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 20)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "Observado",
    y = "Prediccion",
    title = "Parque D.Pedro II",
    subtitle = paste(
      "R2 =", round(R2_10 , 3),
      "| RMSE =", round(RMSE_10 , 2),
      "| Bias =", round(Bias_10 , 2),
      "| n =", n_10
    )
  ) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.7) +  # Línea 1:1 negra
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotar etiquetas del eje X
    plot.subtitle = element_text(size = 8)  # Cambiar tamaño del subtítulo
  )
plot_10

#######################
#########################
unique(data_pm_11$estacion.x)
# Crear el gráfico con ggplot2
plot_11 <- ggplot(data_pm_11 , aes(x = mean, y = valor_raster)) +
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 20)) +  # Ticks cada 16 en el eje Y
  scale_x_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 20)) +  # Ticks cada 16 en el eje Y
  labs(
    x = "Observado",
    y = "Prediccion",
    title = "Pico do Jaraguá",
    subtitle = paste(
      "R2 =", round(R2_11 , 3),
      "| RMSE =", round(RMSE_11 , 2),
      "| Bias =", round(Bias_11 , 2),
      "| n =", n_11
    )
  ) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.7) +  # Línea 1:1 negra
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotar etiquetas del eje X
    plot.subtitle = element_text(size = 8)  # Cambiar tamaño del subtítulo
  )
plot_11


#######################
#########################
unique(data_pm_12$estacion.x)
# Crear el gráfico con ggplot2
plot_12 <- ggplot(data_pm_12 , aes(x = mean, y = valor_raster)) +
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 20)) +  # Ticks cada 16 en el eje Y
  scale_x_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 20)) +  # Ticks cada 16 en el eje Y
  labs(
    x = "Observado",
    y = "Prediccion",
    title = "Pinheiros",
    subtitle = paste(
      "R2 =", round(R2_12 , 3),
      "| RMSE =", round(RMSE_12 , 2),
      "| Bias =", round(Bias_12 , 2),
      "| n =", n_12
    )
  ) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.7) +  # Línea 1:1 negra
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotar etiquetas del eje X
    plot.subtitle = element_text(size = 8)  # Cambiar tamaño del subtítulo
  )
plot_12


#######################
#########################
unique(data_pm_13$estacion.x)
# Crear el gráfico con ggplot2
plot_13 <- ggplot(data_pm_13 , aes(x = mean, y = valor_raster)) +
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 20)) +  # Ticks cada 16 en el eje Y
  scale_x_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 20)) +  # Ticks cada 16 en el eje Y
  labs(
    x = "Observado",
    y = "Prediccion",
    title = "Santana",
    subtitle = paste(
      "R2 =", round(R2_13 , 3),
      "| RMSE =", round(RMSE_13 , 2),
      "| Bias =", round(Bias_13 , 2),
      "| n =", n_13
    )
  ) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.7) +  # Línea 1:1 negra
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotar etiquetas del eje X
    plot.subtitle = element_text(size = 8)  # Cambiar tamaño del subtítulo
  )
plot_13



#######################
#########################
unique(data_pm_14$estacion.x)
# Crear el gráfico con ggplot2
plot_14 <- ggplot(data_pm_14 , aes(x = mean, y = valor_raster)) +
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 20)) +  # Ticks cada 14 en el eje Y
  scale_x_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 20)) +  # Ticks cada 14 en el eje Y
  labs(
    x = "Observado",
    y = "Prediccion",
    title = "Santo Amaro",
    subtitle = paste(
      "R2 =", round(R2_14 , 3),
      "| RMSE =", round(RMSE_14 , 2),
      "| Bias =", round(Bias_14 , 2),
      "| n =", n_14
    )
  ) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.7) +  # Línea 1:1 negra
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotar etiquetas del eje X
    plot.subtitle = element_text(size = 8)  # Cambiar tamaño del subtítulo
  )
plot_14

#######################
#########################
unique(data_pm_15$estacion.x)
# Crear el gráfico con ggplot2
plot_15 <- ggplot(data_pm_15 , aes(x = mean, y = valor_raster)) +
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 20)) +  # Ticks cada 15 en el eje Y
  scale_x_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 20)) +  # Ticks cada 15 en el eje Y
  labs(
    x = "Observado",
    y = "Prediccion",
    title = "Taboão da Serra",
    subtitle = paste(
      "R2 =", round(R2_15 , 3),
      "| RMSE =", round(RMSE_15 , 2),
      "| Bias =", round(Bias_15 , 2),
      "| n =", n_15
    )
  ) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.7) +  # Línea 1:1 negra
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotar etiquetas del eje X
    plot.subtitle = element_text(size = 8)  # Cambiar tamaño del subtítulo
  )
plot_15


#######################
#########################
unique(data_pm_16$estacion.x)
# Crear el gráfico con ggplot2
plot_16 <- ggplot(data_pm_16 , aes(x = mean, y = valor_raster)) +
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 20)) +  # Ticks cada 16 en el eje Y
  scale_x_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 20)) +  # Ticks cada 16 en el eje Y
  labs(
    x = "Observado",
    y = "Prediccion",
    title = "Carapicuíba",
    subtitle = paste(
      "R2 =", round(R2_16 , 3),
      "| RMSE =", round(RMSE_16 , 2),
      "| Bias =", round(Bias_16 , 2),
      "| n =", n_16
    )
  ) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.7) +  # Línea 1:1 negra
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotar etiquetas del eje X
    plot.subtitle = element_text(size = 8)  # Cambiar tamaño del subtítulo
  )
plot_16




length(unique(data_pm$estacion))

combined_plot_1 <- grid.arrange(plot_01, plot_02,plot_03,
                                plot_04, plot_05, plot_06, 
                                nrow = 2, ncol = 3)
combined_plot_2 <- grid.arrange(plot_07, plot_08, plot_09,
                                plot_10, plot_11, plot_12,
                                nrow = 2, ncol = 3)

combined_plot_3 <- grid.arrange(plot_13, plot_14, plot_15,
                                plot_16,
                                nrow = 2, ncol = 2)
combined_plot


#########################################################
######################################################
#Analsiis de los LCS
data_ch <- read.csv("D:/Josefina/Proyectos/ProyectoChile/CH/modelos/Salidas/Sensores/sensores_modelos_validacion.csv")
data_merge_subt<- data_ch
unique(data_merge_subt$archivo)
#################################
##### Quilicura_verano
data_Quilicura_verano <- data_merge_subt[data_merge_subt$archivo == "Quilicura Verano",]
# Ajuste del modelo de regresión lineal
modelo_Quilicura_verano <- lm(mean ~ valor_raster, data = data_Quilicura_verano)

R2_Quilicura_verano <- summary(modelo_Quilicura_verano)$r.squared
RMSE_Quilicura_verano <- sqrt(mean(residuals(modelo_Quilicura_verano)^2))
Bias_Quilicura_verano <- mean(data_Quilicura_verano$mean - data_Quilicura_verano$valor_raster)
n_Quilicura_verano <- nrow(data_Quilicura_verano)

# Crear el gráfico con ggplot2
plot_Quilicura_verano <- ggplot(data_Quilicura_verano , aes(x = valor_raster, y = mean)) +
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#+, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 50),breaks = seq(0, 50, by = 10)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 50),breaks = seq(0, 50, by = 10)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "Modelo",
    y = "LCS",
    title = "Quilicura Verano",
    subtitle = paste(
      "R2 =", round(R2_Quilicura_verano , 3),
      "| RMSE =", round(RMSE_Quilicura_verano , 2),
      "| Bias =", round(Bias_Quilicura_verano , 2),
      "| n =", n_Quilicura_verano
    )
  ) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.3) +  # Línea 1:1 negra
  theme_classic() +
  theme(
    plot.title = element_text(size = 10, face = "bold"),        # Tamaño del título
    plot.subtitle = element_text(size = 8),                   # Tamaño del subtítulo
    axis.title = element_text(size = 8),                      # Tamaño de los títulos de los ejes
    axis.text = element_text(size = 6),                        # Tamaño de los textos de los ejes
    axis.ticks.length = unit(0.1, "cm"),                      # Tamaño de los ticks
    axis.line = element_line(size = 0.2)                       # Grosor de las líneas de los ejes
  )
plot_Quilicura_verano

###########################################
##### Quilicura_invierno
data_Quilicura_invierno <- data_merge_subt[data_merge_subt$archivo == "Quilicura_invierno",]
# Ajuste del modelo de regresión lineal
modelo_Quilicura_invierno <- lm(mean ~ valor_raster, data = data_Quilicura_invierno)

R2_Quilicura_invierno <- summary(modelo_Quilicura_invierno)$r.squared
RMSE_Quilicura_invierno <- sqrt(mean(residuals(modelo_Quilicura_invierno)^2))
Bias_Quilicura_invierno <- mean(data_Quilicura_invierno$mean - data_Quilicura_invierno$valor_raster)
n_Quilicura_invierno <- nrow(data_Quilicura_invierno)

# Crear el gráfico con ggplot2
plot_Quilicura_invierno <- ggplot(data_Quilicura_invierno , aes(x = valor_raster, y = mean)) +
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#+, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 180),breaks = seq(0, 180, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 180),breaks = seq(0, 180, by = 40)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "Modelo",
    y = "LCS",
    title = "Quilicura Invierno",
    subtitle = paste(
      "R2 =", round(R2_Quilicura_invierno , 3),
      "| RMSE =", round(RMSE_Quilicura_invierno , 2),
      "| Bias =", round(Bias_Quilicura_invierno , 2),
      "| n =", n_Quilicura_invierno
    )
  ) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.3) +  # Línea 1:1 negra
  theme_classic() +
  theme(
    plot.title = element_text(size = 10, face = "bold"),        # Tamaño del título
    plot.subtitle = element_text(size = 8),                   # Tamaño del subtítulo
    axis.title = element_text(size = 8),                      # Tamaño de los títulos de los ejes
    axis.text = element_text(size = 6),                        # Tamaño de los textos de los ejes
    axis.ticks.length = unit(0.1, "cm"),                      # Tamaño de los ticks
    axis.line = element_line(size = 0.2)                       # Grosor de las líneas de los ejes
  )
plot_Quilicura_invierno




#################################
##### Vitacura_Invierno
data_Vitacura_Invierno <- data_merge_subt[data_merge_subt$archivo == "Vitacura Invierno",]
# Ajuste del modelo de regresión lineal
modelo_Vitacura_Invierno <- lm(mean ~ valor_raster, data = data_Vitacura_Invierno)

R2_Vitacura_Invierno <- summary(modelo_Vitacura_Invierno)$r.squared
RMSE_Vitacura_Invierno <- sqrt(mean(residuals(modelo_Vitacura_Invierno)^2))
Bias_Vitacura_Invierno <- mean(data_Vitacura_Invierno$mean - data_Vitacura_Invierno$valor_raster)
n_Vitacura_Invierno <- nrow(data_Vitacura_Invierno)

# Crear el gráfico con ggplot2
plot_Vitacura_Invierno <- ggplot(data_Vitacura_Invierno , aes(x = valor_raster, y = mean)) +
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#+, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 180),breaks = seq(0, 180, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 180),breaks = seq(0, 180, by = 40)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "Modelo",
    y = "LCS",
    title = "Vitacura Invierno",
    subtitle = paste(
      "R2 =", round(R2_Vitacura_Invierno , 3),
      "| RMSE =", round(RMSE_Vitacura_Invierno , 2),
      "| Bias =", round(Bias_Vitacura_Invierno , 2),
      "| n =", n_Vitacura_Invierno
    )
  ) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.3) +  # Línea 1:1 negra
  theme_classic() +
  theme(
    plot.title = element_text(size = 10, face = "bold"),        # Tamaño del título
    plot.subtitle = element_text(size = 8),                   # Tamaño del subtítulo
    axis.title = element_text(size = 8),                      # Tamaño de los títulos de los ejes
    axis.text = element_text(size = 6),                        # Tamaño de los textos de los ejes
    axis.ticks.length = unit(0.1, "cm"),                      # Tamaño de los ticks
    axis.line = element_line(size = 0.2)                       # Grosor de las líneas de los ejes
  )
plot_Vitacura_Invierno

#################################
unique(data_merge_subt$archivo)
##### Vitacura_verano
data_Vitacura_verano <- data_merge_subt[data_merge_subt$archivo == "vitacura verano",]
# Ajuste del modelo de regresión lineal
modelo_Vitacura_verano <- lm(mean ~ valor_raster, data = data_Vitacura_verano)

R2_Vitacura_verano <- summary(modelo_Vitacura_verano)$r.squared
RMSE_Vitacura_verano <- sqrt(mean(residuals(modelo_Vitacura_verano)^2))
Bias_Vitacura_verano <- mean(data_Vitacura_verano$mean - data_Vitacura_verano$valor_raster)
n_Vitacura_verano <- nrow(data_Vitacura_verano)

# Crear el gráfico con ggplot2
plot_Vitacura_verano <- ggplot(data_Vitacura_verano , aes(x = valor_raster, y = mean)) +
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#+, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 50),breaks = seq(0, 50, by = 10)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 50),breaks = seq(0, 50, by = 10)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "Modelo",
    y = "LCS",
    title = "Vitacura Verano",
    subtitle = paste(
      "R2 =", round(R2_Vitacura_verano , 3),
      "| RMSE =", round(RMSE_Vitacura_verano , 2),
      "| Bias =", round(Bias_Vitacura_verano , 2),
      "| n =", n_Vitacura_verano
    )
  ) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.3) +  # Línea 1:1 negra
  theme_classic() +
  theme(
    plot.title = element_text(size = 10, face = "bold"),        # Tamaño del título
    plot.subtitle = element_text(size = 8),                   # Tamaño del subtítulo
    axis.title = element_text(size = 8),                      # Tamaño de los títulos de los ejes
    axis.text = element_text(size = 6),                        # Tamaño de los textos de los ejes
    axis.ticks.length = unit(0.1, "cm"),                      # Tamaño de los ticks
    axis.line = element_line(size = 0.2)                       # Grosor de las líneas de los ejes
  )
plot_Vitacura_verano


#################################
unique(data_merge_subt$archivo)
##### florida_verano
data_florida_verano <- data_merge_subt[data_merge_subt$archivo == "Florida Verano",]
# Ajuste del modelo de regresión lineal
modelo_florida_verano <- lm(mean ~ valor_raster, data = data_florida_verano)

R2_florida_verano <- summary(modelo_florida_verano)$r.squared
RMSE_florida_verano <- sqrt(mean(residuals(modelo_florida_verano)^2))
Bias_florida_verano <- mean(data_florida_verano$mean - data_florida_verano$valor_raster)
n_florida_verano <- nrow(data_florida_verano)

# Crear el gráfico con ggplot2
plot_florida_verano <- ggplot(data_florida_verano , aes(x = valor_raster, y = mean)) +
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#+, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 50),breaks = seq(0, 50, by = 10)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 50),breaks = seq(0, 50, by = 10)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "Modelo",
    y = "LCS",
    title = "Florida Verano",
    subtitle = paste(
      "R2 =", round(R2_florida_verano , 3),
      "| RMSE =", round(RMSE_florida_verano , 2),
      "| Bias =", round(Bias_florida_verano , 2),
      "| n =", n_florida_verano
    )
  ) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.3) +  # Línea 1:1 negra
  theme_classic() +
  theme(
    plot.title = element_text(size = 10, face = "bold"),        # Tamaño del título
    plot.subtitle = element_text(size = 8),                   # Tamaño del subtítulo
    axis.title = element_text(size = 8),                      # Tamaño de los títulos de los ejes
    axis.text = element_text(size = 6),                        # Tamaño de los textos de los ejes
    axis.ticks.length = unit(0.1, "cm"),                      # Tamaño de los ticks
    axis.line = element_line(size = 0.2)                       # Grosor de las líneas de los ejes
  )
plot_florida_verano


#################################
unique(data_merge_subt$archivo)
##### florida_invierno
data_florida_invierno <- data_merge_subt[data_merge_subt$archivo == "Florida Invierno",]
# Ajuste del modelo de regresión lineal
modelo_florida_invierno <- lm(mean ~ valor_raster, data = data_florida_invierno)

R2_florida_invierno <- summary(modelo_florida_invierno)$r.squared
RMSE_florida_invierno <- sqrt(mean(residuals(modelo_florida_invierno)^2))
Bias_florida_invierno <- mean(data_florida_invierno$mean - data_florida_invierno$valor_raster)
n_florida_invierno <- nrow(data_florida_invierno)

# Crear el gráfico con ggplot2
plot_florida_invierno <- ggplot(data_florida_invierno , aes(x = valor_raster, y = mean)) +
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#+, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 180),breaks = seq(0, 180, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 180),breaks = seq(0, 180, by = 40)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "Modelo",
    y = "LCS",
    title = "Florida Invierno",
    subtitle = paste(
      "R2 =", round(R2_florida_invierno , 3),
      "| RMSE =", round(RMSE_florida_invierno , 2),
      "| Bias =", round(Bias_florida_invierno , 2),
      "| n =", n_florida_invierno
    )
  ) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.3) +  # Línea 1:1 negra
  theme_classic() +
  theme(
    plot.title = element_text(size = 10, face = "bold"),        # Tamaño del título
    plot.subtitle = element_text(size = 8),                   # Tamaño del subtítulo
    axis.title = element_text(size = 8),                      # Tamaño de los títulos de los ejes
    axis.text = element_text(size = 6),                        # Tamaño de los textos de los ejes
    axis.ticks.length = unit(0.1, "cm"),                      # Tamaño de los ticks
    axis.line = element_line(size = 0.2)                       # Grosor de las líneas de los ejes
  )
plot_florida_invierno



#################################
unique(data_merge_subt$archivo)
##### maipu_invierno
data_maipu_invierno <- data_merge_subt[data_merge_subt$archivo == "Maipu Invierno",]
# Ajuste del modelo de regresión lineal
modelo_maipu_invierno <- lm(mean ~ valor_raster, data = data_maipu_invierno)

R2_maipu_invierno <- summary(modelo_maipu_invierno)$r.squared
RMSE_maipu_invierno <- sqrt(mean(residuals(modelo_maipu_invierno)^2))
Bias_maipu_invierno <- mean(data_maipu_invierno$mean - data_maipu_invierno$valor_raster)
n_maipu_invierno <- nrow(data_maipu_invierno)

# Crear el gráfico con ggplot2
plot_maipu_invierno <- ggplot(data_maipu_invierno , aes(x = valor_raster, y = mean)) +
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#+, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 180),breaks = seq(0, 180, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 180),breaks = seq(0, 180, by = 40)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "Modelo",
    y = "LCS",
    title = "Maipu Invierno",
    subtitle = paste(
      "R2 =", round(R2_maipu_invierno , 3),
      "| RMSE =", round(RMSE_maipu_invierno , 2),
      "| Bias =", round(Bias_maipu_invierno , 2),
      "| n =", n_maipu_invierno
    )
  ) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.3) +  # Línea 1:1 negra
  theme_classic() +
  theme(
    plot.title = element_text(size = 10, face = "bold"),        # Tamaño del título
    plot.subtitle = element_text(size = 8),                   # Tamaño del subtítulo
    axis.title = element_text(size = 8),                      # Tamaño de los títulos de los ejes
    axis.text = element_text(size = 6),                        # Tamaño de los textos de los ejes
    axis.ticks.length = unit(0.1, "cm"),                      # Tamaño de los ticks
    axis.line = element_line(size = 0.2)                       # Grosor de las líneas de los ejes
  )
plot_maipu_invierno


#################################
unique(data_merge_subt$archivo)
##### maipu_verano
data_maipu_verano <- data_merge_subt[data_merge_subt$archivo == "Maipu Verano",]
# Ajuste del modelo de regresión lineal
modelo_maipu_verano <- lm(mean ~ valor_raster, data = data_maipu_verano)

R2_maipu_verano <- summary(modelo_maipu_verano)$r.squared
RMSE_maipu_verano <- sqrt(mean(residuals(modelo_maipu_verano)^2))
Bias_maipu_verano <- mean(data_maipu_verano$mean - data_maipu_verano$valor_raster)
n_maipu_verano <- nrow(data_maipu_verano)

# Crear el gráfico con ggplot2
plot_maipu_verano <- ggplot(data_maipu_verano, aes(x = valor_raster, y = mean)) +
  geom_point(color = "#3690c0", size = 1, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE) +  # Línea de regresión
  scale_y_continuous(limits = c(0, 50),breaks = seq(0, 50, by = 10)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 50),breaks = seq(0, 50, by = 10)) +  # Ticks cada 10 en el eje Y
  
  labs(
    x = "Modelo",
    y = "LCS",
    title = "Maipu Verano",
    subtitle = paste(
      "R2 =", round(R2_maipu_verano, 2),
      "| RMSE =", round(RMSE_maipu_verano, 2),
      "| Bias =", round(Bias_maipu_verano, 2),
      "| n =", n_maipu_verano
    )
  ) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.3) +  # Línea 1:1 negra
  theme_classic() +
  theme(
    plot.title = element_text(size = 10, face = "bold"),        # Tamaño del título
    plot.subtitle = element_text(size = 8),                   # Tamaño del subtítulo
    axis.title = element_text(size = 8),                      # Tamaño de los títulos de los ejes
    axis.text = element_text(size = 6),                        # Tamaño de los textos de los ejes
    axis.ticks.length = unit(0.1, "cm"),                      # Tamaño de los ticks
    axis.line = element_line(size = 0.2)                       # Grosor de las líneas de los ejes
  )
plot_maipu_verano


combined_plot_1 <- grid.arrange(plot_maipu_verano, plot_maipu_invierno,
                                plot_florida_verano, plot_florida_invierno,
                                nrow = 2, ncol = 2)

combined_plot_2 <- grid.arrange(plot_Vitacura_verano, plot_Vitacura_Invierno,
                                plot_Quilicura_verano, plot_Quilicura_invierno,
                                nrow = 2, ncol = 2)
