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
# Asegurarse de que 'estacion' sea un factor
data_completo$estacion <- as.factor(data_completo$estacion)
data_completo <- data_completo[complete.cases(data_completo$estacion),]
data_completo <- data_pm

# Crear el gráfico
plot <- ggplot(data_completo, aes(x = date)) +
  # Línea para Registros.validados
  geom_line(aes(y = Registros.completos, color = "SINCA"), size = 0.5,na.rm = FALSE) +
  # Línea para valor_Raster
  geom_line(aes(y = valor_raster, color = "Modelo"), size = 0.5, na.rm = FALSE)+#, linetype = "dashed") +
  # Separar en subplots por estación
  facet_wrap(~ estacion, scales = "free_y") +
   # 
  scale_y_continuous(limits = c(0, 200),breaks = seq(0, 200, by = 50)) +  # Ticks cada 10 en el eje Y
  
  # Títulos y etiquetas
  labs(#title = "Modelo Salida_03-XGB_cv_M1-041024",
       x = "Date",
       y = "PM2.5",
       color = "Variables") +
  # Cambiar los colores de las líneas
  scale_color_manual(values = c("SINCA" = "#ef3b2c", "Modelo" = "#3690c0")) +
  
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
