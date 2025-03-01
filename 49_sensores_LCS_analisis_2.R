#################################
##### Quilicura_verano
data_LCS_Qui_verano <- data_LCS[data_LCS$archivo == "Quilicura Verano",]
data_SINCA_Qui <- data_SINCA[data_SINCA$estacion_SINCA == "QUI",]
data_LCS_Modelo_Qui_verano <- data_LCS_modelo[data_LCS_modelo$archivo == "Quilicura Verano",]

#meduciones LCS
data_LCS_Qui_verano$date <- as.POSIXct(as.character(data_LCS_Qui_verano$date), format = "%d/%m/%Y" )#"%y%m%d")
data_SINCA_Qui$date <- as.POSIXct(as.character(data_SINCA_Qui$date), format = "%d/%m/%Y" )#"%y%m%d")

data_LCS_Modelo_Qui_verano$date <- as.POSIXct(as.character(data_LCS_Modelo_Qui_verano$date), format = "%Y-%m-%d" )#"%y%m%d")


data_SINCA_Qui <- data_SINCA_Qui[year(data_SINCA_Qui$date) == 2021,]
#UniSINCA - Meidiones LCS
merge_Qui_verano <- merge(data_SINCA_Qui, data_LCS_Qui_verano, by = "date", all.y = TRUE)
#Unir SINCA y Medicioones LCS - Modelo en sitios lCS
names(merge_Qui_verano)
names(data_LCS_Modelo_Qui_verano)

unique(data_LCS_Modelo_Qui_verano$ID_archivo)
data_LCS_Modelo_Qui_verano$ID_LCS <- data_LCS_Modelo_Qui_verano$ID_archivo
length(unique(merge_Qui_verano$ID_LCS))


merge_Qui_verano_2 <-merge(merge_Qui_verano, data_LCS_Modelo_Qui_verano, by = c("date","ID_LCS"), all.y = TRUE)
merge_Qui_verano_2 <- merge_Qui_verano_2[complete.cases(merge_Qui_verano_2$Registros.completos),]
# Ajuste del modelo de regresión lineal
#modelo_Quilicura_verano <- lm(mean ~ valor_raster, data = data_Quilicura_verano)
# R2_Quilicura_verano <- summary(modelo_Quilicura_verano)$r.squared
# RMSE_Quilicura_verano <- sqrt(mean(residuals(modelo_Quilicura_verano)^2))
# Bias_Quilicura_verano <- mean(data_Quilicura_verano$mean - data_Quilicura_verano$valor_raster)
# n_Quilicura_verano <- nrow(data_Quilicura_verano)


# Sinca mas cercano vs LCS
# modelo_Qui_verano <- lm(mean_LCS ~ Registros.completos, data = merge_Qui_verano)
# R2_Quilicura_verano <- summary(modelo_Qui_verano)$r.squared
# RMSE_Quilicura_verano <- sqrt(mean(residuals(modelo_Qui_verano)^2))
# Bias_Quilicura_verano <- mean(merge_Qui_verano$mean_LCS - merge_Qui_verano$Registros.completos)
# n_Quilicura_verano <- nrow(merge_Qui_verano)

# Sinca mas cercano vs modelo en sensores LCS
modelo_raster_Qui_verano <- lm(valor_raster_LCS ~ Registros.completos, data = merge_Qui_verano_2)
R2_Quilicura_verano_modelo <- round(summary(modelo_raster_Qui_verano)$r.squared,2)
RMSE_Quilicura_verano_modelo <- round(sqrt(mean(residuals(modelo_raster_Qui_verano)^2)),2)
Bias_Quilicura_verano_modelo <- round(mean(merge_Qui_verano_2$valor_raster_LCS - merge_Qui_verano_2$Registros.completos),2)
n_Quilicura_verano_modelo <- nrow(merge_Qui_verano_2)

#monitoreo
modelo_monitoreo_Qui_verano <- lm(mean_LCS_model ~ Registros.completos, data = merge_Qui_verano_2)
R2_Quilicura_verano_monitoreo<- round(summary(modelo_monitoreo_Qui_verano)$r.squared,2)
RMSE_Quilicura_verano_monitoreo <- round(sqrt(mean(residuals(modelo_monitoreo_Qui_verano)^2)),2)
Bias_Quilicura_verano_monitoreo <- round(mean(merge_Qui_verano_2$mean_LCS_model - merge_Qui_verano_2$Registros.completos),2)
n_Quilicura_verano_monitoreo <- nrow(merge_Qui_verano_2)


# Crear el gráfico con ggplot2
#plot_Quilicura_verano <- ggplot(data_Quilicura_verano , aes(x = valor_raster, y = mean)) +
# Crear el gráfico con ggplot2
#plot_Qui_verano <- ggplot(merge_Qui_verano , aes(x = Registros.completos, y = mean_LCS)) +
plot_Qui_verano <- ggplot() +
  
  geom_point(merge_Qui_verano_2, mapping = aes(x = Registros.completos, y = mean_LCS, color = "Monitoreo"), size = 1.5, alpha = 0.6) +  # Puntos de datos (mean_LCS)
  geom_point(merge_Qui_verano_2, mapping = aes(x = Registros.completos, y = valor_raster_LCS, color = "Modelo"), size = 1.5, alpha = 0.6) +  # Puntos de datos (valor_raster_LCS)
  
  geom_smooth(merge_Qui_verano_2, mapping = aes(x = Registros.completos, y = valor_raster_LCS, color = "Modelo"), method = "lm", se = FALSE) +  # Línea de regresión (valor_raster_LCS)
  geom_smooth(merge_Qui_verano_2, mapping = aes(x = Registros.completos, y = mean_LCS, color = "Monitoreo"), method = "lm", se = FALSE) +  # Línea de regresión (mean_LCS)
  
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 10)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 10)) +  # Ticks cada 10 en el eje X
  
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.3) +  # Línea 1:1 negra
  
  labs(color = "") +  # Título de la leyenda
  
  # Usar geom_text para agregar el texto en las coordenadas deseadas
  geom_text(aes(x = 35, y = 15, label = "Monitoreo"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 46, y = 15, label = "Modelo"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  
  ##Metricas
  geom_text(aes(x = 25, y = 12, label = "R2"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 25, y = 9, label = "RMSE"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 25, y = 6, label = "Bias"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 25, y = 3, label = "n"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  
  
  ##Raster
  geom_text(aes(x = 46, y = 12, label = R2_Quilicura_verano_modelo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 46, y = 9, label = RMSE_Quilicura_verano_modelo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 46, y = 6, label = Bias_Quilicura_verano_modelo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 46, y = 3, label = n_Quilicura_verano_modelo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  
  
  ##Monitoreo
  geom_text(aes(x = 36, y = 12, label = R2_Quilicura_verano_monitoreo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 36, y = 9, label = RMSE_Quilicura_verano_monitoreo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 36, y = 6, label = Bias_Quilicura_verano_monitoreo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 36, y = 3, label = n_Quilicura_verano_monitoreo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  labs(
    title = "Quilicura Verano",  # Título del gráfico
    #subtitle = "Análisis de desempeño del modelo",     # Subtítulo
    x = "SINCA",                         # Texto para el eje X
    y = "LCS Monitoreo - Modelado"                                 # Texto para el eje Y
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 10, face = "bold"),  # Tamaño del título
    plot.subtitle = element_text(size = 8),               # Tamaño del subtítulo
    axis.title = element_text(size = 8),                   # Tamaño de los títulos de los ejes
    axis.text = element_text(size = 6),                    # Tamaño de los textos de los ejes
    axis.ticks.length = unit(0.1, "cm"),                   # Tamaño de los ticks
    axis.line = element_line(size = 0.2)                   # Grosor de las líneas de los ejes
  )
plot_Qui_verano <- plot_Qui_verano +
  theme(
    legend.position = c(0.15, 0.9),  # Ubicación dentro del gráfico
    legend.text = element_text(size = 8),  # Tamaño del texto de la leyenda
    legend.title = element_text(size = 8)  # Tamaño del título de la leyenda (si lo tienes)
  )

plot_Qui_verano



#################################
##### Quilicura_invierno
data_LCS_Qui_invierno <- data_LCS[data_LCS$archivo == "Quilicura_invierno",]
data_SINCA_Qui_invierno <- data_SINCA[data_SINCA$estacion_SINCA == "QUI",]
data_LCS_Modelo_Qui_invierno <- data_LCS_modelo[data_LCS_modelo$archivo == "Quilicura_invierno",]

#meduciones LCS
data_LCS_Qui_invierno$date <- as.POSIXct(as.character(data_LCS_Qui_invierno$date), format = "%d/%m/%Y" )#"%y%m%d")
data_SINCA_Qui_invierno$date <- as.POSIXct(as.character(data_SINCA_Qui_invierno$date), format = "%d/%m/%Y" )#"%y%m%d")

data_LCS_Modelo_Qui_invierno$date <- as.POSIXct(as.character(data_LCS_Modelo_Qui_invierno$date), format = "%Y-%m-%d" )#"%y%m%d")


data_SINCA_Qui_invierno <- data_SINCA_Qui_invierno[year(data_SINCA_Qui_invierno$date) == 2021,]
#UniSINCA - Meidiones LCS
merge_Qui_invierno <- merge(data_SINCA_Qui_invierno, data_LCS_Qui_invierno, by = "date", all.y = TRUE)
#Unir SINCA y Medicioones LCS - Modelo en sitios lCS
names(merge_Qui_invierno)
names(data_LCS_Modelo_Qui_invierno)

unique(data_LCS_Modelo_Qui_invierno$ID_archivo)
data_LCS_Modelo_Qui_invierno$ID_LCS <- data_LCS_Modelo_Qui_invierno$ID_archivo
length(unique(merge_Qui_invierno$ID_LCS))


merge_Qui_invierno_2 <-merge(merge_Qui_invierno, data_LCS_Modelo_Qui_invierno, by = c("date","ID_LCS"), all.y = TRUE)
merge_Qui_invierno_2 <- merge_Qui_invierno_2[complete.cases(merge_Qui_invierno_2$Registros.completos),]
# Sinca mas cercano vs modelo en sensores LCS
modelo_raster_Qui_invierno <- lm(valor_raster_LCS ~ Registros.completos, data = merge_Qui_invierno_2)
R2_Quilicura_invierno_modelo <- round(summary(modelo_raster_Qui_invierno)$r.squared,2)
RMSE_Quilicura_invierno_modelo <- round(sqrt(mean(residuals(modelo_raster_Qui_invierno)^2)),2)
Bias_Quilicura_invierno_modelo <- round(mean(merge_Qui_invierno_2$valor_raster_LCS - merge_Qui_invierno_2$Registros.completos),2)
n_Quilicura_invierno_modelo <- nrow(merge_Qui_invierno_2)

#monitoreo
modelo_monitoreo_Qui_invierno <- lm(mean_LCS_model ~ Registros.completos, data = merge_Qui_invierno_2)
R2_Quilicura_invierno_monitoreo<- round(summary(modelo_monitoreo_Qui_invierno)$r.squared,2)
RMSE_Quilicura_invierno_monitoreo <- round(sqrt(mean(residuals(modelo_monitoreo_Qui_invierno)^2)),2)
Bias_Quilicura_invierno_monitoreo <- round(mean(merge_Qui_invierno_2$mean_LCS_model - merge_Qui_invierno_2$Registros.completos),2)
n_Quilicura_invierno_monitoreo <- nrow(merge_Qui_invierno_2)


# Crear el gráfico con ggplot2
#plot_Quilicura_invierno <- ggplot(data_Quilicura_invierno , aes(x = valor_raster, y = mean)) +
# Crear el gráfico con ggplot2
#plot_Qui_invierno <- ggplot(merge_Qui_invierno , aes(x = Registros.completos, y = mean_LCS)) +
plot_Qui_invierno <- ggplot() +
  
  geom_point(merge_Qui_invierno_2, mapping = aes(x = Registros.completos, y = mean_LCS, color = "Monitoreo"), size = 1.5, alpha = 0.6) +  # Puntos de datos (mean_LCS)
  geom_point(merge_Qui_invierno_2, mapping = aes(x = Registros.completos, y = valor_raster_LCS, color = "Modelo"), size = 1.5, alpha = 0.6) +  # Puntos de datos (valor_raster_LCS)
  
  geom_smooth(merge_Qui_invierno_2, mapping = aes(x = Registros.completos, y = valor_raster_LCS, color = "Modelo"), method = "lm", se = FALSE) +  # Línea de regresión (valor_raster_LCS)
  geom_smooth(merge_Qui_invierno_2, mapping = aes(x = Registros.completos, y = mean_LCS, color = "Monitoreo"), method = "lm", se = FALSE) +  # Línea de regresión (mean_LCS)
  
  scale_y_continuous(limits = c(0,180), breaks = seq(0, 180, by = 60)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 180), breaks = seq(0, 180, by = 60)) +  # Ticks cada 10 en el eje X
  
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.3) +  # Línea 1:1 negra
  
  labs(color = "") +  # Título de la leyenda
  
  # Usar geom_text para agregar el texto en las coordenadas deseadas
  geom_text(aes(x = 120, y = 45, label = "Monitoreo"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 160, y = 45, label = "Modelo"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  
  ##Metricas
  geom_text(aes(x = 85, y = 35, label = "R2"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 85, y = 25, label = "RMSE"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 85, y = 15, label = "Bias"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 85, y = 5, label = "n"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  
  
  ##Raster
  geom_text(aes(x = 160, y = 35, label = R2_Quilicura_invierno_modelo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 160, y = 25, label = RMSE_Quilicura_invierno_modelo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 160, y = 15, label = Bias_Quilicura_invierno_modelo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 160, y = 5, label = n_Quilicura_invierno_modelo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  
  
  ##Monitoreo
  geom_text(aes(x = 120, y = 35, label = R2_Quilicura_invierno_monitoreo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 120, y = 25, label = RMSE_Quilicura_invierno_monitoreo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 120, y = 15, label = Bias_Quilicura_invierno_monitoreo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 120, y = 5, label = n_Quilicura_invierno_monitoreo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  labs(
    title = "Quilicura Invierno",  # Título del gráfico
    #subtitle = "Análisis de desempeño del modelo",     # Subtítulo
    x = "SINCA",                         # Texto para el eje X
    y = "LCS Monitoreo - Modelado"                                 # Texto para el eje Y
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 10, face = "bold"),  # Tamaño del título
    plot.subtitle = element_text(size = 8),               # Tamaño del subtítulo
    axis.title = element_text(size = 8),                   # Tamaño de los títulos de los ejes
    axis.text = element_text(size = 6),                    # Tamaño de los textos de los ejes
    axis.ticks.length = unit(0.1, "cm"),                   # Tamaño de los ticks
    axis.line = element_line(size = 0.2)                   # Grosor de las líneas de los ejes
  )

plot_Qui_invierno <- plot_Qui_invierno +
  theme(
    legend.position = c(0.15, 0.9),  # Ubicación dentro del gráfico
    legend.text = element_text(size = 8),  # Tamaño del texto de la leyenda
    legend.title = element_text(size = 8)  # Tamaño del título de la leyenda (si lo tienes)
  )


plot_Qui_invierno


#################################
#################################
#################################
##### vitacura_verano
data_LCS_vitacura_verano <- data_LCS[data_LCS$archivo == "vitacura verano",]
data_SINCA_vitacura_verano <- data_SINCA[data_SINCA$estacion_SINCA == "CDE",]
data_LCS_Modelo_vitacura_verano <- data_LCS_modelo[data_LCS_modelo$archivo == "vitacura verano",]

#meduciones LCS
data_LCS_vitacura_verano$date <- as.POSIXct(as.character(data_LCS_vitacura_verano$date), format = "%d/%m/%Y" )#"%y%m%d")
data_SINCA_vitacura_verano$date <- as.POSIXct(as.character(data_SINCA_vitacura_verano$date), format = "%d/%m/%Y" )#"%y%m%d")

data_LCS_Modelo_vitacura_verano$date <- as.POSIXct(as.character(data_LCS_Modelo_vitacura_verano$date), format = "%Y-%m-%d" )#"%y%m%d")


data_SINCA_vitacura_verano <- data_SINCA_vitacura_verano[year(data_SINCA_vitacura_verano$date) == 2021,]
#UniSINCA - Meidiones LCS
merge_vitacura_verano <- merge(data_SINCA_vitacura_verano, data_LCS_vitacura_verano, by = "date", all.y = TRUE)
#Unir SINCA y Medicioones LCS - Modelo en sitios lCS
names(merge_vitacura_verano)
names(data_LCS_Modelo_vitacura_verano)

unique(data_LCS_Modelo_vitacura_verano$ID_archivo)
data_LCS_Modelo_vitacura_verano$ID_LCS <- data_LCS_Modelo_vitacura_verano$ID_archivo
length(unique(merge_vitacura_verano$ID_LCS))


merge_vitacura_verano_2 <-merge(merge_vitacura_verano, data_LCS_Modelo_vitacura_verano, by = c("date","ID_LCS"), all.y = TRUE)
merge_vitacura_verano_2 <- merge_vitacura_verano_2[complete.cases(merge_vitacura_verano_2$Registros.completos),]
# Sinca mas cercano vs modelo en sensores LCS
modelo_raster_vitacura_verano <- lm(valor_raster_LCS ~ Registros.completos, data = merge_vitacura_verano_2)
R2_vitacura_verano_modelo <- round(summary(modelo_raster_vitacura_verano)$r.squared,2)
RMSE_vitacura_verano_modelo <- round(sqrt(mean(residuals(modelo_raster_vitacura_verano)^2)),2)
Bias_vitacura_verano_modelo <- round(mean(merge_vitacura_verano_2$valor_raster_LCS - merge_vitacura_verano_2$Registros.completos),2)
n_vitacura_verano_modelo <- nrow(merge_vitacura_verano_2)

#monitoreo
modelo_monitoreo_vitacura_verano <- lm(mean_LCS_model ~ Registros.completos, data = merge_vitacura_verano_2)
R2_vitacura_verano_monitoreo<- round(summary(modelo_monitoreo_vitacura_verano)$r.squared,2)
RMSE_vitacura_verano_monitoreo <- round(sqrt(mean(residuals(modelo_monitoreo_vitacura_verano)^2)),2)
Bias_vitacura_verano_monitoreo <- round(mean(merge_vitacura_verano_2$mean_LCS_model - merge_vitacura_verano_2$Registros.completos),2)
n_vitacura_verano_monitoreo <- nrow(merge_vitacura_verano_2)


# Crear el gráfico con ggplot2
#plot_vitacura_verano <- ggplot(data_vitacura_verano , aes(x = valor_raster, y = mean)) +
# Crear el gráfico con ggplot2
#plot_vitacura_verano <- ggplot(merge_vitacura_verano , aes(x = Registros.completos, y = mean_LCS)) +
plot_vitacura_verano <- ggplot() +
  
  geom_point(merge_vitacura_verano_2, mapping = aes(x = Registros.completos, y = mean_LCS, color = "Monitoreo"), size = 1.5, alpha = 0.6) +  # Puntos de datos (mean_LCS)
  geom_point(merge_vitacura_verano_2, mapping = aes(x = Registros.completos, y = valor_raster_LCS, color = "Modelo"), size = 1.5, alpha = 0.6) +  # Puntos de datos (valor_raster_LCS)
  
  geom_smooth(merge_vitacura_verano_2, mapping = aes(x = Registros.completos, y = valor_raster_LCS, color = "Modelo"), method = "lm", se = FALSE) +  # Línea de regresión (valor_raster_LCS)
  geom_smooth(merge_vitacura_verano_2, mapping = aes(x = Registros.completos, y = mean_LCS, color = "Monitoreo"), method = "lm", se = FALSE) +  # Línea de regresión (mean_LCS)
  
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 10)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 10)) +  # Ticks cada 10 en el eje X
  
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.3) +  # Línea 1:1 negra
  
  labs(color = "") +  # Título de la leyenda
  
  # Usar geom_text para agregar el texto en las coordenadas deseadas
  geom_text(aes(x = 35, y = 15, label = "Monitoreo"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 46, y = 15, label = "Modelo"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  
  ##Metricas
  geom_text(aes(x = 25, y = 12, label = "R2"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 25, y = 9, label = "RMSE"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 25, y = 6, label = "Bias"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 25, y = 3, label = "n"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  
  
  ##Raster
  geom_text(aes(x = 46, y = 12, label = R2_vitacura_verano_modelo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 46, y = 9, label = RMSE_vitacura_verano_modelo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 46, y = 6, label = Bias_vitacura_verano_modelo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 46, y = 3, label = n_vitacura_verano_modelo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  
  
  ##Monitoreo
  geom_text(aes(x = 36, y = 12, label = R2_vitacura_verano_monitoreo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 36, y = 9, label = RMSE_vitacura_verano_monitoreo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 36, y = 6, label = Bias_vitacura_verano_monitoreo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 36, y = 3, label = n_vitacura_verano_monitoreo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  labs(
    title = "vitacura Verano",  # Título del gráfico
    #subtitle = "Análisis de desempeño del modelo",     # Subtítulo
    x = "SINCA",                         # Texto para el eje X
    y = "LCS Monitoreo - Modelado"                                 # Texto para el eje Y
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 10, face = "bold"),  # Tamaño del título
    plot.subtitle = element_text(size = 8),               # Tamaño del subtítulo
    axis.title = element_text(size = 8),                   # Tamaño de los títulos de los ejes
    axis.text = element_text(size = 6),                    # Tamaño de los textos de los ejes
    axis.ticks.length = unit(0.1, "cm"),                   # Tamaño de los ticks
    axis.line = element_line(size = 0.2)                   # Grosor de las líneas de los ejes
  )

plot_vitacura_verano <- plot_vitacura_verano +
  theme(
    legend.position = c(0.15, 0.9),  # Ubicación dentro del gráfico
    legend.text = element_text(size = 8),  # Tamaño del texto de la leyenda
    legend.title = element_text(size = 8)  # Tamaño del título de la leyenda (si lo tienes)
  )


plot_vitacura_verano

##############################
#############################
#################################
##### vitacura_invierno
data_LCS_vitacura_invierno <- data_LCS[data_LCS$archivo == "Vitacura Invierno",]
data_SINCA_vitacura_invierno <- data_SINCA[data_SINCA$estacion_SINCA == "CDE",]
data_LCS_Modelo_vitacura_invierno <- data_LCS_modelo[data_LCS_modelo$archivo == "Vitacura Invierno",]

#meduciones LCS
data_LCS_vitacura_invierno$date <- as.POSIXct(as.character(data_LCS_vitacura_invierno$date), format = "%d/%m/%Y" )#"%y%m%d")
data_SINCA_vitacura_invierno$date <- as.POSIXct(as.character(data_SINCA_vitacura_invierno$date), format = "%d/%m/%Y" )#"%y%m%d")

data_LCS_Modelo_vitacura_invierno$date <- as.POSIXct(as.character(data_LCS_Modelo_vitacura_invierno$date), format = "%Y-%m-%d" )#"%y%m%d")


data_SINCA_vitacura_invierno <- data_SINCA_vitacura_invierno[year(data_SINCA_vitacura_invierno$date) == 2021,]

#UniSINCA - Meidiones LCS
merge_vitacura_invierno <- merge(data_SINCA_vitacura_invierno, data_LCS_vitacura_invierno, by = "date", all.y = TRUE)
#Unir SINCA y Medicioones LCS - Modelo en sitios lCS
names(merge_vitacura_invierno)
names(data_LCS_Modelo_vitacura_invierno)

unique(data_LCS_Modelo_vitacura_invierno$ID_archivo)
data_LCS_Modelo_vitacura_invierno$ID_LCS <- data_LCS_Modelo_vitacura_invierno$ID_archivo
length(unique(merge_vitacura_invierno$ID_LCS))


merge_vitacura_invierno_2 <-merge(merge_vitacura_invierno, data_LCS_Modelo_vitacura_invierno, by = c("date","ID_LCS"), all.y = TRUE)
merge_vitacura_invierno_2 <- merge_vitacura_invierno_2[complete.cases(merge_vitacura_invierno_2$Registros.completos),]
# Sinca mas cercano vs modelo en sensores LCS
modelo_raster_vitacura_invierno <- lm(valor_raster_LCS ~ Registros.completos, data = merge_vitacura_invierno_2)
R2_vitacura_invierno_modelo <- round(summary(modelo_raster_vitacura_invierno)$r.squared,2)
RMSE_vitacura_invierno_modelo <- round(sqrt(mean(residuals(modelo_raster_vitacura_invierno)^2)),2)
Bias_vitacura_invierno_modelo <- round(mean(merge_vitacura_invierno_2$valor_raster_LCS - merge_vitacura_invierno_2$Registros.completos),2)
n_vitacura_invierno_modelo <- nrow(merge_vitacura_invierno_2)

#monitoreo
modelo_monitoreo_vitacura_invierno <- lm(mean_LCS_model ~ Registros.completos, data = merge_vitacura_invierno_2)
R2_vitacura_invierno_monitoreo<- round(summary(modelo_monitoreo_vitacura_invierno)$r.squared,2)
RMSE_vitacura_invierno_monitoreo <- round(sqrt(mean(residuals(modelo_monitoreo_vitacura_invierno)^2)),2)
Bias_vitacura_invierno_monitoreo <- round(mean(merge_vitacura_invierno_2$mean_LCS_model - merge_vitacura_invierno_2$Registros.completos),2)
n_vitacura_invierno_monitoreo <- nrow(merge_vitacura_invierno_2)


# Crear el gráfico con ggplot2
#plot_vitacura_invierno <- ggplot(data_vitacura_invierno , aes(x = valor_raster, y = mean)) +
# Crear el gráfico con ggplot2
#plot_vitacura_invierno <- ggplot(merge_vitacura_invierno , aes(x = Registros.completos, y = mean_LCS)) +
plot_vitacura_invierno <- ggplot() +
  
  geom_point(merge_vitacura_invierno_2, mapping = aes(x = Registros.completos, y = mean_LCS, color = "Monitoreo"), size = 1.5, alpha = 0.6) +  # Puntos de datos (mean_LCS)
  geom_point(merge_vitacura_invierno_2, mapping = aes(x = Registros.completos, y = valor_raster_LCS, color = "Modelo"), size = 1.5, alpha = 0.6) +  # Puntos de datos (valor_raster_LCS)
  
  geom_smooth(merge_vitacura_invierno_2, mapping = aes(x = Registros.completos, y = valor_raster_LCS, color = "Modelo"), method = "lm", se = FALSE) +  # Línea de regresión (valor_raster_LCS)
  geom_smooth(merge_vitacura_invierno_2, mapping = aes(x = Registros.completos, y = mean_LCS, color = "Monitoreo"), method = "lm", se = FALSE) +  # Línea de regresión (mean_LCS)
  
  scale_y_continuous(limits = c(0,180), breaks = seq(0, 180, by = 60)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 180), breaks = seq(0, 180, by = 60)) +  # Ticks cada 10 en el eje X
  
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.3) +  # Línea 1:1 negra
  
  labs(color = "") +  # Título de la leyenda
  
  # Usar geom_text para agregar el texto en las coordenadas deseadas
  geom_text(aes(x = 120, y = 45, label = "Monitoreo"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 160, y = 45, label = "Modelo"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  
  ##Metricas
  geom_text(aes(x = 85, y = 35, label = "R2"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 85, y = 25, label = "RMSE"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 85, y = 15, label = "Bias"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 85, y = 5, label = "n"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  
  
  ##Raster
  geom_text(aes(x = 160, y = 35, label = R2_vitacura_invierno_modelo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 160, y = 25, label = RMSE_vitacura_invierno_modelo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 160, y = 15, label = Bias_vitacura_invierno_modelo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 160, y = 5, label = n_vitacura_invierno_modelo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  
  
  ##Monitoreo
  geom_text(aes(x = 120, y = 35, label = R2_vitacura_invierno_monitoreo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 120, y = 25, label = RMSE_vitacura_invierno_monitoreo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 120, y = 15, label = Bias_vitacura_invierno_monitoreo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 120, y = 5, label = n_vitacura_invierno_monitoreo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  labs(
    title = "vitacura Invierno",  # Título del gráfico
    #subtitle = "Análisis de desempeño del modelo",     # Subtítulo
    x = "SINCA",                         # Texto para el eje X
    y = "LCS Monitoreo - Modelado"                                 # Texto para el eje Y
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 10, face = "bold"),  # Tamaño del título
    plot.subtitle = element_text(size = 8),               # Tamaño del subtítulo
    axis.title = element_text(size = 8),                   # Tamaño de los títulos de los ejes
    axis.text = element_text(size = 6),                    # Tamaño de los textos de los ejes
    axis.ticks.length = unit(0.1, "cm"),                   # Tamaño de los ticks
    axis.line = element_line(size = 0.2)                   # Grosor de las líneas de los ejes
  )

plot_vitacura_invierno <- plot_vitacura_invierno +
  theme(
    legend.position = c(0.15, 0.9),  # Ubicación dentro del gráfico
    legend.text = element_text(size = 8),  # Tamaño del texto de la leyenda
    legend.title = element_text(size = 8)  # Tamaño del título de la leyenda (si lo tienes)
  )


plot_vitacura_invierno


#################################
#################################
#################################

##############################
#############################
#################################
##### maipu_invierno
data_LCS_maipu_invierno <- data_LCS[data_LCS$archivo == "Maipu Invierno",]
data_SINCA_maipu_invierno <- data_SINCA[data_SINCA$estacion_SINCA == "PDH",]
data_LCS_Modelo_maipu_invierno <- data_LCS_modelo[data_LCS_modelo$archivo == "Maipu Invierno",]

#meduciones LCS
data_LCS_maipu_invierno$date <- as.POSIXct(as.character(data_LCS_maipu_invierno$date), format = "%d/%m/%Y" )#"%y%m%d")
data_SINCA_maipu_invierno$date <- as.POSIXct(as.character(data_SINCA_maipu_invierno$date), format = "%d/%m/%Y" )#"%y%m%d")

data_LCS_Modelo_maipu_invierno$date <- as.POSIXct(as.character(data_LCS_Modelo_maipu_invierno$date), format = "%Y-%m-%d" )#"%y%m%d")


data_SINCA_maipu_invierno <- data_SINCA_maipu_invierno[year(data_SINCA_maipu_invierno$date) == 2021,]

#UniSINCA - Meidiones LCS
merge_maipu_invierno <- merge(data_SINCA_maipu_invierno, data_LCS_maipu_invierno, by = "date", all.y = TRUE)
#Unir SINCA y Medicioones LCS - Modelo en sitios lCS
names(merge_maipu_invierno)
names(data_LCS_Modelo_maipu_invierno)

unique(data_LCS_Modelo_maipu_invierno$ID_archivo)
data_LCS_Modelo_maipu_invierno$ID_LCS <- data_LCS_Modelo_maipu_invierno$ID_archivo
length(unique(merge_maipu_invierno$ID_LCS))


merge_maipu_invierno_2 <-merge(merge_maipu_invierno, data_LCS_Modelo_maipu_invierno, by = c("date","ID_LCS"), all.y = TRUE)
merge_maipu_invierno_2 <- merge_maipu_invierno_2[complete.cases(merge_maipu_invierno_2$Registros.completos),]
# Sinca mas cercano vs modelo en sensores LCS
modelo_raster_maipu_invierno <- lm(valor_raster_LCS ~ Registros.completos, data = merge_maipu_invierno_2)
R2_maipu_invierno_modelo <- round(summary(modelo_raster_maipu_invierno)$r.squared,2)
RMSE_maipu_invierno_modelo <- round(sqrt(mean(residuals(modelo_raster_maipu_invierno)^2)),2)
Bias_maipu_invierno_modelo <- round(mean(merge_maipu_invierno_2$valor_raster_LCS - merge_maipu_invierno_2$Registros.completos),2)
n_maipu_invierno_modelo <- nrow(merge_maipu_invierno_2)

#monitoreo
modelo_monitoreo_maipu_invierno <- lm(mean_LCS_model ~ Registros.completos, data = merge_maipu_invierno_2)
R2_maipu_invierno_monitoreo<- round(summary(modelo_monitoreo_maipu_invierno)$r.squared,2)
RMSE_maipu_invierno_monitoreo <- round(sqrt(mean(residuals(modelo_monitoreo_maipu_invierno)^2)),2)
Bias_maipu_invierno_monitoreo <- round(mean(merge_maipu_invierno_2$mean_LCS_model - merge_maipu_invierno_2$Registros.completos),2)
n_maipu_invierno_monitoreo <- nrow(merge_maipu_invierno_2)


# Crear el gráfico con ggplot2
#plot_maipu_invierno <- ggplot(data_maipu_invierno , aes(x = valor_raster, y = mean)) +
# Crear el gráfico con ggplot2
#plot_maipu_invierno <- ggplot(merge_maipu_invierno , aes(x = Registros.completos, y = mean_LCS)) +
plot_maipu_invierno <- ggplot() +
  
  geom_point(merge_maipu_invierno_2, mapping = aes(x = Registros.completos, y = mean_LCS, color = "Monitoreo"), size = 1.5, alpha = 0.6) +  # Puntos de datos (mean_LCS)
  geom_point(merge_maipu_invierno_2, mapping = aes(x = Registros.completos, y = valor_raster_LCS, color = "Modelo"), size = 1.5, alpha = 0.6) +  # Puntos de datos (valor_raster_LCS)
  
  geom_smooth(merge_maipu_invierno_2, mapping = aes(x = Registros.completos, y = valor_raster_LCS, color = "Modelo"), method = "lm", se = FALSE) +  # Línea de regresión (valor_raster_LCS)
  geom_smooth(merge_maipu_invierno_2, mapping = aes(x = Registros.completos, y = mean_LCS, color = "Monitoreo"), method = "lm", se = FALSE) +  # Línea de regresión (mean_LCS)
  
  scale_y_continuous(limits = c(0,180), breaks = seq(0, 180, by = 60)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 180), breaks = seq(0, 180, by = 60)) +  # Ticks cada 10 en el eje X
  
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.3) +  # Línea 1:1 negra
  
  labs(color = "") +  # Título de la leyenda
  
  # Usar geom_text para agregar el texto en las coordenadas deseadas
  geom_text(aes(x = 120, y = 45, label = "Monitoreo"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 160, y = 45, label = "Modelo"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  
  ##Metricas
  geom_text(aes(x = 85, y = 35, label = "R2"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 85, y = 25, label = "RMSE"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 85, y = 15, label = "Bias"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 85, y = 5, label = "n"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  
  
  ##Raster
  geom_text(aes(x = 160, y = 35, label = R2_maipu_invierno_modelo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 160, y = 25, label = RMSE_maipu_invierno_modelo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 160, y = 15, label = Bias_maipu_invierno_modelo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 160, y = 5, label = n_maipu_invierno_modelo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  
  
  ##Monitoreo
  geom_text(aes(x = 120, y = 35, label = R2_maipu_invierno_monitoreo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 120, y = 25, label = RMSE_maipu_invierno_monitoreo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 120, y = 15, label = Bias_maipu_invierno_monitoreo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 120, y = 5, label = n_maipu_invierno_monitoreo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  labs(
    title = "Maipu Invierno",  # Título del gráfico
    #subtitle = "Análisis de desempeño del modelo",     # Subtítulo
    x = "SINCA",                         # Texto para el eje X
    y = "LCS Monitoreo - Modelado"                                 # Texto para el eje Y
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 10, face = "bold"),  # Tamaño del título
    plot.subtitle = element_text(size = 8),               # Tamaño del subtítulo
    axis.title = element_text(size = 8),                   # Tamaño de los títulos de los ejes
    axis.text = element_text(size = 6),                    # Tamaño de los textos de los ejes
    axis.ticks.length = unit(0.1, "cm"),                   # Tamaño de los ticks
    axis.line = element_line(size = 0.2)                   # Grosor de las líneas de los ejes
  )

plot_maipu_invierno <- plot_maipu_invierno +
  theme(
    legend.position = c(0.15, 0.9),  # Ubicación dentro del gráfico
    legend.text = element_text(size = 8),  # Tamaño del texto de la leyenda
    legend.title = element_text(size = 8)  # Tamaño del título de la leyenda (si lo tienes)
  )


plot_maipu_invierno


#################################
#################################

#############################
#################################
##### maipu_verano
data_LCS_maipu_verano <- data_LCS[data_LCS$archivo == "Maipu Verano",]
data_SINCA_maipu_verano <- data_SINCA[data_SINCA$estacion_SINCA == "PDH",]
data_LCS_Modelo_maipu_verano <- data_LCS_modelo[data_LCS_modelo$archivo == "Maipu Verano",]

#meduciones LCS
data_LCS_maipu_verano$date <- as.POSIXct(as.character(data_LCS_maipu_verano$date), format = "%d/%m/%Y" )#"%y%m%d")
data_SINCA_maipu_verano$date <- as.POSIXct(as.character(data_SINCA_maipu_verano$date), format = "%d/%m/%Y" )#"%y%m%d")

data_LCS_Modelo_maipu_verano$date <- as.POSIXct(as.character(data_LCS_Modelo_maipu_verano$date), format = "%Y-%m-%d" )#"%y%m%d")


data_SINCA_maipu_verano <- data_SINCA_maipu_verano[year(data_SINCA_maipu_verano$date) == 2021,]

#UniSINCA - Meidiones LCS
merge_maipu_verano <- merge(data_SINCA_maipu_verano, data_LCS_maipu_verano, by = "date", all.y = TRUE)
#Unir SINCA y Medicioones LCS - Modelo en sitios lCS
names(merge_maipu_verano)
names(data_LCS_Modelo_maipu_verano)

unique(data_LCS_Modelo_maipu_verano$ID_archivo)
data_LCS_Modelo_maipu_verano$ID_LCS <- data_LCS_Modelo_maipu_verano$ID_archivo
length(unique(merge_maipu_verano$ID_LCS))


merge_maipu_verano_2 <-merge(merge_maipu_verano, data_LCS_Modelo_maipu_verano, by = c("date","ID_LCS"), all.y = TRUE)
merge_maipu_verano_2 <- merge_maipu_verano_2[complete.cases(merge_maipu_verano_2$Registros.completos),]
# Sinca mas cercano vs modelo en sensores LCS
modelo_raster_maipu_verano <- lm(valor_raster_LCS ~ Registros.completos, data = merge_maipu_verano_2)
R2_maipu_verano_modelo <- round(summary(modelo_raster_maipu_verano)$r.squared,2)
RMSE_maipu_verano_modelo <- round(sqrt(mean(residuals(modelo_raster_maipu_verano)^2)),2)
Bias_maipu_verano_modelo <- round(mean(merge_maipu_verano_2$valor_raster_LCS - merge_maipu_verano_2$Registros.completos),2)
n_maipu_verano_modelo <- nrow(merge_maipu_verano_2)

#monitoreo
modelo_monitoreo_maipu_verano <- lm(mean_LCS_model ~ Registros.completos, data = merge_maipu_verano_2)
R2_maipu_verano_monitoreo<- round(summary(modelo_monitoreo_maipu_verano)$r.squared,2)
RMSE_maipu_verano_monitoreo <- round(sqrt(mean(residuals(modelo_monitoreo_maipu_verano)^2)),2)
Bias_maipu_verano_monitoreo <- round(mean(merge_maipu_verano_2$mean_LCS_model - merge_maipu_verano_2$Registros.completos),2)
n_maipu_verano_monitoreo <- nrow(merge_maipu_verano_2)


# Crear el gráfico con ggplot2
#plot_maipu_verano <- ggplot(data_maipu_verano , aes(x = valor_raster, y = mean)) +
# Crear el gráfico con ggplot2
#plot_maipu_verano <- ggplot(merge_maipu_verano , aes(x = Registros.completos, y = mean_LCS)) +
plot_maipu_verano <- ggplot() +
  
  geom_point(merge_maipu_verano_2, mapping = aes(x = Registros.completos, y = mean_LCS, color = "Monitoreo"), size = 1.5, alpha = 0.6) +  # Puntos de datos (mean_LCS)
  geom_point(merge_maipu_verano_2, mapping = aes(x = Registros.completos, y = valor_raster_LCS, color = "Modelo"), size = 1.5, alpha = 0.6) +  # Puntos de datos (valor_raster_LCS)
  
  geom_smooth(merge_maipu_verano_2, mapping = aes(x = Registros.completos, y = valor_raster_LCS, color = "Modelo"), method = "lm", se = FALSE) +  # Línea de regresión (valor_raster_LCS)
  geom_smooth(merge_maipu_verano_2, mapping = aes(x = Registros.completos, y = mean_LCS, color = "Monitoreo"), method = "lm", se = FALSE) +  # Línea de regresión (mean_LCS)
  
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 10)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 10)) +  # Ticks cada 10 en el eje X
  
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.3) +  # Línea 1:1 negra
  
  labs(color = "") +  # Título de la leyenda
  
  # Usar geom_text para agregar el texto en las coordenadas deseadas
  geom_text(aes(x = 35, y = 15, label = "Monitoreo"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 46, y = 15, label = "Modelo"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  
  ##Metricas
  geom_text(aes(x = 25, y = 12, label = "R2"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 25, y = 9, label = "RMSE"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 25, y = 6, label = "Bias"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 25, y = 3, label = "n"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  
  
  ##Raster
  geom_text(aes(x = 46, y = 12, label = R2_maipu_verano_modelo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 46, y = 9, label = RMSE_maipu_verano_modelo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 46, y = 6, label = Bias_maipu_verano_modelo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 46, y = 3, label = n_maipu_verano_modelo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  
  
  ##Monitoreo
  geom_text(aes(x = 36, y = 12, label = R2_maipu_verano_monitoreo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 36, y = 9, label = RMSE_maipu_verano_monitoreo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 36, y = 6, label = Bias_maipu_verano_monitoreo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 36, y = 3, label = n_maipu_verano_monitoreo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  labs(
    title = "Maipu Verano",  # Título del gráfico
    #subtitle = "Análisis de desempeño del modelo",     # Subtítulo
    x = "SINCA",                         # Texto para el eje X
    y = "LCS Monitoreo - Modelado"                                 # Texto para el eje Y
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 10, face = "bold"),  # Tamaño del título
    plot.subtitle = element_text(size = 8),               # Tamaño del subtítulo
    axis.title = element_text(size = 8),                   # Tamaño de los títulos de los ejes
    axis.text = element_text(size = 6),                    # Tamaño de los textos de los ejes
    axis.ticks.length = unit(0.1, "cm"),                   # Tamaño de los ticks
    axis.line = element_line(size = 0.2)                   # Grosor de las líneas de los ejes
  )

plot_maipu_verano <- plot_maipu_verano +
  theme(
    legend.position = c(0.15, 0.9),  # Ubicación dentro del gráfico
    legend.text = element_text(size = 8),  # Tamaño del texto de la leyenda
    legend.title = element_text(size = 8)  # Tamaño del título de la leyenda (si lo tienes)
  )


plot_maipu_verano


#################################
#################################

#############################
#################################
##### florida_verano
data_LCS_florida_verano <- data_LCS[data_LCS$archivo == "Florida Verano",]
data_SINCA_florida_verano <- data_SINCA[data_SINCA$estacion_SINCA == "FLD",]
data_LCS_Modelo_florida_verano <- data_LCS_modelo[data_LCS_modelo$archivo == "Florida Verano",]

#meduciones LCS
data_LCS_florida_verano$date <- as.POSIXct(as.character(data_LCS_florida_verano$date), format = "%d/%m/%Y" )#"%y%m%d")
data_SINCA_florida_verano$date <- as.POSIXct(as.character(data_SINCA_florida_verano$date), format = "%d/%m/%Y" )#"%y%m%d")

data_LCS_Modelo_florida_verano$date <- as.POSIXct(as.character(data_LCS_Modelo_florida_verano$date), format = "%Y-%m-%d" )#"%y%m%d")


data_SINCA_florida_verano <- data_SINCA_florida_verano[year(data_SINCA_florida_verano$date) == 2021,]

#UniSINCA - Meidiones LCS
merge_florida_verano <- merge(data_SINCA_florida_verano, data_LCS_florida_verano, by = "date", all.y = TRUE)
#Unir SINCA y Medicioones LCS - Modelo en sitios lCS
names(merge_florida_verano)
names(data_LCS_Modelo_florida_verano)

unique(data_LCS_Modelo_florida_verano$ID_archivo)
data_LCS_Modelo_florida_verano$ID_LCS <- data_LCS_Modelo_florida_verano$ID_archivo
length(unique(merge_florida_verano$ID_LCS))


merge_florida_verano_2 <-merge(merge_florida_verano, data_LCS_Modelo_florida_verano, by = c("date","ID_LCS"), all.y = TRUE)
merge_florida_verano_2 <- merge_florida_verano_2[complete.cases(merge_florida_verano_2$Registros.completos),]
# Sinca mas cercano vs modelo en sensores LCS
modelo_raster_florida_verano <- lm(valor_raster_LCS ~ Registros.completos, data = merge_florida_verano_2)
R2_florida_verano_modelo <- round(summary(modelo_raster_florida_verano)$r.squared,2)
RMSE_florida_verano_modelo <- round(sqrt(mean(residuals(modelo_raster_florida_verano)^2)),2)
Bias_florida_verano_modelo <- round(mean(merge_florida_verano_2$valor_raster_LCS - merge_florida_verano_2$Registros.completos),2)
n_florida_verano_modelo <- nrow(merge_florida_verano_2)

#monitoreo
modelo_monitoreo_florida_verano <- lm(mean_LCS_model ~ Registros.completos, data = merge_florida_verano_2)
R2_florida_verano_monitoreo<- round(summary(modelo_monitoreo_florida_verano)$r.squared,2)
RMSE_florida_verano_monitoreo <- round(sqrt(mean(residuals(modelo_monitoreo_florida_verano)^2)),2)
Bias_florida_verano_monitoreo <- round(mean(merge_florida_verano_2$mean_LCS_model - merge_florida_verano_2$Registros.completos),2)
n_florida_verano_monitoreo <- nrow(merge_florida_verano_2)


# Crear el gráfico con ggplot2
#plot_florida_verano <- ggplot(data_florida_verano , aes(x = valor_raster, y = mean)) +
# Crear el gráfico con ggplot2
#plot_florida_verano <- ggplot(merge_florida_verano , aes(x = Registros.completos, y = mean_LCS)) +
plot_florida_verano <- ggplot() +
  
  geom_point(merge_florida_verano_2, mapping = aes(x = Registros.completos, y = mean_LCS, color = "Monitoreo"), size = 1.5, alpha = 0.6) +  # Puntos de datos (mean_LCS)
  geom_point(merge_florida_verano_2, mapping = aes(x = Registros.completos, y = valor_raster_LCS, color = "Modelo"), size = 1.5, alpha = 0.6) +  # Puntos de datos (valor_raster_LCS)
  
  geom_smooth(merge_florida_verano_2, mapping = aes(x = Registros.completos, y = valor_raster_LCS, color = "Modelo"), method = "lm", se = FALSE) +  # Línea de regresión (valor_raster_LCS)
  geom_smooth(merge_florida_verano_2, mapping = aes(x = Registros.completos, y = mean_LCS, color = "Monitoreo"), method = "lm", se = FALSE) +  # Línea de regresión (mean_LCS)
  
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 10)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 10)) +  # Ticks cada 10 en el eje X
  
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.3) +  # Línea 1:1 negra
  
  labs(color = "") +  # Título de la leyenda
  
  # Usar geom_text para agregar el texto en las coordenadas deseadas
  geom_text(aes(x = 35, y = 15, label = "Monitoreo"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 46, y = 15, label = "Modelo"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  
  ##Metricas
  geom_text(aes(x = 25, y = 12, label = "R2"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 25, y = 9, label = "RMSE"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 25, y = 6, label = "Bias"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 25, y = 3, label = "n"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  
  
  ##Raster
  geom_text(aes(x = 46, y = 12, label = R2_florida_verano_modelo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 46, y = 9, label = RMSE_florida_verano_modelo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 46, y = 6, label = Bias_florida_verano_modelo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 46, y = 3, label = n_florida_verano_modelo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  
  
  ##Monitoreo
  geom_text(aes(x = 36, y = 12, label = R2_florida_verano_monitoreo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 36, y = 9, label = RMSE_florida_verano_monitoreo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 36, y = 6, label = Bias_florida_verano_monitoreo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 36, y = 3, label = n_florida_verano_monitoreo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  labs(
    title = "Florida Verano",  # Título del gráfico
    #subtitle = "Análisis de desempeño del modelo",     # Subtítulo
    x = "SINCA",                         # Texto para el eje X
    y = "LCS Monitoreo - Modelado"                                 # Texto para el eje Y
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 10, face = "bold"),  # Tamaño del título
    plot.subtitle = element_text(size = 8),               # Tamaño del subtítulo
    axis.title = element_text(size = 8),                   # Tamaño de los títulos de los ejes
    axis.text = element_text(size = 6),                    # Tamaño de los textos de los ejes
    axis.ticks.length = unit(0.1, "cm"),                   # Tamaño de los ticks
    axis.line = element_line(size = 0.2)                   # Grosor de las líneas de los ejes
  )

plot_florida_verano <- plot_florida_verano +
  theme(
    legend.position = c(0.15, 0.9),  # Ubicación dentro del gráfico
    legend.text = element_text(size = 8),  # Tamaño del texto de la leyenda
    legend.title = element_text(size = 8)  # Tamaño del título de la leyenda (si lo tienes)
  )


plot_florida_verano


#################################
#################################

#################################
#################################

#############################
#################################
##### florida_invierno
data_LCS_florida_invierno <- data_LCS[data_LCS$archivo == "Florida Invierno",]
data_SINCA_florida_invierno <- data_SINCA[data_SINCA$estacion_SINCA == "FLD",]
data_LCS_Modelo_florida_invierno <- data_LCS_modelo[data_LCS_modelo$archivo == "Florida Invierno",]

#meduciones LCS
data_LCS_florida_invierno$date <- as.POSIXct(as.character(data_LCS_florida_invierno$date), format = "%d/%m/%Y" )#"%y%m%d")
data_SINCA_florida_invierno$date <- as.POSIXct(as.character(data_SINCA_florida_invierno$date), format = "%d/%m/%Y" )#"%y%m%d")

data_LCS_Modelo_florida_invierno$date <- as.POSIXct(as.character(data_LCS_Modelo_florida_invierno$date), format = "%Y-%m-%d" )#"%y%m%d")


data_SINCA_florida_invierno <- data_SINCA_florida_invierno[year(data_SINCA_florida_invierno$date) == 2021,]

#UniSINCA - Meidiones LCS
merge_florida_invierno <- merge(data_SINCA_florida_invierno, data_LCS_florida_invierno, by = "date", all.y = TRUE)
#Unir SINCA y Medicioones LCS - Modelo en sitios lCS
names(merge_florida_invierno)
names(data_LCS_Modelo_florida_invierno)

unique(data_LCS_Modelo_florida_invierno$ID_archivo)
data_LCS_Modelo_florida_invierno$ID_LCS <- data_LCS_Modelo_florida_invierno$ID_archivo
length(unique(merge_florida_invierno$ID_LCS))


merge_florida_invierno_2 <-merge(merge_florida_invierno, data_LCS_Modelo_florida_invierno, by = c("date","ID_LCS"), all.y = TRUE)
merge_florida_invierno_2 <- merge_florida_invierno_2[complete.cases(merge_florida_invierno_2$Registros.completos),]
# Sinca mas cercano vs modelo en sensores LCS
modelo_raster_florida_invierno <- lm(valor_raster_LCS ~ Registros.completos, data = merge_florida_invierno_2)
R2_florida_invierno_modelo <- round(summary(modelo_raster_florida_invierno)$r.squared,2)
RMSE_florida_invierno_modelo <- round(sqrt(mean(residuals(modelo_raster_florida_invierno)^2)),2)
Bias_florida_invierno_modelo <- round(mean(merge_florida_invierno_2$valor_raster_LCS - merge_florida_invierno_2$Registros.completos),2)
n_florida_invierno_modelo <- nrow(merge_florida_invierno_2)

#monitoreo
modelo_monitoreo_florida_invierno <- lm(mean_LCS_model ~ Registros.completos, data = merge_florida_invierno_2)
R2_florida_invierno_monitoreo<- round(summary(modelo_monitoreo_florida_invierno)$r.squared,2)
RMSE_florida_invierno_monitoreo <- round(sqrt(mean(residuals(modelo_monitoreo_florida_invierno)^2)),2)
Bias_florida_invierno_monitoreo <- round(mean(merge_florida_invierno_2$mean_LCS_model - merge_florida_invierno_2$Registros.completos),2)
n_florida_invierno_monitoreo <- nrow(merge_florida_invierno_2)


# Crear el gráfico con ggplot2
#plot_florida_invierno <- ggplot(data_florida_invierno , aes(x = valor_raster, y = mean)) +
# Crear el gráfico con ggplot2
#plot_florida_invierno <- ggplot(merge_florida_invierno , aes(x = Registros.completos, y = mean_LCS)) +
plot_florida_invierno <- ggplot() +
  
  geom_point(merge_florida_invierno_2, mapping = aes(x = Registros.completos, y = mean_LCS, color = "Monitoreo"), size = 1.5, alpha = 0.6) +  # Puntos de datos (mean_LCS)
  geom_point(merge_florida_invierno_2, mapping = aes(x = Registros.completos, y = valor_raster_LCS, color = "Modelo"), size = 1.5, alpha = 0.6) +  # Puntos de datos (valor_raster_LCS)
  
  geom_smooth(merge_florida_invierno_2, mapping = aes(x = Registros.completos, y = valor_raster_LCS, color = "Modelo"), method = "lm", se = FALSE) +  # Línea de regresión (valor_raster_LCS)
  geom_smooth(merge_florida_invierno_2, mapping = aes(x = Registros.completos, y = mean_LCS, color = "Monitoreo"), method = "lm", se = FALSE) +  # Línea de regresión (mean_LCS)
  
  scale_y_continuous(limits = c(0,180), breaks = seq(0, 180, by = 60)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 180), breaks = seq(0, 180, by = 60)) +  # Ticks cada 10 en el eje X
  
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.3) +  # Línea 1:1 negra
  
  labs(color = "") +  # Título de la leyenda
  
  # Usar geom_text para agregar el texto en las coordenadas deseadas
  geom_text(aes(x = 120, y = 45, label = "Monitoreo"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 160, y = 45, label = "Modelo"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  
  ##Metricas
  geom_text(aes(x = 85, y = 35, label = "R2"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 85, y = 25, label = "RMSE"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 85, y = 15, label = "Bias"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 85, y = 5, label = "n"), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  
  
  ##Raster
  geom_text(aes(x = 160, y = 35, label = R2_florida_invierno_modelo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 160, y = 25, label = RMSE_florida_invierno_modelo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 160, y = 15, label = Bias_florida_invierno_modelo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 160, y = 5, label = n_florida_invierno_modelo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  
  
  ##Monitoreo
  geom_text(aes(x = 120, y = 35, label = R2_florida_invierno_monitoreo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 120, y = 25, label = RMSE_florida_invierno_monitoreo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 120, y = 15, label = Bias_florida_invierno_monitoreo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 120, y = 5, label = n_florida_invierno_monitoreo), size = 2.5, color = "black") +  # Agregar texto en (40, 10)
  labs(
    title = "Florida Invierno",  # Título del gráfico
    #subtitle = "Análisis de desempeño del modelo",     # Subtítulo
    x = "SINCA",                         # Texto para el eje X
    y = "LCS Monitoreo - Modelado"                                 # Texto para el eje Y
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 10, face = "bold"),  # Tamaño del título
    plot.subtitle = element_text(size = 8),               # Tamaño del subtítulo
    axis.title = element_text(size = 8),                   # Tamaño de los títulos de los ejes
    axis.text = element_text(size = 6),                    # Tamaño de los textos de los ejes
    axis.ticks.length = unit(0.1, "cm"),                   # Tamaño de los ticks
    axis.line = element_line(size = 0.2)                   # Grosor de las líneas de los ejes
  )

plot_florida_invierno <- plot_florida_invierno +
  theme(
    legend.position = c(0.15, 0.9),  # Ubicación dentro del gráfico
    legend.text = element_text(size = 8),  # Tamaño del texto de la leyenda
    legend.title = element_text(size = 8)  # Tamaño del título de la leyenda (si lo tienes)
  )


plot_florida_invierno


#################################
#################################


#################################

combined_plot_1 <- grid.arrange(plot_maipu_verano, plot_maipu_invierno,nrow=1)#,
combined_plot_2 <- grid.arrange(plot_florida_verano, plot_florida_invierno,ncol = 2)

combined_plot_3 <- grid.arrange(plot_vitacura_verano, plot_vitacura_invierno,nrow = 1)
combined_plot_4 <- grid.arrange(plot_Qui_verano, plot_Qui_invierno,nrow = 1)
combined_plot_5 <- grid.arrange(plot_maipu_verano, plot_maipu_invierno,nrow = 1)#
                                nrow = 1, ncol = 2)