# Mi valor de referencia es el modelo no las mediciones de LCS
merged_df_subt2 <- merged_df_subt[complete.cases(merged_df_subt$mean),]
# Ajuste del modelo de regresión lineal
nombreModelo <- "07-RF_esp_cv_M6-180225-CH"
merged_df_BSQ <- merged_df_subt2[merged_df_subt2$estacion == "BSQ",]
modelo_BSQ <- lm( mean~valor_raster , data = merged_df_BSQ)
R2_BSQ <- summary(modelo_BSQ)$r.squared
RMSE_BSQ <- sqrt(mean(residuals(modelo_BSQ)^2))
Bias_BSQ <- mean(merged_df_BSQ$mean - merged_df_BSQ$valor_raster)
n_BSQ <- nrow(merged_df_BSQ)

# Crear el gráfico con ggplot2
plot_BSQ <- ggplot(merged_df_subt, aes(y = mean, x= valor_raster)) +
  geom_abline(slope = 1, intercept = 0,  color = "black", size = 0.5) +  # Línea 1:1
  
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 140),breaks = seq(0, 140, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 140),breaks = seq(0, 140, by = 40)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "Monitoreo",
    y = "Modelo",
    subtitle =nombreModelo,
    title = "BSQ"
  ) +
  ##Metricas
  geom_text(aes(x = 85, y = 35, label = "R2 ="), size = 3, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 85, y = 25, label = "RMSE ="), size = 3, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 85, y = 15, label = "Bias ="), size = 3, color = "black") +  # Agregar texto en (40, 10)
  #geom_text(aes(x = 85, y = 5, label = "n ="), size = 3, color = "black") +  # Agregar texto en (40, 10)
  
  ##Monitoreo
  geom_text(aes(x = 100, y = 35, label = round(R2_BSQ,2)), size = 3, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 100, y = 25, label = round(RMSE_BSQ,2)), size = 3, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 100, y = 15, label = round(Bias_BSQ,2)), size = 3, color = "black") +  # Agregar texto en (40, 10)
  #geom_text(aes(x = 100, y = 5, label = n_BSQ), size = 3, color = "black") +  # Agregar texto en (40, 10)
  
  theme_classic() #+
plot_BSQ

#################################
################################
#################################
################################
merged_df_subt <- read.csv("merged_df_subt.csv")
merged_df_subt$estacion<-merged_df_subt$estacion.x
merged_df_subt$date<- as.POSIXct(merged_df_subt$date, format = "%Y-%m-%d")
merged_df_subt$mean <- merged_df_subt$Registros.completos

# Mi valor de referencia es el modelo no las mediciones de LCS
merged_df_subt2 <- merged_df_subt[complete.cases(merged_df_subt$mean),]
unique(merged_df_subt2$estacion)
# Ajuste del modelo de regresión lineal
nombreModelo <- "07-RF_esp_cv_M6-180225-CH"
merged_df_CNA <- merged_df_subt2[merged_df_subt2$estacion == "CNA",]
modelo_CNA <- lm( mean~valor_raster , data = merged_df_CNA)
R2_CNA <- summary(modelo_CNA)$r.squared
RMSE_CNA <- sqrt(mean(residuals(modelo_CNA)^2))
Bias_CNA <- mean(merged_df_CNA$mean - merged_df_CNA$valor_raster)
n_CNA <- nrow(merged_df_CNA)

# Crear el gráfico con ggplot2
plot_CNA <- ggplot(merged_df_subt, aes(y = mean, x= valor_raster)) +
  geom_abline(slope = 1, intercept = 0,  color = "black", size = 0.5) +  # Línea 1:1
  
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 140),breaks = seq(0, 140, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 140),breaks = seq(0, 140, by = 40)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "Monitoreo",
    y = "Modelo",
    subtitle =nombreModelo,
    title = "CNA"
  ) +
  ##Metricas
  geom_text(aes(x = 85, y = 35, label = "R2 ="), size = 3, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 85, y = 25, label = "RMSE ="), size = 3, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 85, y = 15, label = "Bias ="), size = 3, color = "black") +  # Agregar texto en (40, 10)
  #geom_text(aes(x = 85, y = 5, label = "n ="), size = 3, color = "black") +  # Agregar texto en (40, 10)
  
  ##Monitoreo
  geom_text(aes(x = 100, y = 35, label = round(R2_CNA,2)), size = 3, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 100, y = 25, label = round(RMSE_CNA,2)), size = 3, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 100, y = 15, label = round(Bias_CNA,2)), size = 3, color = "black") +  # Agregar texto en (40, 10)
  #geom_text(aes(x = 100, y = 5, label = n_CNA), size = 3, color = "black") +  # Agregar texto en (40, 10)
  
  theme_classic() #+
plot_CNA

#################################
################################
#################################
################################
merged_df_subt <- read.csv("merged_df_subt.csv")
merged_df_subt$estacion<-merged_df_subt$estacion.x
merged_df_subt$date<- as.POSIXct(merged_df_subt$date, format = "%Y-%m-%d")
merged_df_subt$mean <- merged_df_subt$Registros.completos

# Mi valor de referencia es el modelo no las mediciones de LCS
merged_df_subt2 <- merged_df_subt[complete.cases(merged_df_subt$mean),]
unique(merged_df_subt2$estacion)
# Ajuste del modelo de regresión lineal
nombreModelo <- "07-RF_esp_cv_M6-180225-CH"
merged_df_CDE <- merged_df_subt2[merged_df_subt2$estacion == "CDE",]
modelo_CDE <- lm( mean~valor_raster , data = merged_df_CDE)
R2_CDE <- summary(modelo_CDE)$r.squared
RMSE_CDE <- sqrt(mean(residuals(modelo_CDE)^2))
Bias_CDE <- mean(merged_df_CDE$mean - merged_df_CDE$valor_raster)
n_CDE <- nrow(merged_df_CDE)

# Crear el gráfico con ggplot2
plot_CDE <- ggplot(merged_df_subt, aes(y = mean, x= valor_raster)) +
  geom_abline(slope = 1, intercept = 0,  color = "black", size = 0.5) +  # Línea 1:1
  
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 140),breaks = seq(0, 140, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 140),breaks = seq(0, 140, by = 40)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "Monitoreo",
    y = "Modelo",
    subtitle =nombreModelo,
    title = "CDE"
  ) +
  ##Metricas
  geom_text(aes(x = 85, y = 35, label = "R2 = "), size = 3, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 85, y = 25, label = "RMSE ="), size = 3, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 85, y = 15, label = "Bias ="), size = 3, color = "black") +  # Agregar texto en (40, 10)
  #geom_text(aes(x = 85, y = 5, label = "n ="), size = 3, color = "black") +  # Agregar texto en (40, 10)
  
  ##Monitoreo
  geom_text(aes(x = 100, y = 35, label = round(R2_CDE,2)), size = 3, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 100, y = 25, label = round(RMSE_CDE,2)), size = 3, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 100, y = 15, label = round(Bias_CDE,2)), size = 3, color = "black") +  # Agregar texto en (40, 10)
  #geom_text(aes(x = 100, y = 5, label = n_CDE), size = 3, color = "black") +  # Agregar texto en (40, 10)
  
  theme_classic() #+
plot_CDE


#################################
################################
#################################
################################
merged_df_subt <- read.csv("merged_df_subt.csv")
merged_df_subt$estacion<-merged_df_subt$estacion.x
merged_df_subt$date<- as.POSIXct(merged_df_subt$date, format = "%Y-%m-%d")
merged_df_subt$mean <- merged_df_subt$Registros.completos

# Mi valor de referencia es el modelo no las mediciones de LCS
merged_df_subt2 <- merged_df_subt[complete.cases(merged_df_subt$mean),]
unique(merged_df_subt2$estacion)
# Ajuste del modelo de regresión lineal
nombreModelo <- "07-RF_esp_cv_M6-180225-CH"
merged_df_FLD <- merged_df_subt2[merged_df_subt2$estacion == "FLD",]
modelo_FLD <- lm( mean~valor_raster , data = merged_df_FLD)
R2_FLD <- summary(modelo_FLD)$r.squared
RMSE_FLD <- sqrt(mean(residuals(modelo_FLD)^2))
Bias_FLD <- mean(merged_df_FLD$mean - merged_df_FLD$valor_raster)
n_FLD <- nrow(merged_df_FLD)

# Crear el gráfico con ggplot2
plot_FLD <- ggplot(merged_df_subt, aes(y = mean, x= valor_raster)) +
  geom_abline(slope = 1, intercept = 0,  color = "black", size = 0.5) +  # Línea 1:1
  
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 140),breaks = seq(0, 140, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 140),breaks = seq(0, 140, by = 40)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "Monitoreo",
    y = "Modelo",
    subtitle =nombreModelo,
    title = "FLD"
  ) +
  ##Metricas
  geom_text(aes(x = 85, y = 35, label = "R2 ="), size = 3, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 85, y = 25, label = "RMSE ="), size = 3, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 85, y = 15, label = "Bias ="), size = 3, color = "black") +  # Agregar texto en (40, 10)
  #geom_text(aes(x = 85, y = 5, label = "n ="), size = 3, color = "black") +  # Agregar texto en (40, 10)
  
  ##Monitoreo
  geom_text(aes(x = 100, y = 35, label = round(R2_FLD,2)), size = 3, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 100, y = 25, label = round(RMSE_FLD,2)), size =3, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 100, y = 15, label = round(Bias_FLD,2)), size = 3, color = "black") +  # Agregar texto en (40, 10)
  #geom_text(aes(x = 100, y = 5, label = n_FLD), size = 3, color = "black") +  # Agregar texto en (40, 10)
  
  theme_classic() #+
plot_FLD


#################################
################################
#################################
################################
merged_df_subt <- read.csv("merged_df_subt.csv")
merged_df_subt$estacion<-merged_df_subt$estacion.x
merged_df_subt$date<- as.POSIXct(merged_df_subt$date, format = "%Y-%m-%d")
merged_df_subt$mean <- merged_df_subt$Registros.completos

# Mi valor de referencia es el modelo no las mediciones de LCS
merged_df_subt2 <- merged_df_subt[complete.cases(merged_df_subt$mean),]
unique(merged_df_subt2$estacion)
# Ajuste del modelo de regresión lineal
nombreModelo <- "07-RF_esp_cv_M6-180225-CH"
merged_df_PDH <- merged_df_subt2[merged_df_subt2$estacion == "PDH",]
modelo_PDH <- lm( mean~valor_raster , data = merged_df_PDH)
R2_PDH <- summary(modelo_PDH)$r.squared
RMSE_PDH <- sqrt(mean(residuals(modelo_PDH)^2))
Bias_PDH <- mean(merged_df_PDH$mean - merged_df_PDH$valor_raster)
#n_PDH <- nrow(merged_df_PDH)

# Crear el gráfico con ggplot2
plot_PDH <- ggplot(merged_df_subt, aes(y = mean, x= valor_raster)) +
  geom_abline(slope = 1, intercept = 0,  color = "black", size = 0.5) +  # Línea 1:1
  
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 140),breaks = seq(0, 140, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 140),breaks = seq(0, 140, by = 40)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "Monitoreo",
    y = "Modelo",
    subtitle =nombreModelo,
    title = "PDH"
      
  ) +
  ##Metricas
  geom_text(aes(x = 85, y = 35, label = "R2 ="), size = 3, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 85, y = 25, label = "RMSE ="), size = 3, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 85, y = 15, label = "Bias ="), size = 3, color = "black") +  # Agregar texto en (40, 10)
  #geom_text(aes(x = 85, y = 5, label = "n ="), size = 3, color = "black") +  # Agregar texto en (40, 10)
  
  ##Monitoreo
  geom_text(aes(x = 100, y = 35, label = round(R2_PDH,2)), size = 3, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 100, y = 25, label = round(RMSE_PDH,2)), size = 3, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 100, y = 15, label = round(Bias_PDH,2)), size = 3, color = "black") +  # Agregar texto en (40, 10)
  #geom_text(aes(x = 100, y = 5, label = n_PDH), size = 3, color = "black") +  # Agregar texto en (40, 10)
  
  theme_classic() #+
plot_PDH


#################################
################################
#################################
################################
merged_df_subt <- read.csv("merged_df_subt.csv")
merged_df_subt$estacion<-merged_df_subt$estacion.x
merged_df_subt$date<- as.POSIXct(merged_df_subt$date, format = "%Y-%m-%d")
merged_df_subt$mean <- merged_df_subt$Registros.completos

# Mi valor de referencia es el modelo no las mediciones de LCS
merged_df_subt2 <- merged_df_subt[complete.cases(merged_df_subt$mean),]
unique(merged_df_subt2$estacion)
# Ajuste del modelo de regresión lineal
nombreModelo <- "07-RF_esp_cv_M6-180225-CH"
merged_df_PTA <- merged_df_subt2[merged_df_subt2$estacion == "PTA",]
modelo_PTA <- lm( mean~valor_raster , data = merged_df_PTA)
R2_PTA <- summary(modelo_PTA)$r.squared
RMSE_PTA <- sqrt(mean(residuals(modelo_PTA)^2))
Bias_PTA <- mean(merged_df_PTA$mean - merged_df_PTA$valor_raster)
n_PTA <- nrow(merged_df_PTA)

# Crear el gráfico con ggplot2
plot_PTA <- ggplot(merged_df_subt, aes(y = mean, x= valor_raster)) +
  geom_abline(slope = 1, intercept = 0,  color = "black", size = 0.5) +  # Línea 1:1
  
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 140),breaks = seq(0, 140, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 140),breaks = seq(0, 140, by = 40)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "Monitoreo",
    y = "Modelo",
    subtitle =nombreModelo,
    title = "PTA"
    
  ) +
  ##Metricas
  geom_text(aes(x = 85, y = 35, label = "R2 ="), size = 3, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 85, y = 25, label = "RMSE ="), size = 3, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 85, y = 15, label = "Bias ="), size = 3, color = "black") +  # Agregar texto en (40, 10)
  #geom_text(aes(x = 85, y = 5, label = "n ="), size = 3, color = "black") +  # Agregar texto en (40, 10)
  
  ##Monitoreo
  geom_text(aes(x = 100, y = 35, label = round(R2_PTA,2)), size = 3, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 100, y = 25, label = round(RMSE_PTA,2)), size = 3, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 100, y = 15, label = round(Bias_PTA,2)), size = 3, color = "black") +  # Agregar texto en (40, 10)
  #geom_text(aes(x = 100, y = 5, label = n_PTA), size = 3, color = "black") +  # Agregar texto en (40, 10)
  
  theme_classic() #+
plot_PTA


#################################
################################
#################################
################################
merged_df_subt <- read.csv("merged_df_subt.csv")
merged_df_subt$estacion<-merged_df_subt$estacion.x
merged_df_subt$date<- as.POSIXct(merged_df_subt$date, format = "%Y-%m-%d")
merged_df_subt$mean <- merged_df_subt$Registros.completos

# Mi valor de referencia es el modelo no las mediciones de LCS
merged_df_subt2 <- merged_df_subt[complete.cases(merged_df_subt$mean),]
unique(merged_df_subt2$estacion)
# Ajuste del modelo de regresión lineal
nombreModelo <- "07-RF_esp_cv_M6-180225-CH"
merged_df_QUI <- merged_df_subt2[merged_df_subt2$estacion == "QUI",]
modelo_QUI <- lm( mean~valor_raster , data = merged_df_QUI)
R2_QUI <- summary(modelo_QUI)$r.squared
RMSE_QUI <- sqrt(mean(residuals(modelo_QUI)^2))
Bias_QUI <- mean(merged_df_QUI$mean - merged_df_QUI$valor_raster)
n_QUI <- nrow(merged_df_QUI)

# Crear el gráfico con ggplot2
plot_QUI <- ggplot(merged_df_subt, aes(y = mean, x= valor_raster)) +
  geom_abline(slope = 1, intercept = 0,  color = "black", size = 0.5) +  # Línea 1:1
  
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 140),breaks = seq(0, 140, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 140),breaks = seq(0, 140, by = 40)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "Monitoreo",
    y = "Modelo",
    subtitle =nombreModelo,
    title = "QUI"
    
  ) +
  ##Metricas
  geom_text(aes(x = 85, y = 35, label = "R2 ="), size = 3, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 85, y = 25, label = "RMSE ="), size = 3, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 85, y = 15, label = "Bias ="), size = 3, color = "black") +  # Agregar texto en (40, 10)
  #geom_text(aes(x = 85, y = 5, label = "n ="), size = 3, color = "black") +  # Agregar texto en (40, 10)
  
  ##Monitoreo
  geom_text(aes(x = 100, y = 35, label = round(R2_QUI,2)), size = 3, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 100, y = 25, label = round(RMSE_QUI,2)), size = 3, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 100, y = 15, label = round(Bias_QUI,2)), size = 3, color = "black") +  # Agregar texto en (40, 10)
  #geom_text(aes(x = 100, y = 5, label = n_QUI), size = 3, color = "black") +  # Agregar texto en (40, 10)
  
  theme_classic() #+
plot_QUI




#################################
################################
#################################
################################
merged_df_subt <- read.csv("merged_df_subt.csv")
merged_df_subt$estacion<-merged_df_subt$estacion.x
merged_df_subt$date<- as.POSIXct(merged_df_subt$date, format = "%Y-%m-%d")
merged_df_subt$mean <- merged_df_subt$Registros.completos

# Mi valor de referencia es el modelo no las mediciones de LCS
merged_df_subt2 <- merged_df_subt[complete.cases(merged_df_subt$mean),]
unique(merged_df_subt2$estacion)
# Ajuste del modelo de regresión lineal
nombreModelo <- "07-RF_esp_cv_M6-180225-CH"
merged_df_CER_II <- merged_df_subt2[merged_df_subt2$estacion == "CER-II",]
modelo_CER_II <- lm( mean~valor_raster , data = merged_df_CER_II)
R2_CER_II <- summary(modelo_CER_II)$r.squared
RMSE_CER_II <- sqrt(mean(residuals(modelo_CER_II)^2))
Bias_CER_II <- mean(merged_df_CER_II$mean - merged_df_CER_II$valor_raster)
n_CER_II <- nrow(merged_df_CER_II)

# Crear el gráfico con ggplot2
plot_CER_II <- ggplot(merged_df_subt, aes(y = mean, x= valor_raster)) +
  geom_abline(slope = 1, intercept = 0,  color = "black", size = 0.5) +  # Línea 1:1
  
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 140),breaks = seq(0, 140, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 140),breaks = seq(0, 140, by = 40)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "Monitoreo",
    y = "Modelo",
    subtitle =nombreModelo,
    title = "CER-II"
    
  ) +
  ##Metricas
  geom_text(aes(x = 85, y = 35, label = "R2 ="), size = 3, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 85, y = 25, label = "RMSE ="), size = 3, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 85, y = 15, label = "Bias ="), size = 3, color = "black") +  # Agregar texto en (40, 10)
  #geom_text(aes(x = 85, y = 5, label = "n ="), size = 3, color = "black") +  # Agregar texto en (40, 10)
  
  ##Monitoreo
  geom_text(aes(x = 100, y = 35, label = round(R2_CER_II,2)), size = 3, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 100, y = 25, label = round(RMSE_CER_II,2)), size = 3, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 100, y = 15, label = round(Bias_CER_II,2)), size = 3, color = "black") +  # Agregar texto en (40, 10)
  #geom_text(aes(x = 93, y = 5, label = n_CER_II), size = 3, color = "black") +  # Agregar texto en (40, 10)
  
  theme_classic() #+
plot_CER_II



#################################
################################
#################################
################################
merged_df_subt <- read.csv("merged_df_subt.csv")
merged_df_subt$estacion<-merged_df_subt$estacion.x
merged_df_subt$date<- as.POSIXct(merged_df_subt$date, format = "%Y-%m-%d")
merged_df_subt$mean <- merged_df_subt$Registros.completos

# Mi valor de referencia es el modelo no las mediciones de LCS
merged_df_subt2 <- merged_df_subt[complete.cases(merged_df_subt$mean),]
unique(merged_df_subt2$estacion)
# Ajuste del modelo de regresión lineal
nombreModelo <- "07-RF_esp_cv_M6-180225-CH"
merged_df_OHG <- merged_df_subt2[merged_df_subt2$estacion == "OHG",]
modelo_OHG <- lm( mean~valor_raster , data = merged_df_OHG)
R2_OHG <- summary(modelo_OHG)$r.squared
RMSE_OHG <- sqrt(mean(residuals(modelo_OHG)^2))
Bias_OHG <- mean(merged_df_OHG$mean - merged_df_OHG$valor_raster)
n_OHG <- nrow(merged_df_OHG)

# Crear el gráfico con ggplot2
plot_OHG <- ggplot(merged_df_subt, aes(y = mean, x= valor_raster)) +
  geom_abline(slope = 1, intercept = 0,  color = "black", size = 0.5) +  # Línea 1:1
  
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 140),breaks = seq(0, 140, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 140),breaks = seq(0, 140, by = 40)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "Monitoreo",
    y = "Modelo",
    subtitle =nombreModelo,
    title = "OHG"
    
  ) +
  ##Metricas
  geom_text(aes(x = 85, y = 35, label = "R2 ="), size = 3, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 85, y = 25, label = "RMSE ="), size = 3, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 85, y = 15, label = "Bias ="), size = 3, color = "black") +  # Agregar texto en (40, 10)
  #geom_text(aes(x = 85, y = 5, label = "n ="), size = 3, color = "black") +  # Agregar texto en (40, 10)
  
  ##Monitoreo
  geom_text(aes(x = 93, y = 35, label = round(R2_OHG,2)), size = 3, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 93, y = 25, label = round(RMSE_OHG,2)), size = 3, color = "black") +  # Agregar texto en (40, 10)
  geom_text(aes(x = 93, y = 15, label = round(Bias_OHG,2)), size = 3, color = "black") +  # Agregar texto en (40, 10)
  #geom_text(aes(x = 93, y = 5, label = n_OHG), size = 3, color = "black") +  # Agregar texto en (40, 10)
  
  theme_classic() #+
plot_OHG


# plot_CDE,plot_FLD, plot_PDH, plot_PTA, plot_QUI, plot_CER_II, plot
combined_plot_1 <- grid.arrange(plot_BSQ,plot_CNA,nrow=1)#,
combined_plot_1 <- grid.arrange(plot_CDE,plot_FLD,nrow=1)#,
combined_plot_1 <- grid.arrange(plot_PDH,plot_PTA,nrow=1)#,
combined_plot_1 <- grid.arrange(plot_QUI,plot_CER_II,nrow=1)#,
                                nrow=2)#,
combined_plot_1 <- grid.arrange(plot_OHG,nrow=1)#,
