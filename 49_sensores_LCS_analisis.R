#########################################################
######################################################
#Analsiis de los LCS
data_ch <- read.csv("D:/Josefina/Proyectos/ProyectoChile/CH/modelos/Salidas/Sensores/sensores_modelos_validacion.csv")
data_estaciones <- read.csv("D:/Josefina/Proyectos/ProyectoChile/CH/proceed/06_estaciones/CH_estaciones.csv")

data_LCS <- data.frame(ID_archivo = data_ch$ID_archivo,date= data_ch$date,
                             archivo = data_ch$archivo, ID_LCS = data_ch$ID,
                             estacion_LCS = data_ch$estacion, valor_raster_LCS=data_ch$valor_raster,
                             mean_LCS = data_ch$mean)
data_SINCA <- data.frame(date=data_estaciones$date, ID_SINCA =data_estaciones$ID,
                              estacion_SINCA = data_estaciones$estacion, Registros.validados=data_estaciones$Registros.validados,
                              Registros.preliminares = data_estaciones$Registros.preliminares,Registros.no.validados= data_estaciones$Registros.no.validados,
                              Registros.completos = data_estaciones$Registros.completos)
data_SINCA_Prueba <- data_SINCA[data_SINCA$estacion_SINCA == "CER-II",]
data_SINCA_Prueba$date <- as.POSIXct(as.character(data_SINCA_Prueba$date), format = "%d/%m/%Y" )#"%y%m%d")
unique(year(data_SINCA_Prueba$date))
#################################
##### Quilicura_verano
data_LCS_Qui_verano <- data_LCS[data_LCS$archivo == "Quilicura Verano",]
data_SINCA_Qui <- data_SINCA[data_SINCA$estacion_SINCA == "QUI",]

data_LCS_Qui_verano$date <- as.POSIXct(as.character(data_LCS_Qui_verano$date), format = "%Y-%m-%d" )#"%y%m%d")
data_SINCA_Qui$date <- as.POSIXct(as.character(data_SINCA_Qui$date), format = "%d/%m/%Y" )#"%y%m%d")


data_SINCA_Qui <- data_SINCA_Qui[year(data_SINCA_Qui$date) == 2021,]
merge_Qui_verano <- merge(data_SINCA_Qui, data_LCS_Qui_verano, by = "date", all.y = TRUE)

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
modelo_Qui_verano <- lm(valor_raster_LCS ~ Registros.completos, data = merge_Qui_verano)
R2_Quilicura_verano <- summary(modelo_Qui_verano)$r.squared
RMSE_Quilicura_verano <- sqrt(mean(residuals(modelo_Qui_verano)^2))
Bias_Quilicura_verano <- mean(merge_Qui_verano$valor_raster_LCS - merge_Qui_verano$Registros.completos)
n_Quilicura_verano <- nrow(merge_Qui_verano)

# Crear el gráfico con ggplot2
#plot_Quilicura_verano <- ggplot(data_Quilicura_verano , aes(x = valor_raster, y = mean)) +
# Crear el gráfico con ggplot2
#plot_Qui_verano <- ggplot(merge_Qui_verano , aes(x = Registros.completos, y = mean_LCS)) +
plot_Qui_verano <- ggplot(merge_Qui_verano , aes(x = Registros.completos, y = valor_raster_LCS)) +
  
 geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#+, linetype = "dashed") +  # Línea de regresión
  #scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  #scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_y_continuous(limits = c(0, 50),breaks = seq(0, 50, by = 10)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 50),breaks = seq(0, 50, by = 10)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "SINCA",
    y = "Modelo (LCS)",
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
plot_Qui_verano

###########################################
##### Quilicura_invierno

unique(data_LCS$archivo)
data_LCS_Qui_invierno <- data_LCS[data_LCS$archivo == "Quilicura_invierno",]
data_SINCA_Qui <- data_SINCA[data_SINCA$estacion_SINCA == "QUI",]

data_LCS_Qui_invierno$date <- as.POSIXct(as.character(data_LCS_Qui_invierno$date), format = "%Y-%m-%d" )#"%y%m%d")
data_SINCA_Qui$date <- as.POSIXct(as.character(data_SINCA_Qui$date), format = "%d/%m/%Y" )#"%y%m%d")


data_SINCA_Qui <- data_SINCA_Qui[year(data_SINCA_Qui$date) == 2021,]
merge_Qui_invierno <- merge(data_SINCA_Qui, data_LCS_Qui_invierno, by = "date", all.y = TRUE)

ZZ_prueba <- merge_Qui_invierno[complete.cases(merge_Qui_invierno$mean_LCS),]
nrow(merge_Qui_invierno) == nrow(ZZ_prueba)
nrow(merge_Qui_invierno)
# Ajuste del modelo de regresión lineal
#modelo_Quilicura_invierno <- lm(mean ~ valor_raster, data = data_Quilicura_invierno)
# R2_Quilicura_invierno <- summary(modelo_Quilicura_invierno)$r.squared
# RMSE_Quilicura_invierno <- sqrt(mean(residuals(modelo_Quilicura_invierno)^2))
# Bias_Quilicura_invierno <- mean(data_Quilicura_invierno$mean - data_Quilicura_invierno$valor_raster)
# n_Quilicura_invierno <- nrow(data_Quilicura_invierno)

# ## LCS VS SINCA MAS CERCANO
# modelo_Qui_invierno <- lm(mean_LCS ~ Registros.completos, data = merge_Qui_invierno)
# R2_Quilicura_invierno <- summary(modelo_Qui_invierno)$r.squared
# RMSE_Quilicura_invierno <- sqrt(mean(residuals(modelo_Qui_invierno)^2))
# Bias_Quilicura_invierno <- mean(merge_Qui_invierno$mean_LCS - merge_Qui_invierno$Registros.completos)
# n_Quilicura_invierno <- nrow(merge_Qui_invierno)

## LCS VS SINCA MAS CERCANO
modelo_Qui_invierno <- lm(valor_raster_LCS ~ Registros.completos, data = merge_Qui_invierno)
R2_Quilicura_invierno <- summary(modelo_Qui_invierno)$r.squared
RMSE_Quilicura_invierno <- sqrt(mean(residuals(modelo_Qui_invierno)^2))
Bias_Quilicura_invierno <- mean(merge_Qui_invierno$valor_raster_LCS - merge_Qui_invierno$Registros.completos)
n_Quilicura_invierno <- nrow(merge_Qui_invierno)


# Crear el gráfico con ggplot2
# plot_Quilicura_invierno <- ggplot(data_Quilicura_invierno , aes(x = valor_raster, y = mean)) +
#plot_Qui_invierno <- ggplot(merge_Qui_invierno , aes(x = Registros.completos, y = mean_LCS)) +
plot_Qui_invierno <- ggplot(merge_Qui_invierno , aes(x = Registros.completos, y = valor_raster_LCS)) +
  
 geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#+, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 180),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 180),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  #scale_y_continuous(limits = c(0, 50),breaks = seq(0, 50, by = 10)) +  # Ticks cada 10 en el eje Y
  #scale_x_continuous(limits = c(0, 50),breaks = seq(0, 50, by = 10)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "SINCA",
    y = "Modelo (LCS)",
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
plot_Qui_invierno




#################################
#################################
##### vitacura verano
unique(data_LCS$archivo)
unique(data_SINCA$estacion_SINCA)
data_LCS_Vitacura_verano <- data_LCS[data_LCS$archivo == "vitacura verano",]
data_SINCA_Vitacura <- data_SINCA[data_SINCA$estacion_SINCA == "CDE",]

data_LCS_Vitacura_verano$date <- as.POSIXct(as.character(data_LCS_Vitacura_verano$date), format = "%Y-%m-%d" )#"%y%m%d")
data_SINCA_Vitacura$date <- as.POSIXct(as.character(data_SINCA_Vitacura$date), format = "%d/%m/%Y" )#"%y%m%d")


data_SINCA_Vitacura <- data_SINCA_Vitacura[year(data_SINCA_Vitacura$date) == 2021,]
merge_Vitacura_verano <- merge(data_SINCA_Vitacura, data_LCS_Vitacura_verano, by = "date", all.y = TRUE)
# Ajuste del modelo de regresión lineal
#modelo_Vitacura_verano <- lm(mean ~ valor_raster, data = data_Vitacura_verano)
# R2_Vitacura_verano <- summary(modelo_Vitacura_verano)$r.squared
# RMSE_Vitacura_verano <- sqrt(mean(residuals(modelo_Vitacura_verano)^2))
# Bias_Vitacura_verano <- mean(data_Vitacura_verano$mean - data_Vitacura_verano$valor_raster)
# n_Vitacura_verano <- nrow(data_Vitacura_verano)


# # Sinca mas cercano vs LCS
# modelo_Vitacura_verano <- lm(mean_LCS ~ Registros.completos, data = merge_Vitacura_verano)
# R2_Vitacura_verano <- summary(modelo_Vitacura_verano)$r.squared
# RMSE_Vitacura_verano <- sqrt(mean(residuals(modelo_Vitacura_verano)^2))
# Bias_Vitacura_verano <- mean(merge_Vitacura_verano$mean_LCS - merge_Vitacura_verano$Registros.completos)
# n_Vitacura_verano <- nrow(merge_Vitacura_verano)

# Sinca mas cercano vs Modelo(SINCA)
modelo_Vitacura_verano <- lm(valor_raster_LCS ~ Registros.completos, data = merge_Vitacura_verano)
R2_Vitacura_verano <- summary(modelo_Vitacura_verano)$r.squared
RMSE_Vitacura_verano <- sqrt(mean(residuals(modelo_Vitacura_verano)^2))
Bias_Vitacura_verano <- mean(merge_Vitacura_verano$valor_raster_LCS - merge_Vitacura_verano$Registros.completos)
n_Vitacura_verano <- nrow(merge_Vitacura_verano)



# Crear el gráfico con ggplot2
#plot_Vitacura_verano <- ggplot(data_Vitacura_verano , aes(x = valor_raster, y = mean)) +
# Crear el gráfico con ggplot2
#plot_Vitacura_verano <- ggplot(merge_Vitacura_verano , aes(x = Registros.completos, y = mean_LCS)) +
  plot_Vitacura_verano <- ggplot(merge_Vitacura_verano , aes(x = Registros.completos, y = valor_raster_LCS)) +
  
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#+, linetype = "dashed") +  # Línea de regresión
  #scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  #scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_y_continuous(limits = c(0, 50),breaks = seq(0, 50, by = 10)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 50),breaks = seq(0, 50, by = 10)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "SINCA",
    y = "Modelo (LCS)",
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
##### vitacura invierno
unique(data_LCS$archivo)
unique(data_SINCA$estacion_SINCA)
data_LCS_Vitacura_invierno <- data_LCS[data_LCS$archivo == "Vitacura Invierno",]
data_SINCA_Vitacura <- data_SINCA[data_SINCA$estacion_SINCA == "CDE",]

data_LCS_Vitacura_invierno$date <- as.POSIXct(as.character(data_LCS_Vitacura_invierno$date), format = "%Y-%m-%d" )#"%y%m%d")
data_SINCA_Vitacura$date <- as.POSIXct(as.character(data_SINCA_Vitacura$date), format = "%d/%m/%Y" )#"%y%m%d")


data_SINCA_Vitacura <- data_SINCA_Vitacura[year(data_SINCA_Vitacura$date) == 2021,]
merge_Vitacura_invierno <- merge(data_SINCA_Vitacura, data_LCS_Vitacura_invierno, by = "date", all.y = TRUE)
# Ajuste del modelo de regresión lineal
#modelo_Vitacura_invierno <- lm(mean ~ valor_raster, data = data_Vitacura_invierno)
# R2_Vitacura_invierno <- summary(modelo_Vitacura_invierno)$r.squared
# RMSE_Vitacura_invierno <- sqrt(mean(residuals(modelo_Vitacura_invierno)^2))
# Bias_Vitacura_invierno <- mean(data_Vitacura_invierno$mean - data_Vitacura_invierno$valor_raster)
# n_Vitacura_invierno <- nrow(data_Vitacura_invierno)


# # # Sinca mas cercano vs LCS
# modelo_Vitacura_invierno <- lm(mean_LCS ~ Registros.completos, data = merge_Vitacura_invierno)
# R2_Vitacura_invierno <- summary(modelo_Vitacura_invierno)$r.squared
# RMSE_Vitacura_invierno <- sqrt(mean(residuals(modelo_Vitacura_invierno)^2))
# Bias_Vitacura_invierno <- mean(merge_Vitacura_invierno$mean_LCS - merge_Vitacura_invierno$Registros.completos)
# n_Vitacura_invierno <- nrow(merge_Vitacura_invierno)


# Sinca mas cercano vs modelo(LCS)
modelo_Vitacura_invierno <- lm(valor_raster_LCS ~ Registros.completos, data = merge_Vitacura_invierno)
R2_Vitacura_invierno <- summary(modelo_Vitacura_invierno)$r.squared
RMSE_Vitacura_invierno <- sqrt(mean(residuals(modelo_Vitacura_invierno)^2))
Bias_Vitacura_invierno <- mean(merge_Vitacura_invierno$valor_raster_LCS - merge_Vitacura_invierno$Registros.completos)
n_Vitacura_invierno <- nrow(merge_Vitacura_invierno)

# Crear el gráfico con ggplot2
#plot_Vitacura_invierno <- ggplot(data_Vitacura_invierno , aes(x = valor_raster, y = mean)) +
# Crear el gráfico con ggplot2
#plot_Vitacura_invierno <- ggplot(merge_Vitacura_invierno , aes(x = Registros.completos, y = mean_LCS)) +
  plot_Vitacura_invierno <- ggplot(merge_Vitacura_invierno , aes(x = Registros.completos, y = valor_raster_LCS)) +
  
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#+, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  #scale_y_continuous(limits = c(0, 50),breaks = seq(0, 50, by = 10)) +  # Ticks cada 10 en el eje Y
  #scale_x_continuous(limits = c(0, 50),breaks = seq(0, 50, by = 10)) +  # Ticks cada 10 en el eje Y
  
  labs(
    x = "SINCA",
    y = "Modelo (LCS)",
    title = "Vitacura invierno",
    subtitle = paste(
      "R2 =", round(R2_Vitacura_invierno , 3),
      "| RMSE =", round(RMSE_Vitacura_invierno , 2),
      "| Bias =", round(Bias_Vitacura_invierno , 2),
      "| n =", n_Vitacura_invierno
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
plot_Vitacura_invierno

#################################
##### Maipu invierno
unique(data_LCS$archivo)
unique(data_SINCA$estacion_SINCA)
rm(list = setdiff(ls(), c("data_LCS","data_SINCA")))
data_LCS_Maipu_invierno <- data_LCS[data_LCS$archivo == "Maipu Invierno",]
data_SINCA_Maipu <- data_SINCA[data_SINCA$estacion_SINCA == "PDH",]

data_LCS_Maipu_invierno$date <- as.POSIXct(as.character(data_LCS_Maipu_invierno$date), format = "%Y-%m-%d" )#"%y%m%d")
data_SINCA_Maipu$date <- as.POSIXct(as.character(data_SINCA_Maipu$date), format = "%d/%m/%Y" )#"%y%m%d")


data_SINCA_Maipu <- data_SINCA_Maipu[year(data_SINCA_Maipu$date) == 2021,]
merge_Maipu_invierno <- merge(data_SINCA_Maipu, data_LCS_Maipu_invierno, by = "date", all.y = TRUE)
# Ajuste del modelo de regresión lineal
#modelo_Maipu_invierno <- lm(mean ~ valor_raster, data = data_Maipu_invierno)
# R2_Maipu_invierno <- summary(modelo_Maipu_invierno)$r.squared
# RMSE_Maipu_invierno <- sqrt(mean(residuals(modelo_Maipu_invierno)^2))
# Bias_Maipu_invierno <- mean(data_Maipu_invierno$mean - data_Maipu_invierno$valor_raster)
# n_Maipu_invierno <- nrow(data_Maipu_invierno)


# # Sinca mas cercano vs LCS
# modelo_Maipu_invierno <- lm(mean_LCS ~ Registros.completos, data = merge_Maipu_invierno)
# R2_Maipu_invierno <- summary(modelo_Maipu_invierno)$r.squared
# RMSE_Maipu_invierno <- sqrt(mean(residuals(modelo_Maipu_invierno)^2))
# Bias_Maipu_invierno <- mean(merge_Maipu_invierno$mean_LCS - merge_Maipu_invierno$Registros.completos)
# n_Maipu_invierno <- nrow(merge_Maipu_invierno)

# Sinca mas cercano vs Modelo(LCS)
modelo_Maipu_invierno <- lm(valor_raster_LCS ~ Registros.completos, data = merge_Maipu_invierno)
R2_Maipu_invierno <- summary(modelo_Maipu_invierno)$r.squared
RMSE_Maipu_invierno <- sqrt(mean(residuals(modelo_Maipu_invierno)^2))
Bias_Maipu_invierno <- mean(merge_Maipu_invierno$valor_raster_LCS - merge_Maipu_invierno$Registros.completos)
n_Maipu_invierno <- nrow(merge_Maipu_invierno)


# Crear el gráfico con ggplot2
#plot_Maipu_invierno <- ggplot(data_Maipu_invierno , aes(x = valor_raster, y = mean)) +
# Crear el gráfico con ggplot2
#plot_Maipu_invierno <- ggplot(merge_Maipu_invierno , aes(x = Registros.completos, y = mean_LCS)) +
  plot_Maipu_invierno <- ggplot(merge_Maipu_invierno , aes(x = Registros.completos, y = valor_raster_LCS)) +
  
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#+, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  #scale_y_continuous(limits = c(0, 50),breaks = seq(0, 50, by = 10)) +  # Ticks cada 10 en el eje Y
  #scale_x_continuous(limits = c(0, 50),breaks = seq(0, 50, by = 10)) +  # Ticks cada 10 en el eje Y
  
  labs(
    x = "SINCA",
    y = "Modelo (LCS)",
    title = "Maipu invierno",
    subtitle = paste(
      "R2 =", round(R2_Maipu_invierno , 3),
      "| RMSE =", round(RMSE_Maipu_invierno , 2),
      "| Bias =", round(Bias_Maipu_invierno , 2),
      "| n =", n_Maipu_invierno
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
plot_Maipu_invierno


#################################
##### Maipu verano
unique(data_LCS$archivo)
unique(data_SINCA$estacion_SINCA)
data_LCS_Maipu_verano <- data_LCS[data_LCS$archivo == "Maipu Verano",]
data_SINCA_Maipu <- data_SINCA[data_SINCA$estacion_SINCA == "PDH",]

data_LCS_Maipu_verano$date <- as.POSIXct(as.character(data_LCS_Maipu_verano$date), format = "%Y-%m-%d" )#"%y%m%d")
data_SINCA_Maipu$date <- as.POSIXct(as.character(data_SINCA_Maipu$date), format = "%d/%m/%Y" )#"%y%m%d")


data_SINCA_Maipu <- data_SINCA_Maipu[year(data_SINCA_Maipu$date) == 2021,]
merge_Maipu_verano <- merge(data_SINCA_Maipu, data_LCS_Maipu_verano, by = "date", all.y = TRUE)
# Ajuste del modelo de regresión lineal
#modelo_Maipu_verano <- lm(mean ~ valor_raster, data = data_Maipu_verano)
# R2_Maipu_verano <- summary(modelo_Maipu_verano)$r.squared
# RMSE_Maipu_verano <- sqrt(mean(residuals(modelo_Maipu_verano)^2))
# Bias_Maipu_verano <- mean(data_Maipu_verano$mean - data_Maipu_verano$valor_raster)
# n_Maipu_verano <- nrow(data_Maipu_verano)


# # Sinca mas cercano vs LCS
# modelo_Maipu_verano <- lm(mean_LCS ~ Registros.completos, data = merge_Maipu_verano)
# R2_Maipu_verano <- summary(modelo_Maipu_verano)$r.squared
# RMSE_Maipu_verano <- sqrt(mean(residuals(modelo_Maipu_verano)^2))
# Bias_Maipu_verano <- mean(merge_Maipu_verano$mean_LCS - merge_Maipu_verano$Registros.completos)
# n_Maipu_verano <- nrow(merge_Maipu_verano)


# Sinca mas cercano vs LCS
modelo_Maipu_verano <- lm(valor_raster_LCS ~ Registros.completos, data = merge_Maipu_verano)
R2_Maipu_verano <- summary(modelo_Maipu_verano)$r.squared
RMSE_Maipu_verano <- sqrt(mean(residuals(modelo_Maipu_verano)^2))
Bias_Maipu_verano <- mean(merge_Maipu_verano$valor_raster_LCS - merge_Maipu_verano$Registros.completos)
n_Maipu_verano <- nrow(merge_Maipu_verano)

# Crear el gráfico con ggplot2
#plot_Maipu_verano <- ggplot(data_Maipu_verano , aes(x = valor_raster, y = mean)) +
# Crear el gráfico con ggplot2
#plot_Maipu_verano <- ggplot(merge_Maipu_verano , aes(x = Registros.completos, y = mean_LCS)) +
  plot_Maipu_verano <- ggplot(merge_Maipu_verano , aes(x = Registros.completos, y = valor_raster_LCS)) +
  
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#+, linetype = "dashed") +  # Línea de regresión
  #scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  #scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_y_continuous(limits = c(0, 50),breaks = seq(0, 50, by = 10)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 50),breaks = seq(0, 50, by = 10)) +  # Ticks cada 10 en el eje Y
  
  labs(
    x = "SINCA",
    y = "Modelo (LCS)",
    title = "Maipu verano",
    subtitle = paste(
      "R2 =", round(R2_Maipu_verano , 3),
      "| RMSE =", round(RMSE_Maipu_verano , 2),
      "| Bias =", round(Bias_Maipu_verano , 2),
      "| n =", n_Maipu_verano
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
plot_Maipu_verano



#################################
##### florida verano
unique(data_LCS$archivo)
unique(data_SINCA$estacion_SINCA)
data_LCS_Florida_verano <- data_LCS[data_LCS$archivo == "Florida Verano",]
data_SINCA_Florida <- data_SINCA[data_SINCA$estacion_SINCA == "FLD",]

data_LCS_Florida_verano$date <- as.POSIXct(as.character(data_LCS_Florida_verano$date), format = "%Y-%m-%d" )#"%y%m%d")
data_SINCA_Florida$date <- as.POSIXct(as.character(data_SINCA_Florida$date), format = "%d/%m/%Y" )#"%y%m%d")


data_SINCA_Florida <- data_SINCA_Florida[year(data_SINCA_Florida$date) == 2021,]
merge_Florida_verano <- merge(data_SINCA_Florida, data_LCS_Florida_verano, by = "date", all.y = TRUE)
# Ajuste del modelo de regresión lineal
# modelo_Florida_verano <- lm(mean ~ valor_raster, data = data_Florida_verano)
# R2_Florida_verano <- summary(modelo_Florida_verano)$r.squared
# RMSE_Florida_verano <- sqrt(mean(residuals(modelo_Florida_verano)^2))
# Bias_Florida_verano <- mean(data_Florida_verano$mean - data_Florida_verano$valor_raster)
# n_Florida_verano <- nrow(data_Florida_verano)


# # Sinca mas cercano vs LCS
# modelo_Florida_verano <- lm(mean_LCS ~ Registros.completos, data = merge_Florida_verano)
# R2_Florida_verano <- summary(modelo_Florida_verano)$r.squared
# RMSE_Florida_verano <- sqrt(mean(residuals(modelo_Florida_verano)^2))
# Bias_Florida_verano <- mean(merge_Florida_verano$mean_LCS - merge_Florida_verano$Registros.completos)
# n_Florida_verano <- nrow(merge_Florida_verano)

# Sinca mas cercano vs Modelo LCS
modelo_Florida_verano <- lm(valor_raster_LCS ~ Registros.completos, data = merge_Florida_verano)
R2_Florida_verano <- summary(modelo_Florida_verano)$r.squared
RMSE_Florida_verano <- sqrt(mean(residuals(modelo_Florida_verano)^2))
Bias_Florida_verano <- mean(merge_Florida_verano$valor_raster_LCS - merge_Florida_verano$Registros.completos)
n_Florida_verano <- nrow(merge_Florida_verano)

# Crear el gráfico con ggplot2
#plot_Florida_verano <- ggplot(data_Florida_verano , aes(x = valor_raster, y = mean)) +
# Crear el gráfico con ggplot2
#plot_Florida_verano <- ggplot(merge_Florida_verano , aes(x = Registros.completos, y = mean_LCS)) +
 plot_Florida_verano <- ggplot(merge_Florida_verano , aes(x = Registros.completos, y = valor_raster_LCS)) +
  
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#+, linetype = "dashed") +  # Línea de regresión
  #scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  #scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_y_continuous(limits = c(0, 50),breaks = seq(0, 50, by = 10)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 50),breaks = seq(0, 50, by = 10)) +  # Ticks cada 10 en el eje Y
  
  labs(
    x = "SINCA",
    y = "Modelo (LCS)",
    title = "Florida verano",
    subtitle = paste(
      "R2 =", round(R2_Florida_verano , 3),
      "| RMSE =", round(RMSE_Florida_verano , 2),
      "| Bias =", round(Bias_Florida_verano , 2),
      "| n =", n_Florida_verano
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
plot_Florida_verano


#################################
##### Florida invierno
unique(data_LCS$archivo)
unique(data_SINCA$estacion_SINCA)
data_LCS_Florida_invierno <- data_LCS[data_LCS$archivo == "Florida Invierno",]
data_SINCA_Florida <- data_SINCA[data_SINCA$estacion_SINCA == "FLD",]

data_LCS_Florida_invierno$date <- as.POSIXct(as.character(data_LCS_Florida_invierno$date), format = "%Y-%m-%d" )#"%y%m%d")
data_SINCA_Florida$date <- as.POSIXct(as.character(data_SINCA_Florida$date), format = "%d/%m/%Y" )#"%y%m%d")


data_SINCA_Florida <- data_SINCA_Florida[year(data_SINCA_Florida$date) == 2021,]
merge_Florida_invierno <- merge(data_SINCA_Florida, data_LCS_Florida_invierno, by = "date", all.y = TRUE)
# Ajuste del modelo de regresión lineal
#modelo_Florida_invierno <- lm(mean ~ valor_raster, data = data_Florida_invierno)
# R2_Florida_invierno <- summary(modelo_Florida_invierno)$r.squared
# RMSE_Florida_invierno <- sqrt(mean(residuals(modelo_Florida_invierno)^2))
# Bias_Florida_invierno <- mean(data_Florida_invierno$mean - data_Florida_invierno$valor_raster)
# n_Florida_invierno <- nrow(data_Florida_invierno)


# # Sinca mas cercano vs LCS
# modelo_Florida_invierno <- lm(mean_LCS ~ Registros.completos, data = merge_Florida_invierno)
# R2_Florida_invierno <- summary(modelo_Florida_invierno)$r.squared
# RMSE_Florida_invierno <- sqrt(mean(residuals(modelo_Florida_invierno)^2))
# Bias_Florida_invierno <- mean(merge_Florida_invierno$mean_LCS - merge_Florida_invierno$Registros.completos)
# n_Florida_invierno <- nrow(merge_Florida_invierno)

# # Sinca mas cercano vs LCS
modelo_Florida_invierno <- lm(valor_raster_LCS ~ Registros.completos, data = merge_Florida_invierno)
R2_Florida_invierno <- summary(modelo_Florida_invierno)$r.squared
RMSE_Florida_invierno <- sqrt(mean(residuals(modelo_Florida_invierno)^2))
Bias_Florida_invierno <- mean(merge_Florida_invierno$valor_raster_LCS - merge_Florida_invierno$Registros.completos)
n_Florida_invierno <- nrow(merge_Florida_invierno)


# Crear el gráfico con ggplot2
#plot_Florida_invierno <- ggplot(data_Florida_invierno , aes(x = valor_raster, y = mean)) +
# Crear el gráfico con ggplot2
#plot_Florida_invierno <- ggplot(merge_Florida_invierno , aes(x = Registros.completos, y = mean_LCS)) +
  plot_Florida_invierno <- ggplot(merge_Florida_invierno , aes(x = Registros.completos, y = valor_raster_LCS)) +
  
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE)+#+, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  #scale_y_continuous(limits = c(0, 50),breaks = seq(0, 50, by = 10)) +  # Ticks cada 10 en el eje Y
  #scale_x_continuous(limits = c(0, 50),breaks = seq(0, 50, by = 10)) +  # Ticks cada 10 en el eje Y
  
  labs(
    x = "SINCA",
    y = "Modelo (LCS)",
    title = "Florida invierno",
    subtitle = paste(
      "R2 =", round(R2_Florida_invierno , 3),
      "| RMSE =", round(RMSE_Florida_invierno , 2),
      "| Bias =", round(Bias_Florida_invierno , 2),
      "| n =", n_Florida_invierno
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
plot_Florida_invierno




#################################

combined_plot_1 <- grid.arrange(plot_Maipu_verano, plot_Maipu_invierno,
                                plot_Florida_verano, plot_Florida_invierno,
                                nrow = 2, ncol = 2)

combined_plot_2 <- grid.arrange(plot_Vitacura_verano, plot_Vitacura_invierno,
                                plot_Qui_verano, plot_Qui_invierno,
                                nrow = 2, ncol = 2)
combined_plot_1 <- grid.arrange(plot_Maipu_verano, plot_Maipu_invierno,
                                nrow = 1, ncol = 2)
