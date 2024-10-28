# ---  OBJETIVO ---
#### Analisis de residuos de los distintos modelos generados

## -- Leemos dataset de entrenamiento y testeo
dir <- "D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/"
setwd(dir)
train_data <- read.csv(paste(dir,"Modelo 1/M1_train.csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo 1/M1_test.csv",sep=""))

## -- Cargamos el modelo de interes

# Guardamos modelo
modelDir <- "D:/Josefina/Proyectos/ProyectoChile/modelos/modelo/"
list.files(modelDir)
model <- load(paste(modelDir,"02-RLM_cv_M1-141024.RData",sep=""))
model
rf_cv_model$coefnames
# Predicciones en el conjunto de entrenamiento y testeo
y_train_pred <- predict(lm_cv_model, newdata = train_data)
y_test_pred <- predict(lm_cv_model, newdata = test_data)

########################################
# Para xgb Model
X_train <- train_data[ , c("AOD_055", "ndvi","LandCover", "BCSMASS_dia", "DUSMASS_dia", "DUSMASS25_dia",
                           "OCSMASS_dia", "SO2SMASS_dia", "SO4SMASS_dia", "SSSMASS_dia",
                           "SSSMASS25_dia", "blh_mean", "sp_mean", "d2m_mean",
                           "t2m_mean", "v10_mean", "u10_mean", "tp_mean", "DEM","dayWeek")]

X_test <- test_data[ , c("AOD_055", "ndvi","LandCover", "BCSMASS_dia", "DUSMASS_dia", "DUSMASS25_dia",
                         "OCSMASS_dia", "SO2SMASS_dia", "SO4SMASS_dia", "SSSMASS_dia",
                         "SSSMASS25_dia", "blh_mean", "sp_mean", "d2m_mean",
                         "t2m_mean", "v10_mean", "u10_mean", "tp_mean", "DEM","dayWeek")]


X_train <- train_data[ , c("AOD_055", "ndvi","LandCover", "BCSMASS", "DUSMASS", "DUSMASS25",
                           "OCSMASS", "SO2SMASS", "SO4SMASS", "SSSMASS",
                           "SSSMASS25", "blh_mean", "sp_mean", "d2m_mean",
                           "t2m_mean", "v10_mean", "u10_mean", "tp_mean", "DEM","dayWeek")]

X_test <- test_data[ , c("AOD_055", "ndvi","LandCover", "BCSMASS", "DUSMASS", "DUSMASS25",
                         "OCSMASS", "SO2SMASS", "SO4SMASS", "SSSMASS",
                         "SSSMASS25", "blh_mean", "sp_mean", "d2m_mean",
                         "t2m_mean", "v10_mean", "u10_mean", "tp_mean", "DEM","dayWeek")]
y_train <- train_data$PM25
y_test <- test_data$PM25
dtest <- as.matrix(X_test)
dtrain <- as.matrix(X_train)
X_train <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
X_test<- xgb.DMatrix(data = as.matrix(X_test), label = y_test)
y_train_pred <- predict(xgb_tuned, newdata = X_train)
y_test_pred <- predict(xgb_tuned, newdata = X_test)
##
y_train_pred <- predict(xgb_cv_model, newdata = X_train)
y_test_pred <- predict(xgb_cv_model, newdata = X_test)
# Calcular los residuos
residuals_train <- y_train - y_train_pred
residuals_test <- y_test - y_test_pred
# Crear un dataframe con los valores predichos y los residuos
data_test <- data.frame(predicted = y_test_pred,real=y_test, residuals = residuals_test)
data_train <- data.frame(predicted = y_train_pred, real=y_train,residuals = residuals_train)

################
# Calcular los residuos
residuals_train <- train_data$PM25 - y_train_pred
residuals_test <- test_data$PM25 - y_test_pred


# Crear un dataframe con los valores predichos y los residuos
data_test <- data.frame(predicted = y_test_pred,real=test_data$PM25, residuals = residuals_test)
data_train <- data.frame(predicted = y_train_pred, real=train_data$PM25,residuals = residuals_train)

###-- 
plot_residuos_test <- ggplot(data_test, aes(x = predicted, y = residuals)) +
  geom_point(color = "#2c7fb8",size=2,alpha=0.5) + # Puntos en azul
  geom_hline(yintercept = 0, color = "#de2d26", linetype = "dashed", size = 0.7) + # Línea horizontal
  ggtitle("Test") +
  xlab("Valores predichos (Test)") +
  ylab("Residuos") +
 
  #scale_x_continuous(limits = c(0, 145),breaks = seq(0, 140, by = 20)) +  # Ticks cada 10 en el eje X
  scale_x_continuous(limits = c(0, 165),breaks = seq(0, 160, by = 20)) +  # Ticks cada 10 en el eje X
  
  scale_y_continuous(limits = c(-60, 100),breaks = seq(-60, 110, by = 40)) +  # Ticks cada 10 en el eje Y
  theme_classic()+
  theme(
    plot.title = element_text(size = 10),  # Tamaño del título
    axis.title.x = element_text(size = 8),  # Tamaño etiqueta eje X
    axis.title.y = element_text(size = 8)   # Tamaño etiqueta eje Y
  )

plot_residuos_train <- ggplot(data_train, aes(x = predicted, y = residuals)) +
  geom_point(color = "#31a354",size=2,alpha=0.5) + # Puntos en azul
  geom_hline(yintercept = 0, color = "#de2d26", linetype = "dashed", size = 0.7) + # Línea horizontal
  ggtitle("Train") +
  xlab("Valores predichos (Train)") +
  # scale_x_continuous(limits = c(0, 145),breaks = seq(0, 140, by = 20)) +  # Ticks cada 10 en el eje X
  scale_x_continuous(limits = c(0, 165),breaks = seq(0, 160, by = 20)) +  # Ticks cada 10 en el eje X
  scale_y_continuous(limits = c(-60, 100),breaks = seq(-60, 110, by = 40)) +  # Ticks cada 10 en el eje Y
  
  ylab("Residuos") +
  theme_classic()+
  theme(
    plot.title = element_text(size = 10),  # Tamaño del título
    axis.title.x = element_text(size = 8),  # Tamaño etiqueta eje X
    axis.title.y = element_text(size = 8)   # Tamaño etiqueta eje Y
  )
plot_residuos_test
plot_residuos_train


# Combinar los gráficos en una fila y dos columnas
combinacion_residuos<- grid.arrange(plot_residuos_train, plot_residuos_test, 
                                    nrow = 1, ncol = 2, top = "02-RLM_cv_M1-141024")



########################################################
##### ---- Gráfico de dispersión de Predicciones vs Valores Reales
#muestra cómo se alinean las predicciones del modelo SVR con los valores
#reales. La línea y = x sirve como referencia para una predicción perfecta.

###-- 

plot_y_test <- ggplot(data_test, aes(x = real, y = predicted)) +
  geom_point(color = "#2c7fb8",size=2,alpha=0.5) + # Puntos en azul
  geom_abline(intercept = 0, slope=1,color = "#de2d26", linetype = "dashed", size = 0.7) + # Línea horizontal
  ggtitle("Test") +
  xlab("Observed") +
  ylab("Predicted") +
  
  # scale_x_continuous(limits = c(0, 180),breaks = seq(0, 185, by = 30)) +  # Ticks cada 10 en el eje X
  # scale_y_continuous(limits = c(0,180),breaks = seq(0, 185, by = 30)) +    # Ticks cada 10 en el eje Y
  theme_classic()+
  theme(
    plot.title = element_text(size = 10),  # Tamaño del título
    axis.title.x = element_text(size = 8),  # Tamaño etiqueta eje X
    axis.title.y = element_text(size = 8)   # Tamaño etiqueta eje Y
  )

plot_y_train <- ggplot(data_train, aes(y = predicted, x = real)) +
  geom_point(color = "#31a354",size=2,alpha=0.5) + # Puntos en azul
  geom_abline(intercept = 0, slope=1, color = "#de2d26", linetype = "dashed", size = 0.7) + # Línea horizontal
  ggtitle("Train") +

  # scale_x_continuous(limits = c(0, 180),breaks = seq(0, 185, by = 30)) +  # Ticks cada 10 en el eje X
  # scale_y_continuous(limits = c(0,180),breaks = seq(0, 185, by = 30)) +   # Ticks cada 10 en el eje Y
  
  xlab("Observed") +
  ylab("Predicted") +
  theme_classic()+
  theme(
    plot.title = element_text(size = 10),  # Tamaño del título
    axis.title.x = element_text(size = 8),  # Tamaño etiqueta eje X
    axis.title.y = element_text(size = 8)   # Tamaño etiqueta eje Y
  )
plot_y_test
plot_y_train


# Combinar los gráficos en una fila y dos columnas
combinacion_valores<- grid.arrange(plot_y_train, plot_y_test, 
                                    nrow = 1, ncol = 2, top = "02-RLM_cv_M1-141024")



##### ---- Histograma de los residuos
#Muestra si están distribuidos normalmente, que es uno de los supuestos básicos
#para muchos modelos de regresión
# Supongamos que tienes un dataframe llamado data_train con los residuos calculados
# Asegúrate de que los residuos están en la columna 'residuals' de tu dataframe

# Crea el histograma de los residuos
hist_residuos_train <- ggplot(data_train, aes(x = residuals)) +
  geom_histogram( fill = "#31a354", color = "black", alpha = 0.7) +  # Histogramas verdes con borde negro
  ggtitle("Train") +
  xlab("Residuos") +
  ylab("Frecuencia") +
  #scale_x_continuous(breaks = seq(min(data_train$residuals), max(data_train$residuals), by = 10)) +  # Ticks en el eje X
  #scale_y_continuous(breaks = seq(0, max(table(data_train$residuals)), by = 5)) +  # Ticks en el eje Y
  theme_classic() +
  theme(
    plot.title = element_text(size = 10),  # Tamaño del título
    axis.title.x = element_text(size = 8),  # Tamaño etiqueta eje X
    axis.title.y = element_text(size = 8)   # Tamaño etiqueta eje Y
  )

hist_residuos_test <- ggplot(data_test, aes(x = residuals)) +
  geom_histogram( fill =  "#2c7fb8", color = "black", alpha = 0.7) +  # Histogramas verdes con borde negro
  ggtitle("Train") +
  xlab("Residuos") +
  ylab("Frecuencia") +
  #scale_x_continuous(breaks = seq(min(data_train$residuals), max(data_train$residuals), by = 10)) +  # Ticks en el eje X
  #scale_y_continuous(breaks = seq(0, max(table(data_train$residuals)), by = 5)) +  # Ticks en el eje Y
  theme_classic() +
  theme(
    plot.title = element_text(size = 10),  # Tamaño del título
    axis.title.x = element_text(size = 8),  # Tamaño etiqueta eje X
    axis.title.y = element_text(size = 8)   # Tamaño etiqueta eje Y
  )

##### ---- Q-Q plot de los residuos
#compara la distribución de los residuos con una distribución 
#normal. Idealmente, los puntos deberían seguir una línea recta.


# Supongamos que tienes un vector llamado residuals_test
# Crea un Q-Q plot de los residuos
plot_qq <- ggqqplot(residuals_test, 
                    ggtheme = theme_minimal()) +  # Aplica un tema minimalista
  ggtitle("Q-Q Plot de Residuos (Test)") +
  xlab("Teóricos quantiles") +
  ylab("Quantiles de Residuos") +
  theme(
    plot.title = element_text(size = 10),  # Tamaño del título
    axis.title.x = element_text(size = 8),  # Tamaño etiqueta eje X
    axis.title.y = element_text(size = 8)   # Tamaño etiqueta eje Y
  )

# Añadir la línea de referencia
plot_qq <- plot_qq + geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed")

# Mostrar el Q-Q plot
print(plot_qq)

