# # Leer los datos
# data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/merge_tot/08_TOT_merge_tot.csv")
# 
# # Filtrar datos completos (sin valores NA)
# data_completo <- data[complete.cases(data),]
# 
# # Agregar columnas de día del año y mes
# # data_completo$diaYear <- as.numeric(format(data_completo$date, "%j"))
# # data_completo$month <- as.numeric(format(data_completo$date, "%m"))
# 
# # Dividir el dataframe en 70% entrenamiento y 30% testeo
# set.seed(123)  # Asegurar la reproducibilidad
# train_index <- createDataPartition(data_completo$PM25, p = 0.7, list = FALSE)
# train_data <- data_completo[train_index, ]
# test_data <- data_completo[-train_index, ]
estacion<- "MD"
modelo <- 3
dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/",sep="")
setwd(dir)
train_data <- read.csv(paste(dir,"Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo_1/M1_test_",estacion,".csv",sep=""))

# Entrenar un modelo de regresión lineal simple con PM2.5 como dependiente y AOD_055 como independiente
#lm_model <- lm(PM25 ~ AOD_055, data = train_data)

# Resumen del modelo
#summary(lm_model)

# Definir el control de entrenamiento con validación cruzada de 10 pliegues
train_control <- trainControl(method = "cv", number = 10)

# Entrenar el modelo de regresión lineal con validación cruzada
lm_cv_model <- train(PM25 ~ AOD_055, data = train_data, 
                    method = "lm", trControl = train_control)

# Mostrar los resultados de la validación cruzada
print(lm_cv_model)
print(lm_cv_model$results)



predictions <- predict(lm_cv_model, newdata = test_data)
predictions_train <- predict(lm_cv_model, newdata = train_data)
predicciones_hora<- predictions

#Calcular el coeficiente de determinación (R²)
lm_r_squared_hora <- cor(predicciones_hora, test_data$PM25)^2
lm_r_squared_train <- cor(predictions_train, train_data$PM25)^2


# Calcular el coeficiente de Pearson
pearson_cor_hora <- cor(test_data$PM25, predicciones_hora, method = "pearson")
pearson_train <- cor(train_data$PM25, predictions_train, method = "pearson")

# Calcular el error cuadrático medio (RMSE)
lm_rmse_hora <- sqrt(mean((predicciones_hora - test_data$PM25)^2))
lm_rmse_train <- sqrt(mean((predictions_train - train_data$PM25)^2))

# 3. Calcular el MAE (Mean Absolute Error)
mae_hora <- mean(abs(test_data$PM25 - predicciones_hora))
mae_train <- mean(abs(train_data$PM25 - predictions_train))

# 4. Calcular el MAPE (Mean Absolute Percentage Error)
mape_hora <- mean(abs((test_data$PM25 - predicciones_hora) / test_data$PM25)) * 100
mape_train <- mean(abs((train_data$PM25 - predictions_train) / train_data$PM25)) * 100

# 5. Calcular el MSE (Mean Squared Error)
mse_hora <- mean((test_data$PM25 - predicciones_hora)^2)
mse_train <- mean((train_data$PM25 - predictions_train)^2)

# 6. Calcular el MedAE (Median Absolute Error)
medae_hora <- median(abs(test_data$PM25 - predicciones_hora))
medae_train <- median(abs(train_data$PM25 - predictions_train))

### Test
print(paste("R2 test:", round(lm_r_squared_hora, 5)))
print(paste("R pearson test:", round(pearson_cor_hora, 2)))
print(paste("RMSE test:", round(lm_rmse_hora, 2)))
print(paste("mae test:", round(mae_hora, 2)))
print(paste("MAPE test:", round(mape_hora, 2), "%"))
print(paste("MSE test:", round(mse_hora, 2)))
print(paste("MedAE test:", round(medae_hora,2)))
min(predictions)
max(predictions)
## Train
print(paste("R2 train:", round(lm_r_squared_train, 5)))
print(paste("R pearson train:", round(pearson_train, 2)))
print(paste("RMSE train:", round(lm_rmse_train, 2)))
print(paste("mae train:", round(mae_train, 2)))
print(paste("MAPE train:", round(mape_train, 2), "%"))
print(paste("MSE train:", round(mse_train, 2)))
print(paste("MedAE train:", round(medae_train,2)))
min(predictions_train)
max(predictions_train)

setwd(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/modelo",sep=""))
getwd()
save(lm_cv_model, file=paste("02-RLS-M",modelo,"_090125",estacion,".RData",sep=""))






##############################################################
############################################################
# PLOTS
df_predict <- data.frame(predicciones = lm_predictions_train,observados=train_data$PM25)
# Plot predictions vs test data
# Plot predictions vs test data
ggplot(df_predict,aes(predicciones, observados)) +
  geom_point(color = "darkred", alpha = 0.5) + 
  geom_smooth(method=lm)+ ggtitle('Linear Regression ') +
  ggtitle("Linear Regression: Prediction vs Test Data") +
  xlab("Predecited") +
  ylab("Observed") +
  theme(plot.title = element_text(color="darkgreen",size = 18,hjust = 0.5),
        axis.text.y = element_text(size = 12),
        
        axis.text.x = element_text(size = 12,hjust=.5),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))



plot <- ggplot(df_predict, aes(x = observados , y = predicciones)) +
  geom_point(color = "#99d8c9", alpha = 0.8, size = 2) +  # Puntos del scatter plot
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Línea de regresión lineal
  labs(x = "Observado", y = "Predicción", title = "Prediccion PM2.5 = f(AOD)") +
  theme_classic() +
  
  scale_x_continuous(limits = c(0, 160), breaks = seq(0, 160, by = 40)) +  # Ticks en el eje X
  scale_y_continuous(limits = c(0, 160), breaks = seq(0, 160, by = 40)) +  # Ticks en el eje Y
  
  # Añadir texto para R^2
  geom_text(aes(x = 15, y = 120),
            label = expression(R^2 == 0.025),  # Coloca aquí el valor real de R^2
            size = 4, color = "Black") +
  
  # Añadir texto para coeficiente de correlación Pearson (r)
  geom_text(aes(x = 14, y = 110), 
            label = paste("r = ", round(pearson, 2)), 
            size = 4, color = "Black") +
  
  # Añadir texto para RMSE
  geom_text(aes(x = 18.1, y = 100), 
            label = paste("RMSE = ", round(rmse, 2)), 
            size = 4, color = "Black") +
  
  # Añadir texto para Bias
  geom_text(aes(x = 15.8, y = 90), 
            label = paste("Bias = ", round(bias, 2)), 
            size = 4, color = "Black") +
  
  # Aumentar el tamaño de los ticks en los ejes X e Y
  theme(
    axis.text.x = element_text(size = 12),  # Tamaño de los ticks en el eje X
    axis.text.y = element_text(size = 12)   # Tamaño de los ticks en el eje Y
  )

ggsave("D:/Josefina/Proyectos/ProyectoChile/plots/SeriesTemporales/Salida_regresionLineal.png", plot = plot, width = 10, height = 6, dpi = 300)

################################################################
###################################################################
#Algunos plots
# Gráfico de dispersión de Predicciones vs Valores Reales
#Este gráfico muestra cómo se alinean las predicciones del modelo 
#con los valores reales. Idealmente, los puntos deberían estar cerca de la línea y = x
plot(test_data$PM25, lm_predictions, 
     xlab = "Valores Reales de PM2.5", 
     ylab = "Predicciones de PM2.5", 
     main = "Predicciones vs Valores Reales")
abline(0, 1, col = "red")  # Añadir una línea y = x


# Calcular los residuos
residuos <- test_data$PM25 - lm_predictions

#### --- Gráfico de residuos vs. predicciones

#Este gráfico ayuda a verificar si los residuos (errores) están 
#distribuidos aleatoriamente. Un patrón en este gráfico podría 
#indicar que el modelo no está capturando alguna relación en los
#datos.




plot(lm_predictions, residuos, 
     xlab = "Predicciones de PM2.5", 
     ylab = "Residuos", 
     main = "Residuos vs Predicciones")
abline(h = 0, col = "red")  # Añadir una línea horizontal en y = 0


#### --- Histograma de los residuos
#Un histograma de los residuos te permite ver si están distribuidos
# normalmente, lo cual es un supuesto clave en la regresión lineal.
hist(residuos, 
     breaks = 20, 
     xlab = "Residuos", 
     main = "Histograma de los Residuos")

#### --- Q-Q plot de los residuos
# compara la distribución de los residuos con una distribución normal.
# Si los puntos siguen aproximadamente la línea recta, los residuos
#están normalmente distribuidos.
qqnorm(residuos)
qqline(residuos, col = "red")

#### --- Gráfico de Cook's Distance
#Ayuda a identificar observaciones que tienen una gran influencia 
#en los coeficientes de la regresión
plot(cooks.distance(lm_model), 
     type = "h", 
     ylab = "Cook's Distance", 
     main = "Gráfico de Cook's Distance")
abline(h = 4/(nrow(train_data)-length(lm_model$coefficients)), col = "red")

# Crear el gráfico de leverage vs. standardized residuals
plot(lm_model, which = 5)
###########################################################

setwd("D:/Josefina/Proyectos/ProyectoChile/modelos/modelo")
lm_cv_model_simple <- lm_model
save(lm_cv_model_simple, file="01-LRS-M4_050924.RData")

################################################################################
################################################################################
# cargar el modelo y aplicalo a otro set de datos

# Paso 1: Cargar el modelo
load("01-LRS-M4_050924.RData")

dir_tiff <- "D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/tiff/"
# Paso 2: Preparar el nuevo conjunto de datos
# Supongamos que tienes un nuevo data.frame llamado 'new_data'
maiac <- raster(paste(dir_tiff,"MAIAC_raster.tif",sep=""))
r_stack_df <- as.data.frame(maiac, na.rm = TRUE)
names(r_stack_df) <- c("AOD_055")

# Aplicar el modelo de Random Forest al data frame
predictions <- predict(lm_cv_model_simple, newdata = r_stack_df)

# Crear un raster vac?o con la misma extensi?n y resoluci?n que el stack
pred_raster <- raster(maiac)

# Asignar las predicciones al raster
pred_raster[] <- NA  # Inicia con valores NA

# Reinsertar las predicciones en las celdas correspondientes
pred_raster[!is.na(values(maiac[[1]]))] <- predictions

getwd()

writeRaster(pred_raster, filename = "D:/Josefina/Proyectos/ProyectoChile/modelos/SalidaModelo/01-LRS-M4_050924.tif", format = "GTiff", overwrite = TRUE)

#############################################################
###########################################################
#Evaluar las estaciones en el raster vs los valores reales
#lo hice manual en el qgis

