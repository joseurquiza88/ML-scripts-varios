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

dir <- "D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/"
setwd(dir)
train_data <- read.csv(paste(dir,"Modelo 5/M5_train.csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo 5/M5_test.csv",sep=""))

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

# Predecir en el conjunto de testeo
#lm_predictions <- predict(lm_model, newdata = test_data)
lm_predictions_cv <- predict(lm_cv_model, newdata = test_data)

# Calcular el error cuadrático medio (RMSE)
#lm_rmse <- sqrt(mean((lm_predictions - test_data$PM25)^2))
lm_rmse_cv <- sqrt(mean((lm_predictions_cv - test_data$PM25)^2))

# Calcular el coeficiente de determinación (R²)
#lm_r_squared <- cor(lm_predictions, test_data$PM25)^2
lm_r_squared_cv <- cor(lm_predictions_cv, test_data$PM25)^2
# 3. Calcular el MAE (Mean Absolute Error)
#mae <- mean(abs(test_data$PM25 - lm_predictions))
mae_cv <- mean(abs(test_data$PM25 - lm_predictions_cv))
#print(paste("MAE:", round(mae, 3)))


# 4. Calcular el MAPE (Mean Absolute Percentage Error)
#mape <- mean(abs((test_data$PM25 - lm_predictions) / test_data$PM25)) * 100
#print(paste("MAPE:", round(mape, 2), "%"))

mape_cv <- mean(abs((test_data$PM25 - lm_predictions_cv) / test_data$PM25)) * 100


# 5. Calcular el MSE (Mean Squared Error)
#mse <- mean((test_data$PM25 - lm_predictions)^2)
#print(paste("MSE:", round(mse, 3)))

mse_cv <- mean((test_data$PM25 - lm_predictions_cv)^2)


# 6. Calcular el MedAE (Median Absolute Error)
# medae <- median(abs(test_data$PM25 - lm_predictions))
# print(paste("MedAE cv:", round(medae, 3)))

medae_cv <- median(abs(test_data$PM25 - lm_predictions_cv))
# Calcular el coeficiente de Pearson
pearson_cor <- cor(test_data$PM25, lm_predictions, method = "pearson")
# print(paste("R pearson:", round(pearson_cor, 3)))
pearson_cor_cv <- cor(test_data$PM25, lm_predictions_cv, method = "pearson")


spearman_cor <- cor(test_data$PM25, lm_predictions, method = "spearman")
# print(paste("R spearman:", round(pearson_cor, 3)))
# Mostrar las métricas de evaluación
# cat("RMSE:", lm_rmse, "\n")
# cat("R²:", lm_r_squared, "\n")


cat("R²:", lm_r_squared_cv, "\n")
print(paste("R pearson cv:", round(pearson_cor_cv, 3)))
cat("RMSE:", lm_rmse_cv, "\n")
print(paste("MAE cv:", round(mae_cv, 3)))
print(paste("MAPE cv:", round(mape_cv, 2), "%"))
print(paste("MSE:", round(mse_cv, 3)))
print(paste("MedAE cv:", round(medae_cv, 3)))
min(lm_predictions_cv)
max(lm_predictions_cv)


###############Metricas del train

# Predecir en el conjunto de testeo
lm_predictions_train <- predict(lm_cv_model, newdata = train_data)

lm_rmse_cv_train <- sqrt(mean((lm_predictions_train - train_data$PM25)^2))

lm_r_squared_cv_train <- cor(lm_predictions_train, train_data$PM25)^2
# 3. Calcular el MAE (Mean Absolute Error)
mae_train <- mean(abs(train_data$PM25 - lm_predictions_train))
# 4. Calcular el MAPE (Mean Absolute Percentage Error)
mape_train <- mean(abs((train_data$PM25 - lm_predictions_train) / train_data$PM25)) * 100
# 5. Calcular el MSE (Mean Squared Error)
mse_train <- mean((train_data$PM25 - lm_predictions_train)^2)
medae_train <- median(abs(train_data$PM25 - lm_predictions_train))
# Calcular el coeficiente de Pearson
pearson_cor_train <- cor(train_data$PM25, lm_predictions_train, method = "pearson")


cat("R²:", lm_r_squared_cv_train, "\n")
print(paste("R pearson train:", round(pearson_cor_train, 3)))
cat("RMSE train:", lm_rmse_cv_train, "\n")
print(paste("MAE train:", round(mae_train, 3)))
print(paste("MAPE train:", round(mape_train, 2), "%"))
print(paste("MSE train:", round(mse_train, 3)))
print(paste("MedAE train:", round(medae_train, 3)))
min(lm_predictions_train)
max(lm_predictions_train)


setwd("D:/Josefina/Proyectos/ProyectoChile/modelos/modelo")
save(lm_cv_model, file="01-RLS-M5_250924.RData")






##############################################################
############################################################
# PLOTS
# Plot predictions vs test data
# Plot predictions vs test data
ggplot(my_data,aes(predicted, observed)) +
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

