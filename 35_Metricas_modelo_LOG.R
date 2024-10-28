
#Data modelo 1
test_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 4/M4_test.csv")
train_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 4/M4_train.csv")
# cargar el modelo y aplicalo a otro set de datos
setwd("D:/Josefina/Proyectos/ProyectoChile/modelos/modelo")

# Paso 1: Cargar el modelo
load("07-RF_cv_M4-241024.RData")

#########################
#########################
#########################

# Paso 1: Realizar predicciones en la escala de log(PM2.5) para los conjuntos de entrenamiento y prueba
train_preds_log <- predict(rf_cv_model, newdata = train_data)
test_preds_log <- predict(rf_cv_model, newdata = test_data)

# Paso 2: Transformar las predicciones y valores reales a la escala original de PM2.5
train_preds <- exp(train_preds_log)
test_preds <- exp(test_preds_log)
train_actuals <- train_data$PM25  # Valores reales de PM2.5 en el conjunto de entrenamiento
test_actuals <- test_data$PM25    # Valores reales de PM2.5 en el conjunto de prueba

# Paso 3: Calcular las métricas de desempeño (MAE, RMSE, R^2) en la escala original



##################################################################
####################################################################
# Calcular el coeficiente de determinación (R²)
train_r2 <- cor(train_actuals, train_preds)^2
# train_actuals == test_data$PM25
# prediccion
# train_preds == train_preds_log <- predict(rf_cv_model, newdata = train_data)
predicciones_hora <- test_preds  
predictions_train <-  train_preds 
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
print(paste("R2 test:", round(lm_r_squared_hora, 2)))
print(paste("R pearson test:", round(pearson_cor_hora, 2)))
print(paste("RMSE test:", round(lm_rmse_hora, 2)))
print(paste("mae test:", round(mae_hora, 2)))
print(paste("MAPE test:", round(mape_hora, 2), "%"))
print(paste("MSE test:", round(mse_hora, 2)))
print(paste("MedAE test:", round(medae_hora,2)))
min(predicciones_hora)
max(predicciones_hora)
## Train
print(paste("R2 train:", round(lm_r_squared_train, 2)))
print(paste("R pearson train:", round(pearson_train, 2)))
print(paste("RMSE train:", round(lm_rmse_train, 2)))
print(paste("mae train:", round(mae_train, 2)))
print(paste("MAPE train:", round(mape_train, 2), "%"))
print(paste("MSE train:", round(mse_train, 2)))
print(paste("MedAE train:", round(medae_train,2)))
min(predictions_train)
max(predictions_train)
