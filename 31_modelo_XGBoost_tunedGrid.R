

## TUNED GRID XGboost


rm(list=ls())
estacion <- "BA"
modelo <- "1"
#Data modelo 1
test_data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))
train_data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
# Preparar los datos
X <- train_data[, c("AOD_055", "ndvi",  "BCSMASS_dia", "DUSMASS_dia", "DUSMASS25_dia",
                    "OCSMASS_dia", "SO2SMASS_dia", "SO4SMASS_dia", "SSSMASS_dia",
                    "SSSMASS25_dia", "blh_mean", "sp_mean", "d2m_mean",
                    "t2m_mean", "v10_mean", "u10_mean", "tp_mean", "DEM", "dayWeek")]#"LandCover",


y <- train_data$PM25

# Convertir a un formato adecuado para caret
dtrain <- xgb.DMatrix(data = as.matrix(X), label = y)

# Definir el control de entrenamiento para la búsqueda en cuadrícula
control <- trainControl(
  method = "cv",              # Método de validación cruzada
  number = 10,                # Número de pliegues
  verboseIter = TRUE          # Mostrar el progreso del entrenamiento
)

# Definir el grid de hiperparámetros
grid <- expand.grid(
  nrounds = 2000,#c(100, 200, 300),              # Número de rondas
  eta = c(0.01, 0.1, 0.3),                 # Tasa de aprendizaje
  max_depth = c(3, 6, 9),                  # Profundidad máxima de los árboles
  gamma = 0,#c(0, 0.1, 0.3),                  # Regularización
  colsample_bytree = 1,#c(0.7, 0.8, 1),       # Proporción de características
  subsample = 0.8,#c(0.7, 0.8, 1),              # Proporción de datos
  min_child_weight = 1#1c(1, 3, 5)            # Peso mínimo de los hijos
)




# Ajustar el modelo usando la búsqueda en cuadrícula
xgb_tuned <- train(
  x = X,
  y = y,
  method = "xgbTree",                  # Método especificado como xgboost
  trControl = control,                 # Control de entrenamiento
  tuneGrid = grid,                     # Grid de hiperparámetros
  metric = "RMSE",                     # Métrica de evaluación
  maximize = FALSE                     # Minimizar RMSE
)

07:54
# Ver los mejores hiperparámetros encontrados
print(xgb_tuned$bestTune)

# Ver resumen del modelo ajustado
print(xgb_tuned)

############################################################
#


# Preparar datos de prueba
X_test <- test_data[ , c("AOD_055", "ndvi","LandCover", "BCSMASS", "DUSMASS", "DUSMASS25",
                         "OCSMASS", "SO2SMASS", "SO4SMASS", "SSSMASS",
                         "SSSMASS25", "blh_mean", "sp_mean", "d2m_mean",
                         "t2m_mean", "v10_mean", "u10_mean", "tp_mean", "DEM","dayWeek")]
X_test <- test_data[ , c("AOD_055", "ndvi", "BCSMASS_dia", "DUSMASS_dia", "DUSMASS25_dia",
                         "OCSMASS_dia", "SO2SMASS_dia", "SO4SMASS_dia", "SSSMASS_dia",
                         "SSSMASS25_dia", "blh_mean", "sp_mean", "d2m_mean",
                         "t2m_mean", "v10_mean", "u10_mean", "tp_mean", "DEM","dayWeek")] #"LandCover",

y_test <- test_data$PM25
dtest <- as.matrix(X_test)
dtrain <- as.matrix(X)
#dtest <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)

# Realizar predicciones sobre el conjunto de prueba
# predictions <- predict(xgb_tuned, dtest)
predictions <- predict(xgb_tuned, dtest)
# Evaluar el modelo con el conjunto de prueba
actuals <- y_test
predicted <- predictions

# Calcular métricas de rendimiento
mse <- mean((actuals - predicted)^2)
rmse <- sqrt(mse)
mae <- mean(abs(actuals - predicted))
r2 <- cor(predicted, actuals)^2
r <- cor(actuals, predicted, method = "pearson")
mape <- mean(abs((actuals - predicted) / actuals)) * 100
medae <- median(abs(actuals - predicted))
# Imprimir métricas
cat("R2: ", round(r2,2), "\n")
cat("R: ", round(r,2), "\n")
cat("RMSE: ", round(rmse,2), "\n")
cat("MAE: ", round(mae,2), "\n")
cat("MAPE: ", round(mape,2), "%\n")
cat("MSE: ", round(mse,2), "\n")
cat("MedAE: ", round(medae,2), "\n")
min(predicted)
max(predicted)
# Realizar predicciones sobre el conjunto de entrenamiento
train_predictions <- predict(xgb_tuned, dtrain)
min(train_predictions)
max(train_predictions)
# Evaluar el modelo en el conjunto de entrenamiento
train_actuals <- y
train_predicted <- train_predictions
train_mse <- mean((train_actuals - train_predicted)^2)
train_rmse <- sqrt(train_mse)
train_mae <- mean(abs(train_actuals - train_predicted))
train_r2 <- cor(train_predicted, train_actuals)^2
train_r <- cor(train_actuals, train_predicted, method = "pearson")
train_mape <- mean(abs((train_actuals - train_predicted) / train_actuals)) * 100
train_medae <- median(abs(train_actuals - train_predicted))
# Imprimir métricas para el conjunto de entrenamiento
cat("Training R2: ", round(train_r2,3), "\n")
cat("Training R: ", round(train_r,3), "\n")
cat("Training RMSE: ", round(train_rmse,3), "\n")
cat("Training MAE: ", round(train_mae,3), "\n")
cat("Training MAPE: ", round(train_mape,2), "%\n")
cat("Training MSE: ", round(train_mse,3), "\n")
cat("Training MedAE: ", round(train_medae,2), "\n")
min(train_predictions)
max(train_predictions)
# gUARDAMOS MODELO
# Sin cv
setwd("D:/Josefina/Proyectos/ProyectoChile/SP/modelos/modelo")

save(xgb_tuned, file="03-XGB_cv_M1-131124_SP.RData")
