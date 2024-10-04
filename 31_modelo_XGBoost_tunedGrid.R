

## TUNED GRID XGboost


#Data modelo 1
test_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 1/M1_test.csv")
train_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 1/M1_train.csv")
# Preparar los datos
X <- train_data[, c("AOD_055", "ndvi", "LandCover", "BCSMASS_dia", "DUSMASS_dia", "DUSMASS25_dia",
                    "OCSMASS_dia", "SO2SMASS_dia", "SO4SMASS_dia", "SSSMASS_dia",
                    "SSSMASS25_dia", "blh_mean", "sp_mean", "d2m_mean",
                    "t2m_mean", "v10_mean", "u10_mean", "tp_mean", "DEM", "dayWeek")]

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
  nrounds = c(100, 200, 300),              # Número de rondas
  eta = c(0.01, 0.1, 0.3),                 # Tasa de aprendizaje
  max_depth = c(3, 6, 9),                  # Profundidad máxima de los árboles
  gamma = c(0, 0.1, 0.3),                  # Regularización
  colsample_bytree = c(0.7, 0.8, 1),       # Proporción de características
  subsample = c(0.7, 0.8, 1),              # Proporción de datos
  min_child_weight = c(1, 3, 5)            # Peso mínimo de los hijos
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

09:07
# Ver los mejores hiperparámetros encontrados
print(xgb_tuned$bestTune)

# Ver resumen del modelo ajustado
print(xgb_tuned)
