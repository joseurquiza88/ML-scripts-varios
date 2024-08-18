# Cargar los paquetes necesarios
library(randomForest)
library(raster)

# Generar datos aleatorios para las variables predictoras
set.seed(42)
df <- read.csv("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/merge_superf_sat/02_data_proceed_OHG.csv")

# n <- nrow(data)
# # Definir la proporción de datos para entrenamiento
# train_proportion <- 0.7
# train_indices <- sample(1:n, size = round(train_proportion * n))
# # Crear conjuntos de entrenamiento y testeo
# train_data <- data[train_indices, ]
# test_data <- data[-train_indices, ]

# Dividir el dataframe en 70% entrenamiento y 30% testeo
train_index <- createDataPartition(df$PM25, p = 0.7, list = FALSE)
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

# Entrenar el modelo de Random Forest
rf_model <- randomForest(PM25 ~ AOD_550 + blh_mean + sp_mean + d2m_mean + t2m_mean + v10_mean + u10_mean + tp_mean, data = train_data, importance = TRUE)

# Mostrar la importancia de las variables
importance(rf_model)


# Predecir en el conjunto de testeo
predictions <- predict(rf_model, newdata = test_data)

# Calcular el error cuadrático medio (RMSE)
rmse <- sqrt(mean((predictions - test_data$PM25)^2))

# Calcular el coeficiente de determinación (R²)
r_squared <- cor(predictions, test_data$PM25)^2

# Mostrar las métricas de evaluación
cat("RMSE:", rmse, "\n")
cat("R²:", r_squared, "\n")

# Definir el control de entrenamiento con validación cruzada de 10 pliegues
train_control <- trainControl(method = "cv", number = 10)

# Entrenar el modelo con validación cruzada
rf_cv_model <- train(PM25 ~ AOD_550 + blh_mean + sp_mean + d2m_mean + t2m_mean + v10_mean + u10_mean + tp_mean, data = train_data, 
                     method = "rf", trControl = train_control)

# Mostrar los resultados de la validación cruzada
print(rf_cv_model)
# mtry es el número de variables candidatas consideradas para dividir en cada nodo del árbol de decisión en Random Forest.
# Se probaron tres configuraciones diferentes para mtry: 2, 5, y 8.
# Mostrar las métricas de evaluación de la validación cruzada

# El valor final elegido para mtry fue 2, ya que produjo el menor RMSE.

# RMSESD: La Desviación Estándar del RMSE. Mide la variabilidad del RMSE a través
# de las diferentes particiones de la validación cruzada.
# 
# Valores: 0.9658831 (mtry = 2), 0.9468086 (mtry = 5), 0.9672370 (mtry = 8).
# Una desviación estándar más baja indica que el RMSE es más consistente a través 
# de las particiones.
print(rf_cv_model$results)


# Guardar el modelo entrenado
getwd()
setwd("D:/Josefina/Proyectos/ProyectoChile/modelos")

save(rf_model, file="random_forest_model.RData")
save(rf_cv_model, file="random_forest_model_cv.RData")


print("Modelo Random Forest entrenado y guardado en 'random_forest_model.RData'.")
################################################################################
################################################################################
# cargar el modelo y aplicalo a otro set de datos

# Paso 1: Cargar el modelo
load("random_forest_model.RData")

# Paso 2: Preparar el nuevo conjunto de datos
# Supongamos que tienes un nuevo data.frame llamado 'new_data'
new_data <- data.frame(
  AOD_550 = runif(10, 0, 1),
  blh_mean = runif(10, 10, 30),
  sp_mean = runif(10, 30, 80),
  d2m_mean = runif(10, 0, 1),
  blh_mean = runif(10, 10, 30),
  t2m_mean = runif(10, 0, 1),
  v10_mean = runif(10, 10, 30),
  u10_mean = runif(10, 10, 30),
  tp_mean = runif(10, 0, 1)
  # Añadir más variables predictoras según sea necesario
)


# Paso 3: Aplicar el modelo al nuevo conjunto de datos
new_predictions <- predict(rf_model, newdata = new_data)

# Paso 4: Ver los resultados de las predicciones
print(new_predictions)

