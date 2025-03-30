# Definir las variables predictoras
variables_predictoras <- c("AOD_055", "blh_mean", "sp_mean", "d2m_mean", "t2m_mean", "u10_mean", "v10_mean", "tp_mean",
                           "BCSMASS_dia", "DUSMASS_dia", "DUSMASS25_dia", 
                           "OCSMASS_dia", "SO2SMASS_dia", "SO4SMASS_dia", "SSSMASS_dia",
                           "SSSMASS25_dia")


# Crear un dataframe vacío para almacenar las métricas
metrics_df <- data.frame(
  Model = character(),
  R = numeric(),
  R2 = numeric(),
  RMSE = numeric(),
  Bias = numeric(),
  Slope = numeric(),
  Intercept = numeric(),
  stringsAsFactors = FALSE
)

# Crear un dataframe vacío para almacenar las predicciones de cada modelo
predictions_df <- data.frame(
  Model = character(),
  Predicted = numeric(),
  Actual = numeric(),
  stringsAsFactors = FALSE
)

# Iterar sobre las variables predictoras, agregando una por una al modelo
for (i in 1:length(variables_predictoras)) {
  # Seleccionar las variables predictoras para el modelo actual
  predictor_vars <- variables_predictoras[1:i]
  
  # Crear la fórmula del modelo
  formula <- as.formula(paste("PM25 ~", paste(predictor_vars, collapse = " + ")))
  
  # Ajustar el modelo de regresión lineal con el conjunto de datos de entrenamiento
  model <- lm(formula, data = train_data)
  
  # Realizar predicciones sobre el conjunto de datos de prueba
  predictions <- predict(model, newdata = test_data)
  
  # Valores reales de PM25 en el conjunto de prueba
  actual <- test_data$PM25
  
  # Filtrar las predicciones y valores reales mayores a 0
  idx <- which(predictions > 0 & actual > 0)
  predictions_filtradas <- predictions[idx]
  actual_filtradas <- actual[idx]
  
  # Calcular métricas
  r <- cor(actual_filtradas, predictions_filtradas)            # Correlación
  r2 <- r^2                                                   # Coeficiente de determinación
  rmse <- sqrt(mean((predictions_filtradas - actual_filtradas)^2))  # RMSE
  bias <- mean(predictions_filtradas - actual_filtradas)        # Sesgo
  slope <- coef(lm(actual_filtradas ~ predictions_filtradas))[2]  # Pendiente de la regresión
  intercept <- coef(lm(actual_filtradas ~ predictions_filtradas))[1]  # Intercepto de la regresión
  
  # Guardar las métricas en el dataframe
  metrics_df <- rbind(metrics_df, data.frame(
    Model = paste("Modelo con variables:", paste(predictor_vars, collapse = ", ")),
    R = round(r, 3),
    R2 = round(r2, 3),
    RMSE = round(rmse, 3),
    Bias = round(bias, 5),
    Slope = round(slope, 3),
    Intercept = round(intercept, 3)
  ))
  
  # Guardar las predicciones y valores reales filtrados para el gráfico
  predictions_df <- rbind(predictions_df, data.frame(
    Model = paste("Modelo con variables:", paste(predictor_vars, collapse = ", ")),
    Predicted = predictions_filtradas,
    Actual = actual_filtradas
  ))
}

# Mostrar las métricas
View(metrics_df)
