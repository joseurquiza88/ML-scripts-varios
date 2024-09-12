library(rpart)

test_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 1/M1_test.csv")
train_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 1/M1_train.csv")

# Definir la fórmula del modelo
formula <- PM25 ~ AOD_055 + ndvi + LandCover + BCSMASS + DUSMASS + DUSMASS25 + 
  OCSMASS + SO2SMASS + SO4SMASS + SSSMASS + SSSMASS25 + blh_mean +
  sp_mean + d2m_mean + t2m_mean + v10_mean + u10_mean + tp_mean + DEM + dayWeek

# Crear el modelo con rpart
decision_tree_model <- rpart(formula, data = train_data, method = "anova")  # Para regresión usa "anova"


# Predecir los valores del conjunto de prueba
predictions <- predict(decision_tree_model, newdata = test_data)

# Calcular el RMSE (Raíz del Error Cuadrático Medio)
rmse <- sqrt(mean((predictions - test_data$PM25)^2))

# Mostrar el RMSE
print(paste("RMSE:", rmse))
# Visualizar el árbol
plot(decision_tree_model)
text(decision_tree_model, pretty = 1)

################################################
# Configurar la validación cruzada de 10 pliegues
control <- trainControl(method = "cv", number = 10)

# Entrenar el modelo usando caret y rpart
decision_tree_model_cv <- train(formula, data = train_data, method = "rpart", trControl = control)

# Mostrar los resultados de la validación cruzada
print(decision_tree_model_cv)

# Calcular el RMSE (Raíz del Error Cuadrático Medio)
# metrica de train
predicciones_test <- predict(decision_tree_model_cv, newdata = test_data)
lm_r_squared_test <- cor(predicciones_test , test_data$PM25)^2
print(paste("R2 dataset test:", round(lm_r_squared_test, 2)))
pearson_cor_test <- cor(test_data$PM25, predicciones_test, method = "pearson")
print(paste("R pearson test:", round(pearson_cor_test, 2)))
lm_rmse_test <- sqrt(mean((predicciones_test - test_data$PM25)^2))
print(paste("RMSE test:", round(lm_rmse_test, 2)))
mae_test <- mean(abs(test_data$PM25 - predicciones_test))
print(paste("mae test:", round(mae_test, 2)))

mape_test <- mean(abs((test_data$PM25 - predicciones_test) / test_data$PM25)) * 100
print(paste("MAPE test:", round(mape_test, 2), "%"))

mse_test <- mean((test_data$PM25 - predicciones_test)^2)
print(paste("MSE test:", round(mse_test, 2)))

medae_test <- median(abs(test_data$PM25 - predicciones_test))
print(paste("MedAE test:", round(medae_test,2)))
# Visualizar el árbol
plot(model_cv)
text(model_cv, pretty = 1)
#### METRICAS DE ENTRENAMINTO

# metrica de train
predicciones_train <- predict(decision_tree_model_cv, newdata = train_data)
lm_r_squared_train <- cor(predicciones_train , train_data$PM25)^2
print(paste("R2 dataset train:", round(lm_r_squared_train, 2)))
pearson_cor_train <- cor(train_data$PM25, predicciones_train, method = "pearson")
print(paste("R pearson train:", round(pearson_cor_train, 2)))
lm_rmse_train <- sqrt(mean((predicciones_train - train_data$PM25)^2))
print(paste("RMSE train:", round(lm_rmse_train, 2)))
mae_train <- mean(abs(train_data$PM25 - predicciones_train))
print(paste("mae train:", round(mae_train, 2)))

mape_train <- mean(abs((train_data$PM25 - predicciones_train) / train_data$PM25)) * 100
print(paste("MAPE train:", round(mape_train, 2), "%"))

mse_train <- mean((train_data$PM25 - predicciones_train)^2)
print(paste("MSE train:", round(mse_train, 2)))

medae_train <- median(abs(train_data$PM25 - predicciones_train))
print(paste("MedAE train:", round(medae_train,2)))

##############################################################################
##############################################################################
#B usqueda de hiperparametros + cv
# Crear una rejilla de valores de hiperparámetros solo con el parámetro cp
grid <- expand.grid(cp = seq(0.001, 0.1, by = 0.01))

# Configurar los controles del árbol de decisión
control_rpart <- rpart.control(minsplit = 20, maxdepth = 5)

# Configurar la validación cruzada de 10 pliegues
control <- trainControl(method = "cv", number = 10)

# Ajustar el modelo usando la rejilla de hiperparámetros y los controles de rpart
decision_tree_model_cv_s <- train(PM25 ~ AOD_055 + ndvi + LandCover + BCSMASS + DUSMASS + DUSMASS25 +
                                    OCSMASS + SO2SMASS + SO4SMASS + SSSMASS + SSSMASS25 + blh_mean +
                                    sp_mean + d2m_mean + t2m_mean + v10_mean + u10_mean + tp_mean + DEM + dayWeek,
                                  data = train_data,
                                  method = "rpart",
                                  trControl = control,
                                  tuneGrid = grid,
                                  control = control_rpart)  # Usar control de rpart
06:48
# Mostrar los mejores parámetros encontrados
print(decision_tree_model_cv_s$bestTune)

predicciones_train <- predict(decision_tree_model_cv_s, newdata = train_data)
lm_r_squared_train <- cor(predicciones_train , train_data$PM25)^2
print(paste("R2 dataset train:", round(lm_r_squared_train, 2)))
pearson_cor_train <- cor(train_data$PM25, predicciones_train, method = "pearson")
print(paste("R pearson train:", round(pearson_cor_train, 2)))
lm_rmse_train <- sqrt(mean((predicciones_train - train_data$PM25)^2))
print(paste("RMSE train:", round(lm_rmse_train, 2)))
mae_train <- mean(abs(train_data$PM25 - predicciones_train))
print(paste("mae train:", round(mae_train, 2)))

mape_train <- mean(abs((train_data$PM25 - predicciones_train) / train_data$PM25)) * 100
print(paste("MAPE train:", round(mape_train, 2), "%"))

mse_train <- mean((train_data$PM25 - predicciones_train)^2)
print(paste("MSE train:", round(mse_train, 2)))

medae_train <- median(abs(train_data$PM25 - predicciones_train))
print(paste("MedAE train:", round(medae_train,2)))


######
# metrica de train
predicciones_test <- predict(decision_tree_model_cv_s, newdata = test_data)
lm_r_squared_test <- cor(predicciones_test , test_data$PM25)^2
print(paste("R2 dataset test:", round(lm_r_squared_test, 2)))
pearson_cor_test <- cor(test_data$PM25, predicciones_test, method = "pearson")
print(paste("R pearson test:", round(pearson_cor_test, 2)))
lm_rmse_test <- sqrt(mean((predicciones_test - test_data$PM25)^2))
print(paste("RMSE test:", round(lm_rmse_test, 2)))
mae_test <- mean(abs(test_data$PM25 - predicciones_test))
print(paste("mae test:", round(mae_test, 2)))

mape_test <- mean(abs((test_data$PM25 - predicciones_test) / test_data$PM25)) * 100
print(paste("MAPE test:", round(mape_test, 2), "%"))

mse_test <- mean((test_data$PM25 - predicciones_test)^2)
print(paste("MSE test:", round(mse_test, 2)))

medae_test <- median(abs(test_data$PM25 - predicciones_test))
print(paste("MedAE test:", round(medae_test,2)))

