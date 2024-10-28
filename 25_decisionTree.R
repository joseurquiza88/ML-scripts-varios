library(rpart)
library(rpart.plot)

test_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 1/M1_test.csv")
train_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 1/M1_train.csv")

# Definir la fórmula del modelo
formula <- PM25 ~ AOD_055 + ndvi + LandCover + BCSMASS + DUSMASS + DUSMASS25 + 
  OCSMASS + SO2SMASS + SO4SMASS + SSSMASS + SSSMASS25 + blh_mean +
  sp_mean + d2m_mean + t2m_mean + v10_mean + u10_mean + tp_mean + DEM + dayWeek

# Definir la fórmula del modelo
formula <- PM25 ~ AOD_055 + ndvi + LandCover + BCSMASS_dia + DUSMASS_dia  + DUSMASS25_dia  + 
  OCSMASS_dia  + SO2SMASS_dia  + SO4SMASS_dia  + SSSMASS_dia  + SSSMASS25_dia  + blh_mean +
  sp_mean + d2m_mean + t2m_mean + v10_mean + u10_mean + tp_mean + DEM + dayWeek


# Crear el modelo con rpart
decision_tree_model <- rpart(formula, data = train_data, method = "anova")  # Para regresión usa "anova"
rpart.plot(decision_tree_model, yesno = TRUE)
printcp(decision_tree_model)

data.frame(pred = predict(decision_tree_model, newdata = train_data)) %>%
  mutate(obs = train_data$PM25,
         sq_err = (obs - pred)^2) %>%
  summarise(sse = sum(sq_err))


decision_tree_model$cptable %>%
  data.frame() %>%
  mutate(min_xerror_idx = which.min(decision_tree_model$cptable[, "xerror"]),
         rownum = row_number(),
         xerror_cap = decision_tree_model$cptable[min_xerror_idx, "xerror"] + 
           decision_tree_model$cptable[min_xerror_idx, "xstd"],
         eval = case_when(rownum == min_xerror_idx ~ "min xerror",
                          xerror < xerror_cap ~ "under cap",
                          TRUE ~ "")) %>%
  dplyr::select(-rownum, -min_xerror_idx) 

plotcp(decision_tree_model, upper = "splits")

decision_tree_model_cart <- prune(
  decision_tree_model,
  cp = decision_tree_model$cptable[decision_tree_model$cptable[, 2] == 7, "CP"]
)
rpart.plot(decision_tree_model_cart, yesno = TRUE)

normalized_importance <- decision_tree_model_cart$variable.importance / sum(decision_tree_model_cart$variable.importance)
normalized_importance  %>% 
  data.frame() %>%
  rownames_to_column(var = "Feature") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(Feature, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Variable Importance with Simple Regression")


# Predecir los valores del conjunto de prueba
predictions <- predict(decision_tree_model, newdata = test_data)

# Calcular el RMSE (Raíz del Error Cuadrático Medio)
rmse <- sqrt(mean((predictions - test_data$PM25)^2))

# Mostrar el RMSE
print(paste("RMSE:", rmse))
# Visualizar el árbol
plot(decision_tree_model)
text(decision_tree_model, pretty = 1)

################################################################################################
################################################################################################
test_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 5/M5_test.csv")
train_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 5/M5_train.csv")
# Entrenar el modelo usando caret y rpart
# decision_tree_model_cv <- train(PM25 ~ AOD_055 + ndvi + LandCover + BCSMASS + DUSMASS + DUSMASS25 + 
#                                   OCSMASS + SO2SMASS + SO4SMASS + SSSMASS + SSSMASS25 + blh_mean +
#                                   sp_mean + d2m_mean + t2m_mean + v10_mean + u10_mean + tp_mean + DEM + dayWeek, data = train_data, 
#                                 method = "rpart", 
#                                 tuneLength = 5,
#                                 trControl = control)
# Definir la fórmula del modelo
formula <- PM25 ~ AOD_055 + ndvi + LandCover + BCSMASS + DUSMASS + DUSMASS25 + 
  OCSMASS + SO2SMASS + SO4SMASS + SSSMASS + SSSMASS25 + blh_mean +
  sp_mean + d2m_mean + t2m_mean + v10_mean + u10_mean + tp_mean + DEM + dayWeek
# Configurar la validación cruzada de 10 pliegues
control <- trainControl(method = "cv", number = 10)
# Definir un rango personalizado de valores del parámetro de complejidad (cp)
tuneGrid <- expand.grid(cp = seq(0.001, 0.1, by = 0.01))

# Entrenar el modelo con el grid de valores cp
decision_tree_model_cv <- train(formula, data = train_data, 
                                method = "rpart", 
                                trControl = control, 
                                tuneGrid = tuneGrid)
print(decision_tree_model_cv$bestTune)
print(decision_tree_model_cv$results)
# Predicción sobre el conjunto de test
predicciones <- predict(decision_tree_model_cv, newdata = test_data)

# Mostrar los resultados de la validación cruzada
print(decision_tree_model_cv)
resample_results = decision_tree_model_cv$resample
resample_results
# Calcular el RMSE (Raíz del Error Cuadrático Medio)
# metrica de train
predicciones_test <- predict(decision_tree_model_cv, newdata = test_data)
lm_r_squared_test <- cor(predicciones_test , test_data$PM25)^2
pearson_cor_test <- cor(test_data$PM25, predicciones_test, method = "pearson")
lm_rmse_test <- sqrt(mean((predicciones_test - test_data$PM25)^2))
mae_test <- mean(abs(test_data$PM25 - predicciones_test))
mape_test <- mean(abs((test_data$PM25 - predicciones_test) / test_data$PM25)) * 100
mse_test <- mean((test_data$PM25 - predicciones_test)^2)
medae_test <- median(abs(test_data$PM25 - predicciones_test))


print(paste("R2 dataset test:", round(lm_r_squared_test, 2)))
print(paste("R pearson test:", round(pearson_cor_test, 2)))
print(paste("RMSE test:", round(lm_rmse_test, 2)))
print(paste("mae test:", round(mae_test, 2)))
print(paste("MAPE test:", round(mape_test, 2), "%"))
print(paste("MSE test:", round(mse_test, 2)))
print(paste("MedAE test:", round(medae_test,2)))
min(predicciones)
max(predicciones)
# Visualizar el árbol
plot(model_cv)
text(model_cv, pretty = 1)
#### METRICAS DE ENTRENAMINTO

# metrica de train
predicciones_train <- predict(decision_tree_model_cv, newdata = train_data)
lm_r_squared_train <- cor(predicciones_train , train_data$PM25)^2
pearson_cor_train <- cor(train_data$PM25, predicciones_train, method = "pearson")
lm_rmse_train <- sqrt(mean((predicciones_train - train_data$PM25)^2))
mae_train <- mean(abs(train_data$PM25 - predicciones_train))
mape_train <- mean(abs((train_data$PM25 - predicciones_train) / train_data$PM25)) * 100
mse_train <- mean((train_data$PM25 - predicciones_train)^2)
medae_train <- median(abs(train_data$PM25 - predicciones_train))


print(paste("R2 dataset train:", round(lm_r_squared_train, 2)))
print(paste("R pearson train:", round(pearson_cor_train, 2)))
print(paste("RMSE train:", round(lm_rmse_train, 2)))
print(paste("mae train:", round(mae_train, 2)))
print(paste("MAPE train:", round(mape_train, 2), "%"))
print(paste("MSE train:", round(mse_train, 2)))
print(paste("MedAE train:", round(medae_train,2)))
min(predicciones_train)
max(predicciones_train)

setwd("D:/Josefina/Proyectos/ProyectoChile/modelos/modelo")

save(decision_tree_model_cv, file="02-DTree_cv_M1-141024.RData")

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
                                  tuneGrid = grid)#,
                                  control = control_rpart)  # Usar control de rpart
06:48
# Mostrar los mejores parámetros encontrados
print(decision_tree_model_cv_s$bestTune)
plot(decision_tree_model_cv_s)
rpart.plot(decision_tree_model_cv_s$finalModel)
plot(varImp(decision_tree_model_cv_s), main="Importancia de variables para Regresión")

data.frame(Actual = train_data$PM25, Predicted = predicciones_train) %>%
  ggplot(aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.6, color = "cadetblue") +
  geom_smooth(method = "loess", formula = "y ~ x") +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  labs(title = "Predicted vs Actual (caret)")


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
# metrica de tEST
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

setwd("D:/Josefina/Proyectos/ProyectoChile/modelos/modelo")

save(decision_tree_model_cv_s, file="01-DTree_cv_M1-120924.RData")




cs_scoreboard <- rbind(
  data.frame(Modelo = "Single Tree", RMSE = cs_rmse_cart),
  data.frame(Modelo = "Single Tree (caret)", RMSE = cs_rmse_cart2)
) %>% arrange(RMSE)
knitr::kable(cs_scoreboard, row.names = FALSE)