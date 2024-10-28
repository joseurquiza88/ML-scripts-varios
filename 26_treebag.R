

#### --Bagging árboles de regresión


library(rpart)
library(rpart.plot)

test_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 1/M1_test.csv")
train_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 1/M1_train.csv")

# Definir la fórmula del modelo
formula <- PM25 ~ AOD_055 + ndvi + LandCover + BCSMASS + DUSMASS + DUSMASS25 + 
  OCSMASS + SO2SMASS + SO4SMASS + SSSMASS + SSSMASS25 + blh_mean +
  sp_mean + d2m_mean + t2m_mean + v10_mean + u10_mean + tp_mean + DEM + dayWeek

# Definir la fórmula del modelo
formula <- PM25 ~ AOD_055 + ndvi + LandCover + BCSMASS_dia + DUSMASS_dia + DUSMASS25_dia + 
  OCSMASS_dia + SO2SMASS_dia + SO4SMASS_dia + SSSMASS_dia + SSSMASS25_dia + blh_mean +
  sp_mean + d2m_mean + t2m_mean + v10_mean + u10_mean + tp_mean + DEM + dayWeek

cs_trControl = trainControl(
  method = "cv",
  number = 10,
  savePredictions = "final" 
)
set.seed(1234)
cs_mdl_bag <- train(
  formula, 
  data = test_data, 
  method = "treebag",
  trControl = cs_trControl
)
cs_mdl_bag$results


cs_preds_bag <- bind_cols(
  Predicted = predict(cs_mdl_bag, newdata = test_data),
  Actual = test_data$PM25
)
(cs_rmse_bag <- RMSE(pred = cs_preds_bag$Predicted, obs = cs_preds_bag$Actual))


cs_preds_bag %>%
  ggplot(aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.6, color = "cadetblue") +
  geom_smooth(method = "loess", formula = "y ~ x") +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  labs(title = "Carseats Bagging, Predicted vs Actual (caret)")


plot(varImp(cs_mdl_bag), main="Importancia de variables con Bagging")

#############################################################################
#################################################################################
test_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 5/M5_test.csv")
train_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 5/M5_train.csv")

# Definir la fórmula del modelo
formula <- PM25 ~ AOD_055 + ndvi + LandCover + BCSMASS + DUSMASS + DUSMASS25 + 
  OCSMASS + SO2SMASS + SO4SMASS + SSSMASS + SSSMASS25 + blh_mean +
  sp_mean + d2m_mean + t2m_mean + v10_mean + u10_mean + tp_mean + DEM + dayWeek

# Definir el control de entrenamiento
cs_trControl <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation

# Entrenar el modelo
treeBag_model_cv <- train(
  formula, 
  data = train_data, 
  method = "treebag",
  trControl = cs_trControl)
treeBag_model_cv$results

predicciones_train <- predict(treeBag_model_cv, newdata = train_data)
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

######
# metrica de tEST
predicciones_test <- predict(treeBag_model_cv, newdata = test_data)
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
min(predicciones_test)
max(predicciones_test)
setwd("D:/Josefina/Proyectos/ProyectoChile/modelos/modelo")

save(treeBag_model_cv, file="02-TreeBag_cv_M1-141024.RData")


