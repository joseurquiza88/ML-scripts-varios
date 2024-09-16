

#### --Bagging árboles de regresión


library(rpart)
library(rpart.plot)

test_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 1/M1_test.csv")
train_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 1/M1_train.csv")

# Definir la fórmula del modelo
formula <- PM25 ~ AOD_055 + ndvi + LandCover + BCSMASS + DUSMASS + DUSMASS25 + 
  OCSMASS + SO2SMASS + SO4SMASS + SSSMASS + SSSMASS25 + blh_mean +
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


