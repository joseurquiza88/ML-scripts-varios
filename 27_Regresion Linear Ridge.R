
#Regresion lineal ocn penalizacion Ridge

# Usamos el parametro seguin Bagheri 2022 : regularization parameter: 0.1

# Librerias
library(caret)
library(glmnet)
# Abrimos archivos de train y test
dir <- "D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/"
setwd(dir)
train_data <- read.csv(paste(dir,"Modelo 1/M1_train.csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo 1/M1_test.csv",sep=""))

# Fórmula del modelo y el control de entrenamiento con validación cruzada

train_control <- trainControl(method = "cv", number = 10)


# Preparar las matrices y vectores de entrenamiento y testeo
x_train <- as.matrix(train_data[, c("AOD_055", "ndvi", "LandCover", "BCSMASS", "DUSMASS", "DUSMASS25", 
                                   "OCSMASS", "SO2SMASS", "SO4SMASS", "SSSMASS", "SSSMASS25", "blh_mean", 
                                   "sp_mean", "d2m_mean", "t2m_mean", "v10_mean", "u10_mean", "tp_mean", 
                                   "DEM", "dayWeek")])
# y_train <- train_data$PM25
y_train <- log(train_data$PM25 + 1)
# Transformación logarítmica de PM2.5


x_test <- as.matrix(test_data[, c("AOD_055", "ndvi", "LandCover", "BCSMASS", "DUSMASS", "DUSMASS25", 
                                 "OCSMASS", "SO2SMASS", "SO4SMASS", "SSSMASS", "SSSMASS25", "blh_mean", 
                                 "sp_mean", "d2m_mean", "t2m_mean", "v10_mean", "u10_mean", "tp_mean", 
                                 "DEM", "dayWeek")])
# y_test <- test_data$PM25
y_test <- log(test_data$PM25 + 1)
#El parámetro lambda controla la regularización
#Para Ridge solo usar valores positivos.  
# λ = 0.1 
ridge_model_cv <- train(x_train, y_train,
                     method = "glmnet",
                     trControl = train_control,
                     tuneGrid = expand.grid(alpha = 0,   # alpha = 0 para Ridge
                                            lambda = 0.1)) # Regularización de Ridge con λ = 0.1
# Ajustar el modelo de Ridge sin CV, especificando un valor de lambda

#print(ridge_model_cv)
#coef(ridge_model_cv$finalModel, s = ridge_model_cv$bestTune$lambda)


# Evaluamos el modelo en train and test
# Hacer predicciones
predicciones <- predict(ridge_model_cv, newdata = x_test)


# Deshacer la transformación logarítmica para obtener las predicciones en la escala original
predicciones_2 <- exp(predicciones) - 1
# Comparar predicciones con valores reales
resultado <- data.frame(Real = test_data$PM25, Predicho = predicciones_2)

#print(resultado)


rmse_cv <- sqrt(mean((predicciones - y_test)^2))
r_squared_cv <- cor(predicciones,y_test)^2
mae_cv <- mean(abs(y_test - predicciones))
mape_cv <- mean(abs((y_test - predicciones) / y_test)) * 100
mse_cv <- mean((y_test - predicciones)^2)
medae_cv <- median(abs(y_test- predicciones))
pearson_cor_cv <- cor(y_test, predicciones, method = "pearson")
media_estimador <- mean(medias_muestrales)
bias <- mean(resultado$Predicho-resultado$Real)

cat("R²:", r_squared_cv, "\n")
print(paste("R pearson cv:", round(pearson_cor_cv, 3)))
cat("RMSE:", rmse_cv, "\n")
print(paste("MAE cv:", round(mae_cv, 3)))
print(paste("MAPE cv:", round(mape_cv, 2), "%"))
print(paste("MSE:", round(mse_cv, 3)))
print(paste("MedAE cv:", round(medae_cv, 3)))
min(predicciones)
max(predicciones)

#########################################################
##########################################################
#Predicciones con dataest de entrenamiento
# Hacer predicciones
predicciones_train <- predict(ridge_model_cv, newdata = x_train)

# Comparar predicciones con valores reales
resultado_train <- data.frame(Real = y_train, Predicho = predicciones_train)



rmse_cv_train <- sqrt(mean((predicciones_train - y_train)^2))
r_squared_cv_train <- cor(predicciones_train, y_train)^2
mae_cv_train <- mean(abs(y_train - predicciones_train))
mape_cv_train <- mean(abs((y_train - predicciones_train) / y_train)) * 100
mse_cv_train <- mean((y_train - predicciones_train)^2)
medae_cv_train <- median(abs(y_train - predicciones_train))
pearson_cor_cv_train <- cor(y_train, predicciones_train, method = "pearson")

cat("R² train:", r_squared_cv_train, "\n")
print(paste("R pearson cv train:", round(pearson_cor_cv_train, 3)))
cat("RMSE train:", rmse_cv_train, "\n")
print(paste("MAE train cv:", round(mae_cv_train, 3)))
print(paste("MAPE train cv:", round(mape_cv_train, 2), "%"))
print(paste("MSE train:", round(mse_cv_train, 3)))
print(paste("MedAE traincv:", round(medae_cv_train, 3)))
min(predicciones_train)
max(predicciones_train)

# Guardamos modelo
setwd("D:/Josefina/Proyectos/ProyectoChile/modelos/modelo")
save(ridge_model_cv, file="01-RLR-M5_250924.RData")

############################################################################
###############################################################################
dir <- "D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/"
setwd(dir)
train_data <- read.csv(paste(dir,"Modelo 1/M1_train.csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo 1/M1_test.csv",sep=""))




# Preparar las matrices y vectores de entrenamiento y testeo
x_train <- as.matrix(train_data[, c("AOD_055", "ndvi", "LandCover", "BCSMASS", "DUSMASS", "DUSMASS25", 
                                    "OCSMASS", "SO2SMASS", "SO4SMASS", "SSSMASS", "SSSMASS25", "blh_mean", 
                                    "sp_mean", "d2m_mean", "t2m_mean", "v10_mean", "u10_mean", "tp_mean", 
                                    "DEM", "dayWeek")])
y_train <- train_data$PM25

x_test <- as.matrix(test_data[, c("AOD_055", "ndvi", "LandCover", "BCSMASS", "DUSMASS", "DUSMASS25", 
                                  "OCSMASS", "SO2SMASS", "SO4SMASS", "SSSMASS", "SSSMASS25", "blh_mean", 
                                  "sp_mean", "d2m_mean", "t2m_mean", "v10_mean", "u10_mean", "tp_mean", 
                                  "DEM", "dayWeek")])
y_test <- test_data$PM25

# Ajustar el modelo Ridge con validación cruzada
ridge_cv_model <- cv.glmnet(x_train, y_train, alpha = 0,  # alpha = 0 para Ridge
                            lambda = 10^seq(3, -3, by = -0.1), # Rango de valores de lambda
                            nfolds = 10)  # Validación cruzada de 10 pliegues


# Mejor valor de lambda según la validación cruzada
best_lambda <- ridge_cv_model$lambda.min

# También puedes obtener un valor de lambda más conservador (1 error estándar más)
best_lambda_1se <- ridge_cv_model$lambda.1se

# Ajustar el modelo final con el mejor lambda
ridge_final_model <- glmnet(x_train, y_train, alpha = 0, lambda = best_lambda)


# Hacer predicciones
# predicciones <- predict(ridge_final_model, newdata = x_test)
predicciones <- predict(ridge_final_model, newx = x_test)
# Comparar predicciones con valores reales
resultado <- data.frame(Real = test_data$PM25, Predicho = predicciones)
#print(resultado)


rmse_cv <- sqrt(mean((predicciones - y_test)^2))
r_squared_cv <- cor(predicciones,y_test)^2
mae_cv <- mean(abs(y_test - predicciones))
mape_cv <- mean(abs((y_test - predicciones) / y_test)) * 100
mse_cv <- mean((y_test - predicciones)^2)
medae_cv <- median(abs(y_test- predicciones))
pearson_cor_cv <- cor(y_test, predicciones, method = "pearson")

cat("R²:", r_squared_cv, "\n")
print(paste("R pearson cv:", round(pearson_cor_cv, 3)))
cat("RMSE:", rmse_cv, "\n")
print(paste("MAE cv:", round(mae_cv, 3)))
print(paste("MAPE cv:", round(mape_cv, 2), "%"))
print(paste("MSE:", round(mse_cv, 3)))
print(paste("MedAE cv:", round(medae_cv, 3)))
min(predicciones)
max(predicciones)

#########################################################
##########################################################
#Predicciones con dataest de entrenamiento
# Hacer predicciones
predicciones_train <- predict(ridge_model_cv, newdata = x_train)

# Comparar predicciones con valores reales
resultado_train <- data.frame(Real = y_train, Predicho = predicciones_train)



rmse_cv_train <- sqrt(mean((predicciones_train - y_train)^2))
r_squared_cv_train <- cor(predicciones_train, y_train)^2
mae_cv_train <- mean(abs(y_train - predicciones_train))
mape_cv_train <- mean(abs((y_train - predicciones_train) / y_train)) * 100
mse_cv_train <- mean((y_train - predicciones_train)^2)
medae_cv_train <- median(abs(y_train - predicciones_train))
pearson_cor_cv_train <- cor(y_train, predicciones_train, method = "pearson")

cat("R² train:", r_squared_cv_train, "\n")
print(paste("R pearson cv train:", round(pearson_cor_cv_train, 3)))
cat("RMSE train:", rmse_cv_train, "\n")
print(paste("MAE train cv:", round(mae_cv_train, 3)))
print(paste("MAPE train cv:", round(mape_cv_train, 2), "%"))
print(paste("MSE train:", round(mse_cv_train, 3)))
print(paste("MedAE traincv:", round(medae_cv_train, 3)))
min(predicciones_train)
max(predicciones_train)