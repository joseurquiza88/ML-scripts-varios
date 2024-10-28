# Cargar los paquetes necesarios
library(randomForest)
library(raster)
library(caret) #version 4.2.3
library(e1071)
rm(list=ls())
# Generar datos aleatorios para las variables predictoras
set.seed(42)

# Leer los datos
data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/merge_tot/08_TOT_merge_tot.csv")
data_completo <- data[complete.cases(data), ]

# Normalizar las variables entre 0 y 1 usando Min-Max
preProc <- preProcess(data_completo[, c("AOD_055", "ndvi", "LandCover", "BCSMASS", "DUSMASS", "DUSMASS25", 
                                        "OCSMASS", "SO2SMASS", "SO4SMASS", "SSSMASS", "SSSMASS25", "blh_mean", 
                                        "sp_mean", "d2m_mean", "t2m_mean", "v10_mean", "u10_mean", "tp_mean", "DEM")], 
                      method = "range")

# Aplicar la transformación a los datos completos
data_normalizada <- predict(preProc, data_completo)

# Dividir el dataframe en 70% entrenamiento y 30% testeo
train_index <- createDataPartition(data_normalizada$PM25, p = 0.7, list = FALSE)
train_data <- data_normalizada[train_index, ]
test_data <- data_normalizada[-train_index, ]

# Entrenar el modelo SVR con datos normalizados
svr_model <- svm(PM25 ~ AOD_055 + ndvi + LandCover + BCSMASS +
                   DUSMASS + DUSMASS25 + OCSMASS + SO2SMASS +
                   SO4SMASS + SSSMASS + SSSMASS25 + blh_mean +
                   sp_mean + d2m_mean + t2m_mean + v10_mean + 
                   u10_mean + tp_mean + DEM, data = train_data)

# Realizar predicciones en los datos de prueba
predicciones <- predict(svr_model, newdata = test_data)

# Comparar las predicciones con los valores reales
resultado <- data.frame(Real = test_data$PM25, Predicho = predicciones)
print(resultado)

# Calcular el error medio cuadrático (RMSE)
rmse <- sqrt(mean((resultado$Real - resultado$Predicho)^2))
print(paste("RMSE:", rmse))

# Calcular el coeficiente de determinación (R²)
r_squared <- cor(predicciones, test_data$PM25)^2

# Mostrar las métricas de evaluación
cat("RMSE:", rmse, "\n")
cat("R²:", r_squared, "\n")

####################################################################
####################################################################
####################################################################
test_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 1/M1_test.csv")
train_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 1/M1_train.csv")
# Normalizar las variables entre 0 y 1 usando Min-Max
preProc <- preProcess(train_data[, c("AOD_055", "ndvi",  "BCSMASS_dia", "DUSMASS_dia", "DUSMASS25_dia", 
                                        "OCSMASS_dia", "SO2SMASS_dia", "SO4SMASS_dia", "SSSMASS_dia", "SSSMASS25_dia", "blh_mean", 
                                        "sp_mean", "d2m_mean", "t2m_mean", "v10_mean", "u10_mean", "tp_mean", "DEM")], 
                      method = "range")#"LandCover",
# Multiples parametros
# Definir el control de entrenamiento con validación cruzada de 10 pliegues
train_control <- trainControl(method = "cv", number = 10)

# Definir un grid para ajustar los hiperparámetros
svr_grid <- expand.grid(sigma = c(0.01, 0.05, 0.1),
                        C = c(1, 10, 100))

# Entrenar el modelo SVR con validación cruzada y ajuste de hiperparámetros
svr_cv_model <- train(PM25 ~ AOD_055 + ndvi + LandCover + BCSMASS +
                        DUSMASS + DUSMASS25 + OCSMASS + SO2SMASS +
                        SO4SMASS + SSSMASS + SSSMASS25 + blh_mean +
                        sp_mean + d2m_mean + t2m_mean + v10_mean + 
                        u10_mean + tp_mean + DEM, 
                      data = train_data, 
                      method = "svmRadial", 
                      trControl = train_control, 
                      tuneGrid = svr_grid)

# Mostrar los resultados del modelo
print(svr_cv_model)
print(svr_cv_model$results)

# Realizar predicciones en los datos de prueba usando el mejor modelo encontrado
predicciones <- predict(svr_cv_model, newdata = test_data)

# Comparar las predicciones con los valores reales
resultado <- data.frame(Real = test_data$PM25, Predicho = predicciones)
print(resultado)

# Calcular el error medio cuadrático (RMSE)
rmse <- sqrt(mean((resultado$Real - resultado$Predicho)^2))
print(paste("RMSE:", rmse))

# Calcular el coeficiente de determinación (R²)
r_squared <- cor(predicciones, test_data$PM25)^2

# Mostrar las métricas de evaluación
cat("RMSE:", rmse, "\n")
cat("R²:", r_squared, "\n")



####################################################################
####################################################################
####################################################################
# varios parametros para comprobar
# Definir el control de entrenamiento con validaciÃ³n cruzada de 10 pliegues
train_control <- trainControl(method = "cv", number = 10)
train_control <- trainControl(
  method = "cv",          # Método de validación cruzada
  number = 10,            # Número de pliegues para la validación cruzada
  verboseIter = TRUE,     # Mostrar progreso de entrenamiento
  allowParallel = TRUE    # Permitir procesamiento paralelo
)

# Definir un grid para ajustar los hiperparámetros
svr_grid <- expand.grid(sigma = c(0.05, 0.1),
                        C = c(100))

# Entrenar el modelo SVR con validación cruzada y ajuste de hiperparámetros
rf_cv_model_params <- train(PM25 ~ AOD_055 + ndvi + LandCover + BCSMASS_dia +
                        DUSMASS_dia + DUSMASS25_dia + OCSMASS_dia + SO2SMASS_dia +
                        SO4SMASS_dia + SSSMASS_dia + SSSMASS25_dia + blh_mean +
                        sp_mean + d2m_mean + t2m_mean + v10_mean + 
                        u10_mean + tp_mean + DEM, data = train_data, 
                      method = "svmRadial", trControl = train_control, 
                      tuneGrid = svr_grid)
# tiempo ==> 09:17
# Mostrar los resultados del modelo
print(rf_cv_model_params)
print(rf_cv_model_params$results)

# Mostrar los mejores hiperparámetros encontrados
best_params <- rf_cv_model_params$bestTune
print(best_params)

# Realizar predicciones en los datos de prueba
predicciones <- predict(rf_cv_model_params, newdata = test_data)

# Calcular el RMSE en los datos de prueba
rmse <- sqrt(mean((test_data$PM25 - predicciones)^2))
# Calcular el coeficiente de determinaciÃ³n (RÂ²)
r_squared <- cor(predicciones, test_data$PM25)^2

# Mostrar las mÃ©tricas de evaluaciÃ³n
cat("RMSE:", rmse, "/n")
cat("R²:", r_squared, "/n")


# Guardar el modelo entrenado
getwd()
setwd("D:/Josefina/Proyectos/ProyectoChile/modelos/modelo")

save(svr_model, file="01-SVR_260824.RData")
save(svr_cv_model, file="01-SVR_cv_260824.RData")
save(rf_cv_model_params, file="01-SVR_cv_params_260824.RData")


print("Modelo Random Forest entrenado y guardado en 'random_forest_model.RData'.")
################################################################################
#################################################################################
# Algunos plots para evaluar el modelo

# Predecir en el conjunto de prueba usando el modelo SVR ajustado
svr_predictions <- predict(rf_cv_model_params, newdata = test_data)

##### ---- Gráfico de dispersión de Predicciones vs Valores Reales
#muestra cómo se alinean las predicciones del modelo SVR con los valores
#reales. La línea y = x sirve como referencia para una predicción perfecta.
plot(test_data$PM25, svr_predictions, 
     xlab = "Valores Reales de PM2.5", 
     ylab = "Predicciones de PM2.5", 
     main = "Predicciones vs Valores Reales (SVR)")
abline(0, 1, col = "red")  # Añadir una línea y = x

##### ----  Calcular los residuos
svr_residuos <- test_data$PM25 - svr_predictions

##### ----  Gráfico de residuos vs. predicciones
# ayuda a verificar si los residuos están distribuidos 
#aleatoriamente. Un patrón en los residuos puede indicar que
#el modelo no está capturando una relación importante.
plot(svr_predictions, svr_residuos, 
     xlab = "Predicciones de PM2.5", 
     ylab = "Residuos", 
     main = "Residuos vs Predicciones (SVR)")
abline(h = 0, col = "red")  # Añadir una línea horizontal en y = 0

##### ---- Histograma de los residuos
#Muestra si están distribuidos normalmente, que es uno de los supuestos básicos
#para muchos modelos de regresión
hist(svr_residuos, 
     breaks = 20, 
     xlab = "Residuos", 
     main = "Histograma de los Residuos (SVR)")

##### ---- Q-Q plot de los residuos
#compara la distribución de los residuos con una distribución 
#normal. Idealmente, los puntos deberían seguir una línea recta.

qqnorm(svr_residuos)
qqline(svr_residuos, col = "red")


# Calcular el error absoluto
abs_error <- abs(svr_residuos)

##### ---- Gráfico de error absoluto vs. predicciones
#muestra el error absoluto de las predicciones en función de 
#las predicciones mismas. Ayuda a detectar si el modelo comete 
#errores sistemáticos en ciertas regiones de los datos.

plot(svr_predictions, abs_error, 
     xlab = "Predicciones de PM2.5", 
     ylab = "Error Absoluto", 
     main = "Error Absoluto vs Predicciones (SVR)")
################################################################################
################################################################################
test_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 1/M1_test.csv")
train_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 1/M1_train.csv")
# Normalizar las variables entre 0 y 1 usando Min-Max
# Normalizar los datos
preProc <- preProcess(train_data[, -which(names(train_data) == "PM25")], method = c("center", "scale"))
train_data_normalized <- predict(preProc, train_data)
train_data_normalized<- train_data_normalized[, c("AOD_055", "ndvi",  "BCSMASS_dia", "DUSMASS_dia", "DUSMASS25_dia", "OCSMASS_dia",
                "SO2SMASS_dia", "SO4SMASS_dia", "SSSMASS_dia", "SSSMASS25_dia", "blh_mean","sp_mean",
                "d2m_mean", "t2m_mean", "v10_mean", "u10_mean", "tp_mean", "DEM")]

train_data_normalized$PM25 <- train_data$PM25

test_data_normalized <- predict(preProc, test_data)
test_data_normalized<- test_data_normalized[, c("AOD_055", "ndvi",  "BCSMASS_dia", "DUSMASS_dia", "DUSMASS25_dia", "OCSMASS_dia",
                                                  "SO2SMASS_dia", "SO4SMASS_dia", "SSSMASS_dia", "SSSMASS25_dia", "blh_mean","sp_mean",
                                                  "d2m_mean", "t2m_mean", "v10_mean", "u10_mean", "tp_mean", "DEM")]
test_data_normalized$PM25 <- test_data$PM25
# Definir el control de entrenamiento con validaciÃ³n cruzada de 10 pliegues
train_control <- trainControl(
  method = "cv",          # Método de validación cruzada
  number = 10,            # Número de pliegues para la validación cruzada
  verboseIter = TRUE,     # Mostrar progreso de entrenamiento
  allowParallel = TRUE    # Permitir procesamiento paralelo
)



# Definir un grid para ajustar los hiperparámetros
svr_grid <- expand.grid(sigma = 0.1,
                        C = 100)

# Entrenar el modelo SVR con validación cruzada y ajuste de hiperparámetros
rf_cv_model_params <- train(PM25 ~ AOD_055 + ndvi +  BCSMASS_dia + #LandCover
                              DUSMASS_dia + DUSMASS25_dia + OCSMASS_dia + SO2SMASS_dia +
                              SO4SMASS_dia + SSSMASS_dia + SSSMASS25_dia + blh_mean +
                              sp_mean + d2m_mean + t2m_mean + v10_mean + 
                              u10_mean + tp_mean + DEM, data = train_data_normalized, 
                            method = "svmRadial", #trControl = train_control, 
                            tuneGrid = svr_grid)
09:25
# Realizar predicciones en el conjunto de prueba
predicciones <- predict(rf_cv_model_params, newdata = test_data_normalized)
rmse_cv <- sqrt(mean((predicciones - test_data_normalized$PM25)^2))
r_squared_cv <- cor(predicciones,test_data_normalized$PM25)^2
mae_cv <- mean(abs(test_data_normalized$PM25 - predicciones))
mape_cv <- mean(abs((test_data_normalized$PM25 - predicciones) / test_data_normalized$PM25)) * 100
mse_cv <- mean((test_data_normalized$PM25 - predicciones)^2)
medae_cv <- median(abs(test_data_normalized$PM25- predicciones))
pearson_cor_cv <- cor(test_data_normalized$PM25, predicciones, method = "pearson")


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
predicciones_train <- predict(rf_cv_model_params, newdata = train_data_normalized)
resultado_train <- data.frame(Real = train_data_normalized$PM25, Predicho = predicciones_train)
rmse_cv_train <- sqrt(mean((predicciones_train - train_data_normalized$PM25)^2))
r_squared_cv_train <- cor(predicciones_train, train_data_normalized$PM25)^2
mae_cv_train <- mean(abs(train_data_normalized$PM25 - predicciones_train))
mape_cv_train <- mean(abs((train_data_normalized$PM25 - predicciones_train) / train_data_normalized$PM25)) * 100
mse_cv_train <- mean((train_data_normalized$PM25 - predicciones_train)^2)
medae_cv_train <- median(abs(train_data_normalized$PM25 - predicciones_train))
pearson_cor_cv_train <- cor(train_data_normalized$PM25, predicciones_train, method = "pearson")

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
################################################################################
################################################################################
# cargar el modelo y aplicalo a otro set de datos

# Paso 1: Cargar el modelo
load("01-SVR_260824.RData")

dir_tiff <- "D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/tiff/"
# Paso 2: Preparar el nuevo conjunto de datos
# Supongamos que tienes un nuevo data.frame llamado 'new_data'
maiac <- raster(paste(dir_tiff,"MAIAC_raster.tif",sep=""))
NDVI <- raster(paste(dir_tiff,"NDVI_raster.tif",sep=""))
LandCover <- raster(paste(dir_tiff,"LandCover_raster.tif",sep=""))
DEM <- raster(paste(dir_tiff,"DEM_raster.tif",sep=""))
BCSMASS <- raster(paste(dir_tiff,"BCSMASS_raster.tif",sep=""))
DUSMASS <- raster(paste(dir_tiff,"DUSMASS_raster.tif",sep=""))
DUSMASS25 <- raster(paste(dir_tiff,"DUSMASS25_raster.tif",sep=""))
OCSMASS <- raster(paste(dir_tiff,"OCSMASS_raster.tif",sep=""))
SO2SMASS <- raster(paste(dir_tiff,"SO2SMASS_raster.tif",sep=""))
SO4SMASS <- raster(paste(dir_tiff,"SO4SMASS_raster.tif",sep=""))
SSSMASS <- raster(paste(dir_tiff,"SSSMASS_raster.tif",sep=""))
SSSMASS25 <- raster(paste(dir_tiff,"SSSMASS25_raster.tif",sep=""))
blh_mean <- raster(paste(dir_tiff,"blh_raster.tif",sep=""))
sp_mean <- raster(paste(dir_tiff,"sp_raster.tif",sep=""))
d2m_mean <- raster(paste(dir_tiff,"d2m_raster.tif",sep=""))
t2m_mean <- raster(paste(dir_tiff,"t2m_raster.tif",sep=""))
v10_mean <- raster(paste(dir_tiff,"v10_raster.tif",sep=""))
u10_mean <- raster(paste(dir_tiff,"u10_raster.tif",sep=""))
tp_mean <- raster(paste(dir_tiff,"tp_raster.tif",sep=""))

# Generamos stack
r_stack <- stack(maiac,NDVI,LandCover,BCSMASS ,
                 DUSMASS,DUSMASS25, OCSMASS,SO2SMASS,
                 SO4SMASS,SSSMASS ,SSSMASS25,blh_mean,
                 sp_mean , d2m_mean, t2m_mean,v10_mean, 
                 u10_mean ,tp_mean ,DEM)
#plot(r_stack)

r_stack_df <- as.data.frame(r_stack, na.rm = TRUE)
names(r_stack_df) <- c("AOD_055", "ndvi", "LandCover", "BCSMASS","DUSMASS",
                       "DUSMASS25","OCSMASS", "SO2SMASS","SO4SMASS","SSSMASS",
                       "SSSMASS25","blh_mean", "sp_mean" , "d2m_mean", "t2m_mean",
                       "v10_mean","u10_mean" ,"tp_mean","DEM")
# Aplicar el modelo de Random Forest al data frame
# rf_cv_model_params ES svr
predictions <- predict(svr_model, newdata = r_stack_df)

# Crear un raster vac?o con la misma extensi?n y resoluci?n que el stack
pred_raster <- raster(r_stack)

# Asignar las predicciones al raster
pred_raster[] <- NA  # Inicia con valores NA

# Reinsertar las predicciones en las celdas correspondientes
pred_raster[!is.na(values(r_stack[[1]]))] <- predictions

getwd()

writeRaster(pred_raster, filename = "D:/Josefina/Proyectos/ProyectoChile/modelos/SalidaModelo/01-SVR_model_260824.tif", format = "GTiff", overwrite = TRUE)




