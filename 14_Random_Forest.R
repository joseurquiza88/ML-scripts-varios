# Cargar los paquetes necesarios
library(randomForest)
library(raster)
library(caret) #version 4.2.3
rm(list=ls())

#Data modelo 1
test_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 1/M1_test.csv")
train_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 1/M1_train.csv")

# #Data modelo 2
test_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 2/M2_test.csv")
train_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 2/M2_train.csv")
# 
# #Data modelo 3
test_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 3/M3_test.csv")
train_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 3/M3_train.csv")
# 
# #Data modelo 4
test_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 4/M4_test.csv")
train_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 4/M4_train.csv")

# #Data modelo 5
test_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 5/M5_test.csv")
train_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 5/M5_train.csv")

# #Data modelo 6
test_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 6/M6_test.csv")
train_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 6/M6_train.csv")


# Entrenar el modelo de Random Forest
rf_model <- randomForest(PM25 ~ AOD_055 + ndvi + LandCover + BCSMASS +
                           DUSMASS + DUSMASS25 + OCSMASS + SO2SMASS+
                           SO4SMASS + SSSMASS + SSSMASS25 + blh_mean +
                           sp_mean + d2m_mean + t2m_mean + v10_mean + 
                           u10_mean + tp_mean + DEM + dayWeek,
                         data = train_data,  importance = TRUE)

# Mostrar la importancia de las variables
importance(rf_model)

# Predecir en el conjunto de testeo
predictions <- predict(rf_model, newdata = test_data)

# Calcular el error cuadrÃ¡tico medio (RMSE)
rmse <- sqrt(mean((predictions - test_data$PM25)^2))

# Calcular el coeficiente de determinaciÃ³n (RÂ²)
r_squared <- cor(predictions, test_data$PM25)^2

# Mostrar las mÃ©tricas de evaluaciÃ³n
cat("RMSE:", rmse, "/n")
cat("R²:", r_squared, "/n")

####################################################################
####################################################################
####################################################################
# Definir el control de entrenamiento con validaciÃ³n cruzada de 10 pliegues
train_control <- trainControl(method = "cv", number = 10)
train_control <- trainControl(
  method = "cv",          # Método de validación cruzada
  number = 10,            # Número de pliegues para la validación cruzada
  verboseIter = TRUE,     # Mostrar progreso de entrenamiento
  allowParallel = TRUE    # Permitir procesamiento paralelo
)
# Entrenar el modelo con validaciÃ³n cruzada
# rf_cv_model <- train(PM25 ~ AOD_055 + ndvi + LandCover + BCSMASS +
#                        DUSMASS + DUSMASS25 + OCSMASS + SO2SMASS+
#                        SO4SMASS + SSSMASS + SSSMASS25 + blh_mean +
#                        sp_mean + d2m_mean + t2m_mean + v10_mean + 
#                        u10_mean + tp_mean + DEM + dayWeek, data = train_data, 
#                      method = "rf", trControl = train_control,importance = TRUE)
# 
rf_cv_model <- train(log(PM25) ~ log(AOD_055) + log(ndvi) + log(LandCover) + log(BCSMASS_dia) +
                                log(DUSMASS_dia) + log(DUSMASS25_dia) + log(OCSMASS_dia) + log(SO2SMASS_dia)+
                                log(SO4SMASS_dia) + log(SSSMASS_dia) + log(SSSMASS25_dia) + log(blh_mean) +
                                log(sp_mean) + log(d2m_mean) + log(t2m_mean) + log(v10_mean) + log(u10_mean) +
                                log(tp_mean) + log(DEM) + log(dayWeek), data = train_data, method = "rf", 
                     trControl = train_control,importance = TRUE)
train_data <- train_data[complete.cases(train_data),]
test_data <- test_data[complete.cases(test_data),]
rf_cv_model <- train(PM25_Completo ~ AOD_055 + ndvi + LandCover + BCSMASS_dia +
                       DUSMASS_dia + DUSMASS25_dia + OCSMASS_dia + SO2SMASS_dia+
                      SO4SMASS_dia + SSSMASS_dia + SSSMASS25_dia + blh_mean +
                       sp_mean + d2m_mean + t2m_mean + v10_mean + u10_mean +
                       tp_mean + DEM + dayWeek, data = train_data, method = "rf", 
                     trControl = train_control,importance = TRUE)

# rf_cv_model <- train(PM25 ~ AOD_055 + ndvi  + BCSMASS_dia +
#                        DUSMASS_dia + DUSMASS25_dia + OCSMASS_dia + SO2SMASS_dia+
#                        SO4SMASS_dia + SSSMASS_dia + blh_mean +
#                        sp_mean + d2m_mean + t2m_mean + v10_mean +
#                        u10_mean + tp_mean + DEM + dayWeek, data = train_data,
#                      method = "rf", trControl = train_control,importance = TRUE)

rf_cv_model <- train(PM25 ~ AOD_055 + ndvi  +  blh_mean +
                       sp_mean + d2m_mean + t2m_mean + v10_mean +
                       u10_mean + tp_mean + DEM + dayWeek, data = train_data,
                     method = "rf", trControl = train_control,importance = TRUE)


11:47 -
print(rf_cv_model)
print(rf_cv_model$results)

# Ver el número de árboles generados
rf_cv_model$finalModel$ntree

# Mostrar la importancia de las variables
importance(rf_cv_model)

# Predecir en el conjunto de testeo
test_data$PM25 <- log(test_data$PM25)
predictions <- predict(rf_cv_model, newdata = test_data)
predictions_2 <- predict(rf_cv_model, newdata = test_data)
predictions_train <- predict(rf_cv_model, newdata = train_data)
a <- data.frame(predictions)
b <- data.frame(predictions_train)
c <- data.frame(predictions_2)
predicciones_hora<- predictions
## Importancia de las variables
importancia <- varImp(rf_cv_model, scale = TRUE)
print(importancia)
plot(importancia, main = "Importancia de Variables M1")

# Gráfico personalizado con ggplot2
importancia_df <- as.data.frame(importancia$importance)
importancia_df$Variable <- rownames(importancia_df)

ggplot(importancia_df, aes(x = reorder(Variable, Overall), y = Overall)) +
  
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = 50, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_classic()+
  
  labs(title = "Importancia de Variables RFM4", x = "Variables", y = "Importancia")


#### Metricas:
# postResample es una función del paquete caret en R que se utiliza para calcular
#métricas de rendimiento del modelo
# Calcular el error en el conjunto de entrenamiento
error_train <- postResample(predictions_train, train_data$PM25)

# Calcular el error en el conjunto de prueba
error_test <- postResample(predictions, test_data$PM25)

# Calcular el coeficiente de determinación (R²)
lm_r_squared_hora <- cor(predicciones_hora, test_data$PM25_Completo)^2
lm_r_squared_train <- cor(predictions_train, train_data$PM25_Completo)^2

# Calcular el coeficiente de Pearson
pearson_cor_hora <- cor(test_data$PM25_Completo, predicciones_hora, method = "pearson")

pearson_train <- cor(train_data$PM25, predictions_train, method = "pearson")

# Calcular el error cuadrático medio (RMSE)
lm_rmse_hora <- sqrt(mean((predicciones_hora - test_data$PM25)^2))
lm_rmse_train <- sqrt(mean((predictions_train - train_data$PM25)^2))

# 3. Calcular el MAE (Mean Absolute Error)
mae_hora <- mean(abs(test_data$PM25 - predicciones_hora))
mae_train <- mean(abs(train_data$PM25 - predictions_train))

# 4. Calcular el MAPE (Mean Absolute Percentage Error)
mape_hora <- mean(abs((test_data$PM25 - predicciones_hora) / test_data$PM25)) * 100
mape_train <- mean(abs((train_data$PM25 - predictions_train) / train_data$PM25)) * 100

# 5. Calcular el MSE (Mean Squared Error)
mse_hora <- mean((test_data$PM25 - predicciones_hora)^2)
mse_train <- mean((train_data$PM25 - predictions_train)^2)

# 6. Calcular el MedAE (Median Absolute Error)
medae_hora <- median(abs(test_data$PM25 - predicciones_hora))
medae_train <- median(abs(train_data$PM25 - predictions_train))

### Test
print(paste("R2 test:", round(lm_r_squared_hora, 2)))
print(paste("R pearson test:", round(pearson_cor_hora, 2)))
print(paste("RMSE test:", round(lm_rmse_hora, 2)))
print(paste("mae test:", round(mae_hora, 2)))
print(paste("MAPE test:", round(mape_hora, 2), "%"))
print(paste("MSE test:", round(mse_hora, 2)))
print(paste("MedAE test:", round(medae_hora,2)))
min(predictions)
max(predictions)
## Train
print(paste("R2 train:", round(lm_r_squared_train, 2)))
print(paste("R pearson train:", round(pearson_train, 2)))
print(paste("RMSE train:", round(lm_rmse_train, 2)))
print(paste("mae train:", round(mae_train, 2)))
print(paste("MAPE train:", round(mape_train, 2), "%"))
print(paste("MSE train:", round(mse_train, 2)))
print(paste("MedAE train:", round(medae_train,2)))
min(predictions_train)
max(predictions_train)
View(b)
# Guardar el modelo entrenado
getwd()
setwd("D:/Josefina/Proyectos/ProyectoChile/modelos/modelo")

save(rf_cv_model, file="02-RF_cv_M6-281024.RData")


print("Modelo Random Forest entrenado y guardado en 'random_forest_model.RData'.")
################################################################################
################################################################################
# cargar el modelo y aplicalo a otro set de datos
setwd("D:/Josefina/Proyectos/ProyectoChile/modelos/modelo")

# Paso 1: Cargar el modelo
load("02-RF_cv_M6-281024.RData")
load("01-RF_cv_M1-050924.RData")
load("01-RF_cv_M2-050924.RData")
load("01-RF_cv_M3-050924.RData")
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
week_day <- raster(paste(dir_tiff,"weekDay_raster.tif",sep=""))

# Generamos stack
r_stack <- stack(maiac,NDVI,LandCover,BCSMASS ,
                   DUSMASS,DUSMASS25, OCSMASS,SO2SMASS,
                   SO4SMASS,SSSMASS ,SSSMASS25,blh_mean,
                   sp_mean , d2m_mean, t2m_mean,v10_mean, 
                   u10_mean ,tp_mean ,DEM,week_day)
plot(r_stack)

r_stack_df <- as.data.frame(r_stack, na.rm = TRUE)
names(r_stack_df) <- c("AOD_055", "ndvi", "LandCover", "BCSMASS","DUSMASS",
                       "DUSMASS25","OCSMASS", "SO2SMASS","SO4SMASS","SSSMASS",
                       "SSSMASS25","blh_mean", "sp_mean" , "d2m_mean", "t2m_mean",
                       "v10_mean","u10_mean" ,"tp_mean","DEM","dayWeek")
# Aplicar el modelo de Random Forest al data frame
predictions <- predict(rf_cv_model, newdata = r_stack_df)
A<- data.frame(predictions)
# Crear un raster vac?o con la misma extensi?n y resoluci?n que el stack
pred_raster <- raster(r_stack)

# Asignar las predicciones al raster
pred_raster[] <- NA  # Inicia con valores NA

# Reinsertar las predicciones en las celdas correspondientes
pred_raster[!is.na(values(r_stack[[1]]))] <- predictions

getwd()

writeRaster(pred_raster, filename = "D:/Josefina/Proyectos/ProyectoChile/modelos/SalidaModelo/01-RFM4_cv_060924.tif", format = "GTiff", overwrite = TRUE)




