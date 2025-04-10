# Cargar los paquetes necesarios
library(randomForest)
library(raster)
library(caret) #version 4.2.3
rm(list=ls())

estacion <- "MX"
modelo <- "1"
#Data modelo 1
test_data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))
train_data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
nrow(test_data)+nrow(train_data)
unique(year(test_data$date))
# Entrenar el modelo de Random Forest

estacion <- "MX"
modelo<- 6
data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_",modelo,"/",estacion,"_merge_comp.csv",sep=""))
#data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 7/09_TOT_merge_tot.csv")
data <- data[complete.cases(data), ]
data$date <- strptime(data$date, format = "%Y-%m-%d")#"%d/%m/%Y")#
data$dayWeek <- wday(data$date, week_start = 1)
data<- data[year(data$date) != 2024,]

test_data <- data[data$ID==16,]
train_data <- data[data$ID!=16,]
nrow(test_data)/(nrow(test_data)+nrow(train_data))



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


rf_cv_model <- train(PM25 ~ AOD_055 + ndvi + #LandCover +
                       BCSMASS_dia +DUSMASS_dia + DUSMASS25_dia + OCSMASS_dia
                     + SO2SMASS_dia+
                      SO4SMASS_dia + SSSMASS_dia + SSSMASS25_dia +
                       blh_mean +
                       sp_mean + d2m_mean + t2m_mean +   v10_mean +
                       u10_mean +
                       tp_mean + DEM + dayWeek, data = train_data, method = "rf",
                     trControl = train_control,importance = TRUE)
08:052
#########
## Sin variables MERRA
# rf_cv_model <- train(PM25 ~ AOD_055 + ndvi + 
#                        blh_mean +
#                        sp_mean + d2m_mean + t2m_mean + v10_mean + u10_mean +
#                        tp_mean + DEM + dayWeek, data = train_data, method = "rf", 
#                      trControl = train_control,importance = TRUE)

print(rf_cv_model)
print(rf_cv_model$results)

# Ver el número de árboles generados
rf_cv_model$finalModel$ntree

# Mostrar la importancia de las variables
importance(rf_cv_model)

# Predecir en el conjunto de testeo
test_data$PM25 <- log(test_data$PM25)


predictions <- predict(rf_cv_model, newdata = test_data)
predictions_train <- predict(rf_cv_model, newdata = train_data)
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
lm_r_squared_hora <- cor(predicciones_hora, test_data$PM25)^2
lm_r_squared_train <- cor(predictions_train, train_data$PM25)^2


# Calcular el coeficiente de Pearson
pearson_cor_hora <- cor(test_data$PM25, predicciones_hora, method = "pearson")
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

#setwd("D:/Josefina/Proyectos/ProyectoChile/modelos/modelo")
setwd(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/modelo",sep=""))
getwd()
save(rf_cv_model, file=paste("07-RF_esp_cv_M",modelo,"-180225-",estacion,".RData",sep=""))

# SIN MERRA
#save(rf_cv_model, file=paste("06-RF_cv_M",modelo,"-090125-E_",estacion,".RData",sep=""))

print("Modelo Random Forest entrenado y guardado en 'random_forest_model.RData'.")
################################################################################
################################################################################
# cargar el modelo y aplicalo a otro set de datos
estacion<- "MD"
setwd(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/modelo",sep = ""))

# Paso 1: Cargar el modelo
load("02-RF_cv_M3-261224_MX.RData")
load("01-RF_cv_M1-050924.RData")
load("01-RF_cv_M2-050924.RData")
load("01-RF_cv_M3-050924.RData")
load("02-RF_cv_M1-080125_MD.RData")
load("prueba_02-RF_cv_M1-120225-CH.RData")
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


#############################################################
###############################################################

estacion <- "MD"
modelo <- "6"
#Data modelo 1
data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_",modelo,"/",estacion,"_merge_comp.csv",sep=""))
#data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 7/09_TOT_merge_tot.csv")
data <- data[complete.cases(data), ]
data$date <- strptime(data$date, format = "%Y-%m-%d")#"%d/%m/%Y")#
data$dayWeek <- wday(data$date, week_start = 1)
data<- data[year(data$date) != 2024,]

test_data <- data[data$ID == 84,]
train_data <- data[data$ID != 84,]

test_data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))
train_data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))


nrow(test_data)+nrow(train_data) == nrow(data)
nrow(test_data)/ nrow(data)
nrow(train_data)/ nrow(data)
unique(year(data$date))


# cargar el modelo y aplicalo a otro set de datos
setwd(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/modelo",sep=""))

# Paso 1: Cargar el modelo
load("07-RF-ESP_cv_M6-110225-SP.RData")
load("02-RF_cv_M1-080125_MD.RData")
load("07-RF-ESP_cv_M6-110225-MD.RData")

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
  
  labs(title = "Importancia de Variables 07-RF-ESP_cv_M6-110225-MD", x = "Variables", y = "Importancia")

####################################
######################################

y_train_pred <- predict(rf_cv_model, newdata = train_data)
y_test_pred <- predict(rf_cv_model, newdata = test_data)
# Calcular los residuos
residuals_train <- train_data$PM25 - y_train_pred
residuals_test <- test_data$PM25 - y_test_pred
# Crear un dataframe con los valores predichos y los residuos
data_test <- data.frame(predicted = y_test_pred,real=test_data$PM25, residuals = residuals_test)
data_train <- data.frame(predicted = y_train_pred, real=train_data$PM25,residuals = residuals_train)

################
# Calcular los residuos
residuals_train <- train_data$PM25 - y_train_pred
residuals_test <- test_data$PM25 - y_test_pred


# Crear un dataframe con los valores predichos y los residuos
data_test <- data.frame(predicted = y_test_pred,real=test_data$PM25, residuals = residuals_test)
data_train <- data.frame(predicted = y_train_pred, real=train_data$PM25,residuals = residuals_train)

###-- 
plot_residuos_test <- ggplot(data_test, aes(x = predicted, y = residuals)) +
  geom_point(color = "#2c7fb8",size=2,alpha=0.5) + # Puntos en azul
  geom_hline(yintercept = 0, color = "#de2d26", linetype = "dashed", size = 0.7) + # Línea horizontal
  #ggtitle("Test") +
  xlab("PM2.5 Observado") +
  ylab("Residuos") +
  
  #scale_x_continuous(limits = c(0, 145),breaks = seq(0, 140, by = 20)) +  # Ticks cada 10 en el eje X
  scale_x_continuous(limits = c(0, 105),breaks = seq(0, 100, by = 20)) +  # Ticks cada 10 en el eje X
  scale_y_continuous(limits = c(-50, 50),breaks = seq(-50, 50, by = 10)) +  # Ticks cada 10 en el eje Y
  theme_classic()+
  theme(
    plot.title = element_text(size = 10),  # Tamaño del título
    axis.title.x = element_text(size = 8),  # Tamaño etiqueta eje X
    axis.title.y = element_text(size = 8)   # Tamaño etiqueta eje Y
  )

plot_residuos_train <- ggplot(data_train, aes(x = predicted, y = residuals)) +
  geom_point(color = "#31a354",size=2,alpha=0.5) + # Puntos en azul
  geom_hline(yintercept = 0, color = "#de2d26", linetype = "dashed", size = 0.7) + # Línea horizontal
  #ggtitle("Train") +
  xlab("Valores predichos (Train)") +
  # scale_x_continuous(limits = c(0, 145),breaks = seq(0, 140, by = 20)) +  # Ticks cada 10 en el eje X
  scale_x_continuous(limits = c(0, 105),breaks = seq(0, 100, by = 20)) +  # Ticks cada 10 en el eje X
  scale_y_continuous(limits = c(-50, 50),breaks = seq(-50, 50, by = 10)) +  # Ticks cada 10 en el eje Y
  
  ylab("Residuos") +
  theme_classic()+
  theme(
    plot.title = element_text(size = 10),  # Tamaño del título
    axis.title.x = element_text(size = 8),  # Tamaño etiqueta eje X
    axis.title.y = element_text(size = 8)   # Tamaño etiqueta eje Y
  )
plot_residuos_test
plot_residuos_train


# Combinar los gráficos en una fila y dos columnas
combinacion_residuos<- grid.arrange(plot_residuos_train, plot_residuos_test, 
                                    nrow = 1, ncol = 2, top = "02-RLM_cv_M1-141024")



########################################################
##### ---- Gráfico de dispersión de Predicciones vs Valores Reales
#muestra cómo se alinean las predicciones del modelo SVR con los valores
#reales. La línea y = x sirve como referencia para una predicción perfecta.

###-- 

plot_y_test <- ggplot(data_test, aes(x = real, y = predicted)) +
  geom_point(color = "#2c7fb8",size=2,alpha=0.5) + # Puntos en azul
  geom_abline(intercept = 0, slope=1,color = "#de2d26", linetype = "dashed", size = 0.7) + # Línea horizontal
  #ggtitle("Test") +
  xlab("PM2.5 Observado") +
  ylab("Predicción") +
  
  scale_x_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 20)) +  # Ticks cada 10 en el eje X
  scale_y_continuous(limits = c(0,100),breaks = seq(0, 100, by = 20)) +   # Ticks cada 10 en el eje Y
  theme_classic()+
  theme(
    plot.title = element_text(size = 10),  # Tamaño del título
    axis.title.x = element_text(size = 8),  # Tamaño etiqueta eje X
    axis.title.y = element_text(size = 8)   # Tamaño etiqueta eje Y
  )

plot_y_train <- ggplot(data_train, aes(y = predicted, x = real)) +
  geom_point(color = "#31a354",size=2,alpha=0.5) + # Puntos en azul
  geom_abline(intercept = 0, slope=1, color = "#de2d26", linetype = "dashed", size = 0.7) + # Línea horizontal
  ggtitle("Train") +
  
  scale_x_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 20)) +  # Ticks cada 10 en el eje X
  scale_y_continuous(limits = c(0,100),breaks = seq(0, 100, by = 20)) +   # Ticks cada 10 en el eje Y
  
  xlab("Observed") +
  ylab("Predicted") +
  theme_classic()+
  theme(
    plot.title = element_text(size = 10),  # Tamaño del título
    axis.title.x = element_text(size = 8),  # Tamaño etiqueta eje X
    axis.title.y = element_text(size = 8)   # Tamaño etiqueta eje Y
  )
plot_y_test
plot_y_train


# Combinar los gráficos en una fila y dos columnas
combinacion_valores<- grid.arrange(plot_y_train, plot_y_test, 
                                   nrow = 1, ncol = 2, top = "02-RLM_cv_M1-141024")

# Combinar los gráficos en una fila y dos columnas
combinacion_valores<- grid.arrange(plot_y_test, plot_residuos_test, 
                                   nrow = 1, ncol = 2, top = "07-RF-ESP_cv_M6-110225-MD")
