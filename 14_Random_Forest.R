# Cargar los paquetes necesarios
library(randomForest)
library(raster)
library(caret) #version 4.2.3
rm(list=ls())

#Data modelo 1
#test_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 1/M1_test.csv")
#train_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 1/M1_train.csv")

#Data modelo 2
test_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 2/M2_test.csv")
train_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 2/M2_train.csv")

#Data modelo 3
test_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 3/M3_test.csv")
train_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 3/M3_train.csv")


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

# Entrenar el modelo con validaciÃ³n cruzada
rf_cv_model <- train(PM25 ~ AOD_055 + ndvi + LandCover + BCSMASS +
                       DUSMASS + DUSMASS25 + OCSMASS + SO2SMASS+
                       SO4SMASS + SSSMASS + SSSMASS25 + blh_mean +
                       sp_mean + d2m_mean + t2m_mean + v10_mean + 
                       u10_mean + tp_mean + DEM + dayWeek, data = train_data, 
                     method = "rf", trControl = train_control,importance = TRUE)

13:39
print(rf_cv_model)
print(rf_cv_model$results)

# Mostrar la importancia de las variables
importance(rf_cv_model)

# Predecir en el conjunto de testeo
predictions <- predict(rf_cv_model, newdata = test_data)

# Calcular el error cuadrÃ¡tico medio (RMSE)
rmse <- sqrt(mean((predictions - test_data$PM25)^2))

# Calcular el coeficiente de determinaciÃ³n (RÂ²)
r_squared <- cor(predictions, test_data$PM25)^2

# Mostrar las mÃ©tricas de evaluaciÃ³n
cat("RMSE:", rmse, "/n")
cat("R²:", r_squared, "/n")

# Guardar el modelo entrenado
getwd()
setwd("D:/Josefina/Proyectos/ProyectoChile/modelos/modelo")

save(rf_model, file="01-RF_260824.RData")
save(rf_cv_model, file="01-RF_cv_M3-2908204.RData")


print("Modelo Random Forest entrenado y guardado en 'random_forest_model.RData'.")
################################################################################
################################################################################
# cargar el modelo y aplicalo a otro set de datos

# Paso 1: Cargar el modelo
load("01-RF_cv_260824.RData")

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
plot(r_stack)

r_stack_df <- as.data.frame(r_stack, na.rm = TRUE)
names(r_stack_df) <- c("AOD_055", "ndvi", "LandCover", "BCSMASS","DUSMASS",
                       "DUSMASS25","OCSMASS", "SO2SMASS","SO4SMASS","SSSMASS",
                       "SSSMASS25","blh_mean", "sp_mean" , "d2m_mean", "t2m_mean",
                       "v10_mean","u10_mean" ,"tp_mean","DEM")
# Aplicar el modelo de Random Forest al data frame
predictions <- predict(rf_cv_model, newdata = r_stack_df)

# Crear un raster vac?o con la misma extensi?n y resoluci?n que el stack
pred_raster <- raster(r_stack)

# Asignar las predicciones al raster
pred_raster[] <- NA  # Inicia con valores NA

# Reinsertar las predicciones en las celdas correspondientes
pred_raster[!is.na(values(r_stack[[1]]))] <- predictions

getwd()

writeRaster(pred_raster, filename = "D:/Josefina/Proyectos/ProyectoChile/modelos/SalidaModelo/01-RF_cv_260824.tif", format = "GTiff", overwrite = TRUE)




