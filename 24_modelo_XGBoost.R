# Cargar librerías necesarias
library(xgboost)
library(Matrix)

# Supongamos que tienes un dataframe llamado 'train_data' con la variable objetivo 'PM25'
# y las características que mencionaste.
rm(list=ls())
estacion <- "BA"
modelo <- "1"
#Data modelo 1
test_data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))
train_data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))

# Supongamos que tienes un dataframe llamado 'train_data' con la variable objetivo 'PM25'
# y las características que mencionaste.

# Separar las características y la variable objetivo
X <- train_data[ , c("AOD_055", "ndvi", "BCSMASS", "DUSMASS", "DUSMASS25",
                     "OCSMASS", "SO2SMASS", "SO4SMASS", "SSSMASS",
                     "SSSMASS25", "blh_mean", "sp_mean", "d2m_mean",
                     "t2m_mean", "v10_mean", "u10_mean", "tp_mean", "DEM")]
y <- train_data$PM25


# Convertir a matrices xgboost
dtrain <- xgb.DMatrix(data = as.matrix(X), label = y)

# Especificar los parámetros del modelo
params <- list(
  booster = "gbtree", 
  objective = "reg:squarederror",  # Tarea de regresión
  eval_metric = "rmse",             # Métrica para evaluación
  eta = 0.3,      #0.1chat                  # Tasa de aprendizaje
  max_depth = 6,                    # Profundidad máxima de los árboles
  gamma = 0,                        # Regularización L2
  subsample = 0.8,                  # Proporción de datos para entrenamiento
  colsample_bytree = 1, #0.8 chat           # Proporción de características para entrenamiento
  min_child_weight = 1 
  )

# Ajustar el modelo
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 2000#,      #2000    # Número de rondas de boosting
  #early_stopping_rounds = 10  # Detener el entrenamiento si no mejora
)
08:56 09:02
# Preparar datos de prueba

X_test <- test_data[ , c("AOD_055", "ndvi", "BCSMASS", "DUSMASS", "DUSMASS25",
                     "OCSMASS", "SO2SMASS", "SO4SMASS", "SSSMASS",
                     "SSSMASS25", "blh_mean", "sp_mean", "d2m_mean",
                     "t2m_mean", "v10_mean", "u10_mean", "tp_mean", "DEM")]
y_test<- test_data$PM25


dtest <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)

# Realizar predicciones
predictions <- predict(xgb_model, dtest)
predictions[predictions<0]
# Evaluar el modelo con set de datos de testeo
actuals <- y_test
predicted <- predictions

# Calcular métricas de rendimiento
mse <- mean((actuals - predicted)^2)
rmse <- sqrt(mse)
mae <- mean(abs(actuals - predicted))
r2 <- cor(predicted, actuals)^2
r <- cor(actuals, predicted, method = "pearson")
mape <- mean(abs((actuals - predicted) / actuals)) * 100
medae <- median(abs(actuals - predicted))
# Imprimir métricas
cat("R2: ", round(r2,2), "\n")
cat("R: ", round(r,2), "\n")
cat("RMSE: ", round(rmse,2), "\n")
cat("MAE: ", round(mae,2), "\n")
cat("MAPE: ", round(mape,2), "%\n")
cat("MSE: ", round(mse,2), "\n")
cat("MedAE: ", round(medae,2), "\n")





# Realizar predicciones sobre el conjunto de entrenamiento
train_predictions <- predict(xgb_model, dtrain)

# Evaluar el modelo en el conjunto de entrenamiento
train_actuals <- y
train_predicted <- train_predictions
train_mse <- mean((train_actuals - train_predicted)^2)
train_rmse <- sqrt(train_mse)
train_mae <- mean(abs(train_actuals - train_predicted))
train_r2 <- cor(train_predicted, train_actuals)^2
train_r <- cor(train_actuals, train_predicted, method = "pearson")
cat("Training R2: ", round(train_r2,3), "\n")
cat("Training R: ", round(train_r,3), "\n")
cat("Training MSE: ", round(train_mse,3), "\n")
cat("Training RMSE: ", round(train_rmse,3), "\n")
cat("Training MAE: ", round(train_mae,3), "\n")

# gUARDAMOS MODELO
# Sin cv
setwd("D:/Josefina/Proyectos/ProyectoChile/modelos/modelo")

save(xgb_model, file="01-XGB_M4-100924.RData")
load("03-XGB_cv_M2-071024.RData")
#################################################################
####################################################################
#################################################################
#Modelo pero con cross validation

library(xgboost)
library(caret)  # Para el cálculo de métricas

rm(list=ls())
estacion <- "BA"
modelo <- "3"
#Data modelo 1
test_data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))
train_data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))


names(test_data) <- c("X.1" ,"X" ,"ID", "date", "estacion","PM25","AOD_055",                 
                      "ndvi" ,  "BCSMASS_dia",                  
                      "DUSMASS_dia" ,"DUSMASS25_dia", "OCSMASS_dia", "SO2SMASS_dia" ,"SO4SMASS_dia",
                      "SSSMASS_dia", "SSSMASS25_dia",                 
                      "blh_mean","blh_min","blh_max"  ,"blh_sd","blh_mean_subt", "sp_mean",                  
                      "sp_min", "sp_max","sp_sd"   ,"sp_mean_subt","d2m_mean","d2m_min",                  
                      "d2m_max","d2m_sd" , "d2m_mean_subt",  "t2m_mean", "t2m_min", "t2m_max",                  
                      "t2m_sd",  "t2m_mean_subt", "v10_mean",  "v10_min" , "v10_max" ,"v10_sd"  ,                 
                      "v10_mean_subt", "u10_mean" , "u10_min"  , "u10_max","u10_sd",
                      "u10_mean_subt","tp_mean", "tp_min","tp_max",  "tp_sd", "tp_mean_subt", "DEM" , "dayWeek")

names(train_data) <- c("X.1" ,"X" ,"ID", "date", "estacion","PM25","AOD_055",                 
                       "ndvi" ,  "BCSMASS_dia",                  
                       "DUSMASS_dia" ,"DUSMASS25_dia", "OCSMASS_dia", "SO2SMASS_dia" ,"SO4SMASS_dia",
                       "SSSMASS_dia",  "SSSMASS25_dia",                  
                       "blh_mean","blh_min","blh_max"  ,"blh_sd","blh_mean_subt", "sp_mean",                  
                       "sp_min", "sp_max","sp_sd"   ,"sp_mean_subt","d2m_mean","d2m_min",                  
                       "d2m_max","d2m_sd" , "d2m_mean_subt",  "t2m_mean", "t2m_min", "t2m_max",                  
                       "t2m_sd",  "t2m_mean_subt", "v10_mean",  "v10_min" , "v10_max" ,"v10_sd"  ,                 
                       "v10_mean_subt", "u10_mean" , "u10_min"  , "u10_max","u10_sd",
                       "u10_mean_subt","tp_mean", "tp_min","tp_max",  "tp_sd", "tp_mean_subt", "DEM" , "dayWeek")



# Preparar los datos
# X <- train_data[ , c("AOD_055", "ndvi","LandCover", "BCSMASS", "DUSMASS", "DUSMASS25",
#                      "OCSMASS", "SO2SMASS", "SO4SMASS", "SSSMASS",
#                      "SSSMASS25", "blh_mean", "sp_mean", "d2m_mean",
#                      "t2m_mean", "v10_mean", "u10_mean", "tp_mean", "DEM","dayWeek")]

X <- train_data[ , c("AOD_055", "ndvi", "BCSMASS_dia", "DUSMASS_dia", "DUSMASS25_dia",
                     "OCSMASS_dia", "SO2SMASS_dia", "SO4SMASS_dia", "SSSMASS_dia",
                     "SSSMASS25_dia", "blh_mean", "sp_mean", "d2m_mean",
                     "t2m_mean", "v10_mean", "u10_mean", "tp_mean", "DEM","dayWeek")] #"LandCover",
#A. Sacamos landCover, DEM, SSSMASS_dia
#B. Sacamos landCover, DEM
#C. Sacamos landCover, SSSMASS_dia
#D. Sacamos landCover, SSSMASS25_dia
#E. Sacamos landCover, U10

# X <- train_data[ , c("AOD_055", "ndvi","LandCover", "blh_mean", "sp_mean", "d2m_mean",
#                      "t2m_mean", "v10_mean", "u10_mean", "tp_mean", "DEM","dayWeek")]

y <- train_data$PM25
# Para BA
y <- train_data$PM25_hora
# Convertir a matrices xgboost
dtrain <- xgb.DMatrix(data = as.matrix(X), label = y)

# Configurar los parámetros del modelo
params <- list(
  booster = "gbtree",
  objective = "reg:squarederror",  # Tarea de regresión
  eval_metric = "rmse",             # Métrica para evaluación
  eta = 0.3,                       # Tasa de aprendizaje
  max_depth = 6,                   # Profundidad máxima de los árboles
  gamma = 0,                       # Regularización L2
  subsample = 0.8,                 # Proporción de datos para entrenamiento
  colsample_bytree = 1,            # Proporción de características para entrenamiento
  min_child_weight = 1
)

# Realizar validación cruzada
#Esto es lo que mas tarda!! igual en comparacion con rf tarda mucho menos
# porque?
cv_results  <- xgb.cv(
  params = params,
  data = dtrain,
  nrounds = 2000,                   # Número de rondas de boosting
  nfold = 10,                        # Número de pliegues para la validación cruzada
  early_stopping_rounds = 20,       # Detener si no mejora
  verbose = TRUE                    # Mostrar progreso
)


# Obtener el número óptimo de rondas
best_nrounds <- cv_results$best_iteration

# Ajustar el modelo con el número óptimo de rondas
xgb_cv_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = best_nrounds
)

09:51
# # Preparar datos de prueba
# X_test <- test_data[ , c("AOD_055", "ndvi","LandCover", "BCSMASS", "DUSMASS", "DUSMASS25",
#                          "OCSMASS", "SO2SMASS", "SO4SMASS", "SSSMASS",
#                          "SSSMASS25", "blh_mean", "sp_mean", "d2m_mean",
#                          "t2m_mean", "v10_mean", "u10_mean", "tp_mean", "DEM","dayWeek")]
# X_test <- test_data[ , c("AOD_055", "ndvi","LandCover", "BCSMASS_dia", "DUSMASS_dia", "DUSMASS25_dia",
#                          "OCSMASS_dia", "SO2SMASS_dia", "SO4SMASS_dia", "SSSMASS_dia",
#                          "SSSMASS25_dia", "blh_mean", "sp_mean", "d2m_mean",
#                          "t2m_mean", "v10_mean", "u10_mean", "tp_mean", "DEM","dayWeek")]
# 

X_test <- test_data[ , c("AOD_055", "ndvi","BCSMASS_dia", "DUSMASS_dia", "DUSMASS25_dia",
                     "OCSMASS_dia", "SO2SMASS_dia", "SO4SMASS_dia", "SSSMASS_dia",
                     "SSSMASS25_dia", "blh_mean", "sp_mean", "d2m_mean",
                     "t2m_mean", "v10_mean", "u10_mean", "tp_mean", "DEM","dayWeek")]#"LandCover", 

#
y_test <- test_data$PM25
## PARA BA
y_test <- test_data$PM25_hora
dtest <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)

# Realizar predicciones sobre el conjunto de prueba
predictions <- predict(xgb_cv_model, dtest)
#test_preds <- exp(predictions) # para log



# Evaluar el modelo con el conjunto de prueba
actuals <- y_test
predicted <- predictions
# predicted <- test_preds #para log
# Calcular métricas de rendimiento
mse <- mean((actuals - predicted)^2)
rmse <- sqrt(mse)
mae <- mean(abs(actuals - predicted))
r2 <- cor(predicted, actuals)^2
r <- cor(actuals, predicted, method = "pearson")
mape <- mean(abs((actuals - predicted) / actuals)) * 100
medae <- median(abs(actuals - predicted))





# Imprimir métricas
cat("R2: ", round(r2,2), "\n")
cat("R: ", round(r,2), "\n")
cat("RMSE: ", round(rmse,2), "\n")
cat("MAE: ", round(mae,2), "\n")
cat("MAPE: ", round(mape,2), "%\n")
cat("MSE: ", round(mse,2), "\n")
cat("MedAE: ", round(medae,2), "\n")
min(predicted)
max(predicted)
0.# Realizar predicciones sobre el conjunto de entrenamiento
train_predictions <- predict(xgb_cv_model, dtrain)
# train_predictions <- exp(train_predictions) # para log

# Evaluar el modelo en el conjunto de entrenamiento
train_actuals <- y
train_predicted <- train_predictions
train_mse <- mean((train_actuals - train_predicted)^2)
train_rmse <- sqrt(train_mse)
train_mae <- mean(abs(train_actuals - train_predicted))
train_r2 <- cor(train_predicted, train_actuals)^2
train_r <- cor(train_actuals, train_predicted, method = "pearson")
train_mape <- mean(abs((train_actuals - train_predicted) / train_actuals)) * 100
train_medae <- median(abs(train_actuals - train_predicted))
# Imprimir métricas para el conjunto de entrenamiento
cat("Training R2: ", round(train_r2,2), "\n")
cat("Training R: ", round(train_r,2), "\n")
cat("Training RMSE: ", round(train_rmse,2), "\n")
cat("Training MAE: ", round(train_mae,2), "\n")
cat("Training MAPE: ", round(train_mape,2), "%\n")
cat("Training MSE: ", round(train_mse,2), "\n")
cat("Training MedAE: ", round(train_medae,2), "\n")
min(train_predictions)
max(train_predictions)
# gUARDAMOS MODELO

setwd(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/modelo",sep=""))
getwd()
save(xgb_cv_model, file=paste("02-XGB-M",modelo,"_100125",estacion,".RData",sep=""))


##################################################################
###################################################################

#rm(list=ls())
#df_rbind <- data.frame()
# Suponiendo que mi_dataframe es el objeto que no quieres eliminar
rm(list = setdiff(ls(), "df_rbind"))
setwd("D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/Prediccion_01-2024/tiff/")
dir_salida <- "D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/Prediccion_01-2024/Salida/Salida_01-XGB_cv_M1-100924/"
fechaInteres <- as.Date("31-01-2024", format = "%d-%m-%Y")
#Modelos
dir_modelos <- "D:/Josefina/Proyectos/ProyectoChile/modelos/modelo/"
#load(paste(dir_modelos,"01-RF_cv_M1-050924.RData",sep=""))
#load(paste(dir_modelos,"01-RF_cv_M2-050924.RData",sep=""))
#load(paste(dir_modelos,"01-RF_cv_M3-050924.RData",sep=""))
# load(paste(dir_modelos,"01-RF_cv_M4-060924.RData",sep=""))
# load(paste(dir_modelos,"02-RF_cv_M1-090924.RData",sep=""))
load(paste(dir_modelos,"01-XGB_cv_M3-100924.RData",sep=""))
for (i in 1:1){
  
  ################# -----     00 MAIAC     -----
  # 2024001-MAIAC_raster.tif
  # Convertir a día juliano respecto al 1 de enero del mismo año
  dayJulian <- as.numeric(fechaInteres - as.Date(paste0(format(fechaInteres, "%Y"), "-01-01"))) + 1
  yearInteres <- year(fechaInteres)
  if(nchar(dayJulian)==1){
    sep = "00"
  }
  if (nchar(dayJulian)==2) {
    sep = "0"
  }
  
  if (nchar(dayJulian)==3) {
    sep = ""
  }
  maiacDate <- paste(yearInteres,sep,dayJulian,sep = "")
  MAIAC_raster <- raster(paste("00_MAIAC/00_MAIAC_IDW/IDW-",maiacDate,"-MAIAC_raster.tif",sep=""))
  plot(MAIAC_raster)
  
  ################### -----     NDVI     -----
  # Como es modis tambien la fecha es juliana pero ojo es un dato mensual
  fechaNDVI<- as.Date("01-01-2024", format = "%d-%m-%Y")
  yearNdvi <- year(fechaNDVI)
  dayJulianNDVI <- as.numeric(fechaNDVI - as.Date(paste0(format(fechaNDVI, "%Y"), "-01-01"))) + 1
  
  if(nchar(dayJulianNDVI)==1){
    sep = "00"
  }
  if (nchar(dayJulianNDVI)==2) {
    sep = "0"
  }
  
  if (nchar(dayJulianNDVI)==3) {
    sep = ""
  }
  NDVIDate <- paste(yearInteres,sep,dayJulianNDVI,sep = "")
  
  NDVI_raster <- raster(paste("01_NDVI/",NDVIDate,"-NDVI_raster.tif",sep=""))
  plot(NDVI_raster)
  
  ################# -----     LandCover     -----
  #Este dato es anual por lo que tambien tenemos que setear la fecha dierente
  # Pero es MODIS = Dia juliano
  # MCD12Q1.A2024001.h12v12.061.2022169161028.hdf
  
  
  yearLandCover <- year(fechaNDVI)
  LandCoverDate <- paste(yearLandCover,"001",sep = "")
  LandCover_raster <- raster(paste("02_LandCover/",LandCoverDate,"-LandCover_raster.tif",sep=""))
  
  plot(LandCover_raster)
  
  ################# -----     DEM     -----
  
  DEM_raster <- raster("03_DEM/DEM_raster.tif")
  plot(DEM_raster)
  ################# -----     MERRA-2     -----
  fechaInteres_MERRA <- gsub("-", "", fechaInteres)
  
  ## BCSMASS
  BCSMASS_raster <- raster(paste("04_MERRA-2/",fechaInteres_MERRA,"-BCSMASS_raster.tif",sep=""))
  #plot(BCSMASS_raster)
  
  ## DMSSMASS
  DMSSMASS_raster <- raster(paste("04_MERRA-2/",fechaInteres_MERRA,"-DMSSMASS_raster.tif",sep=""))
  #plot(DMSSMASS_raster)
  
  ## DUSMASS
  DUSMASS_raster <- raster(paste("04_MERRA-2/",fechaInteres_MERRA,"-DUSMASS_raster.tif",sep=""))
  #plot(DUSMASS_raster)
  
  ## DUSMASS25
  DUSMASS25_raster <- raster(paste("04_MERRA-2/",fechaInteres_MERRA,"-DUSMASS25_raster.tif",sep=""))
  #plot(DUSMASS25_raster)
  
  ## OCSMASS
  OCSMASS_raster <- raster(paste("04_MERRA-2/",fechaInteres_MERRA,"-OCSMASS_raster.tif",sep=""))
  #plot(OCSMASS_raster)
  
  ## SO2SMASS
  SO2SMASS_raster <- raster(paste("04_MERRA-2/",fechaInteres_MERRA,"-SO2SMASS_raster.tif",sep=""))
  #plot(SO2SMASS_raster)
  
  ## SO4SMASS
  SO4SMASS_raster <- raster(paste("04_MERRA-2/",fechaInteres_MERRA,"-SO4SMASS_raster.tif",sep=""))
  #plot(SO4SMASS_raster)
  
  ## SSSMASS
  SSSMASS_raster <- raster(paste("04_MERRA-2/",fechaInteres_MERRA,"-SSSMASS_raster.tif",sep=""))
  #plot(SSSMASS_raster)
  
  ## SSSMASS25
  SSSMASS25_raster <- raster(paste("04_MERRA-2/",fechaInteres_MERRA,"-SSSMASS25_raster.tif",sep=""))
  #plot(SSSMASS25_raster)
  
  ################# -----     ERA5     -----
  ## BLH     -----
  BLH_raster <- raster(paste("05_ERA5/",fechaInteres,"-BLH_raster.tif",sep=""))
  #plot(BLH_raster)
  
  ## D2M
  D2M_raster <- raster(paste("05_ERA5/",fechaInteres,"-D2M_raster.tif",sep=""))
  #plot(D2M_raster)
  
  ## T2M
  T2M_raster <- raster(paste("05_ERA5/",fechaInteres,"-T2M_raster.tif",sep=""))
  #plot(T2M_raster)
  
  ## TP
  TP_raster <- raster(paste("05_ERA5/",fechaInteres,"-TP_raster.tif",sep=""))
  #plot(TP_raster)
  
  ## SP
  SP_raster <- raster(paste("05_ERA5/",fechaInteres,"-SP_raster.tif",sep=""))
  #plot(SP_raster)
  
  ## V10
  V10_raster <- raster(paste("05_ERA5/",fechaInteres,"-V10_raster.tif",sep=""))
  #plot(V10_raster)
  
  ## U10
  U10_raster <- raster(paste("05_ERA5/",fechaInteres,"-U10_raster.tif",sep=""))
  #plot(U10_raster)
  
  ################# -----     DayWeek     -----
  ################# -----     ERA5     -----
  ## BLH     -----
  dayWeek_raster <- raster(paste("06_weekDay/",fechaInteres,"-weekDay_raster.tif",sep=""))
  #plot(dayWeek_raster)
  
  ##### STACK
  
  r_stack <- stack(MAIAC_raster,NDVI_raster,LandCover_raster,
                   BCSMASS_raster ,DUSMASS_raster,DUSMASS25_raster,
                   OCSMASS_raster,SO2SMASS_raster, SO4SMASS_raster,
                   SSSMASS_raster ,SSSMASS25_raster,BLH_raster,
                   SP_raster, D2M_raster, T2M_raster,V10_raster, 
                   U10_raster,TP_raster,DEM_raster,dayWeek_raster)
  
  #plot(r_stack)
  
  r_stack_df <- as.data.frame(r_stack, na.rm = TRUE)
  names(r_stack_df)
  
  names(r_stack_df) <- c( "AOD_055" ,"ndvi","LandCover","BCSMASS",
                          "DUSMASS","DUSMASS25",
                          "OCSMASS","SO2SMASS",
                          "SO4SMASS","SSSMASS",
                          "SSSMASS25","blh_mean" ,
                          "sp_mean","d2m_mean",
                          "t2m_mean","v10_mean",
                          "u10_mean" ,"tp_mean" , 
                          "DEM","dayWeek")
  
  
  ###############################################################
  ##################################################################
  ##############################################################
  ####
  
  # Aplicar el modelo
  d_stack <- xgb.DMatrix(data = as.matrix(r_stack_df))
  predictions <- predict(xgb_cv_model, d_stack)
    # Crear un raster vac?o con la misma extensi?n y resoluci?n que el stack
  pred_raster <- raster(r_stack)
  
  # Asignar las predicciones al raster
  pred_raster[] <- NA  # Inicia con valores NA
  
  # Reinsertar las predicciones en las celdas correspondientes
  pred_raster[!is.na(values(r_stack[[1]]))] <- predictions
  
  #getwd()
  
  
  name_salida <- paste(dir_salida,"PM-",fechaInteres,"-01-XGB_cv_M1-100924.tif",sep="")
  writeRaster(pred_raster, filename = name_salida, format = "GTiff", overwrite = TRUE)
  
  ##############################################################
  # Definir coordenadas (por ejemplo, latitud y longitud)
  #for (x in 1:1){
  # Dataframe con coordenadas y nombres de estaciones
  puntos <- data.frame(
    lon = c(-70.659566, -70.66517052, -70.73210014, -70.58813772, -70.52346222, -70.7503877, -70.59475058, -70.74822755),
    lat = c(-33.465694, -33.54606688, -33.43301075, -33.51610874, -33.37639222, -33.43798487, -33.59134682, -33.36576262),
    estacion = c("OHG", "BSQ", "CNA", "FLD", "CDE", "PDH", "PTA", "QUI")
  )
  
  # Extraer los valores del raster en las coordenadas especificadas
  valores_raster <- extract(pred_raster, puntos[, c("lon", "lat")])
  
  # Unir los valores del raster al dataframe original
  puntos_con_valores <- puntos %>%
    mutate(valor_raster = valores_raster)
  
  # Mostrar el dataframe resultante
  # print(puntos_con_valores)
  puntos_con_valores$date <- fechaInteres
  
  df_rbind <- rbind(df_rbind,puntos_con_valores)
}
view(df_rbind)
write.csv(df_rbind, paste(dir_salida,"salida_01-XGB_cv_M2-100924_01-2024.csv",sep=""))



#manualmente lo unimos con las estaciones sinca
dir_salida <- "D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/Prediccion_01-2024/Salida/"

data_pm <- read.csv(paste(dir_salida,"salida_modelos.csv",sep=""))
data_pm <- data_pm[complete.cases(data_pm),]
# Crear scatter plot con línea de regresión
modelo <- lm(X02_RF.CV.M1.090924 ~ valor_sinca, data = data_pm)

# Calcular R²
r2 <- summary(modelo)$r.squared

# Calcular RMSE (Root Mean Squared Error)
rmse <- sqrt(mean((data_pm$X02_RF.CV.M1.090924 - predict(modelo))^2))

# Calcular Bias
bias <- mean(data_pm$X02_RF.CV.M1.090924 - data_pm$valor_sinca)

pearson <- cor(data_pm$valor_sinca, data_pm$X02_RF.CV.M1.090924, method = "pearson")
# Crear scatter plot con línea de regresión
a<-ggplot(data_pm, aes(x = valor_sinca, y = X02_RF.CV.M1.090924)) +
  geom_point(color = "#99d8c9", alpha= 0.7,size = 2) +  # Puntos del scatter plot
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Línea de regresión lineal
  labs(x = "SINCA", y = "Prediccion", title = "Prediccion 01-2024 02-RF.CV.M1") +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +  # Línea 1:1
  ylim (0,25)+
  xlim (0,25)+
  theme_classic() +
  # scale_x_continuous(breaks = c(0,5,10,15,20,25)) +  # Marcas en el eje x cada 5 unidades
  # scale_y_continuous(breaks = c(0,15)) +  # Marcas en el eje y cada 5 unidades
  ###### RF
  geom_text(aes(x = 1.9, y = 24,
                label = paste("r2 = ",round(r2,2))),
            stat = "unique",
            size = 2.5, color = "Black")+
  geom_text(aes(x = 1.8, y = 22.5,
                label = paste("r = ",round(pearson,2))),
            stat = "unique",
            size = 2.5, color = "Black")+

  geom_text(aes(x = 2.25, y = 21,
                label = paste("RMSE = ",round(rmse,2))),
            stat = "unique",
            size = 2.5, color = "Black") +


  geom_text(aes(x = 2.10, y = 19.5,
                label = paste("Bias = ",round(bias,2))),
            stat = "unique",
            size = 2.5, color = "Black")
a
# ####### XGB
# geom_text(aes(x = 8, y = 30,
#               label = paste("r2 = ",round(r2,2))),
#           stat = "unique",
#           size = 2.5, color = "Black")+
#   geom_text(aes(x = 7.8, y = 28,
#                 label = paste("r = ",round(pearson,2))),
#             stat = "unique",
#             size = 2.5, color = "Black")+
#   
#   geom_text(aes(x = 8.23, y = 26,
#                 label = paste("RMSE = ",round(rmse,2))),
#             stat = "unique",
#             size = 2.5, color = "Black") +
#   
#   
#   geom_text(aes(x = 8.10, y = 24,
#                 label = paste("Bias = ",round(bias,2))),
#             stat = "unique",
#             size = 2.5, color = "Black")


data_pm <- data.frame(pred = predictions, real=test_data$PM25,
                      residuos = test_data$PM25- predictions)

# Graficar
ggplot(data_pm, aes(x = pred, y = residuos)) +
  geom_point(color = "#99d8c9", alpha = 0.7, size = 2) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  labs(x = "Predicciones", y = "Residuos", title = "01-XGB_cv_M4-100924") +
  theme_classic()+
  scale_x_continuous(limits = c(-5,130),breaks = c(0,20,40,60,80,100,120,140,160)) +  # Marcas en el eje x cada 5 unidades00
  scale_y_continuous(limits = c(-60,100),breaks = c(-60,-40,-20,0,20,40,60,80, 100)) # Marcas en el eje y cada 5 unidades


importance_matrix <- xgb.importance(model = xgb_cv_model)
xgb.plot.importance(importance_matrix = importance_matrix)
importance_matrix
