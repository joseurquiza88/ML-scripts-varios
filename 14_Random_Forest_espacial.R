
# Cargar los paquetes necesarios
library(randomForest)
library(raster)
library(caret) #version 4.2.3
rm(list=ls())


#Data modelo 1
# Estos datos corresponden al dataset completo - 4 estaciones que tenian
# 4-5 datos
# data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/SP/modelos/ParticionDataSet/Modelo 6/SP_merge_comp_modif.csv")
# data$date <- strptime(data$date, format = "%d/%m/%Y")#"%Y-%m-%d")
# data$dayWeek <- wday(data$date, week_start = 1)


#Data modelo 1
estacion <- "MX"
modelo<-6
data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_",modelo,"/",estacion,"_merge_comp.csv",sep=""))
#data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 7/09_TOT_merge_tot.csv")
data <- data[complete.cases(data), ]
data$date <- strptime(data$date, format = "%Y-%m-%d")#"%d/%m/%Y")#
data$dayWeek <- wday(data$date, week_start = 1)



names(data) <- c("X.1" ,"ID", "date",
                 "estacion", "PM25","AOD_055",                 
                  "ndvi" ,  "BCSMASS_dia","DUSMASS_dia" ,
                 "DUSMASS25_dia", "OCSMASS_dia", "SO2SMASS_dia" ,
                 "SO4SMASS_dia",
                      "SSSMASS_dia", "SSSMASS25_dia",                 
                      "blh_mean","blh_min","blh_max"  ,"blh_sd","blh_mean_subt", "sp_mean",                  
                      "sp_min", "sp_max","sp_sd"   ,"sp_mean_subt","d2m_mean","d2m_min",                  
                      "d2m_max","d2m_sd" , "d2m_mean_subt",  "t2m_mean", "t2m_min", "t2m_max",                  
                      "t2m_sd",  "t2m_mean_subt", "v10_mean",  "v10_min" , "v10_max" ,"v10_sd"  ,                 
                      "v10_mean_subt", "u10_mean" , "u10_min"  , "u10_max","u10_sd",
                      "u10_mean_subt","tp_mean", "tp_min","tp_max",  "tp_sd", "tp_mean_subt", "DEM" , "dayWeek")



# Numero de estaciones (en este caso, 8)
estaciones <- unique(data$ID)
estaciones <- unique(data$estacion)
length(estaciones)
# Crear una lista considerando dejar una estacion afuera
set.seed(123)  # Para reproducibilidad
subsets <- createFolds(estaciones, k = 12, list = TRUE, returnTrain = FALSE)
subsets <- createFolds(estaciones, k = 8, list = TRUE, returnTrain = FALSE)
#MD
subsets <- createFolds(estaciones, k = 17, list = TRUE, returnTrain = FALSE)
#MX
subsets <- createFolds(estaciones, k = 22, list = TRUE, returnTrain = FALSE)

# Muestra los valores reales asociados a cada fold, los ID de cada estacion
subsets <- lapply(subsets, function(idx) estaciones[idx])

# Ver como se dividen las estaciones
# Los ID son caracteres por eso los ordena de esa forma
print(subsets)

dir <- paste("D:/Josefina/Proyectos/ProyectoChile/modelos/",estacion,"modelo/ModeloEspacial/",sep="")

#spatial_cv <- function(data, subsets,dir,numFolds) {
  resultados_rbind <- data.frame()  # DataFrame para las m?tricas
  models <- list()   # Lista para almacenar todos los modelos
  
  # Iniciar con valores altos
  best_model <- NULL
  best_rmse <- Inf 
  train_control <- trainControl(
    method = "cv",          # M?todo de validaci?n cruzada
    number = 3,            # N?mero de pliegues para la validaci?n cruzada
    verboseIter = TRUE,     # Mostrar progreso de entrenamiento
    allowParallel = TRUE    # Permitir procesamiento paralelo
  )
  
  for (i in 1:length(subsets)) {
    print(paste("Fold", i))
    
    # Dividir el conjunto de datos en entrenamiento y validaci?n seg?n estaciones
    validation_stations <- subsets[[i]]  # Estaciones para validaci?n
    training_stations <- setdiff(estaciones, validation_stations)  # Resto de las estaciones para entrenar
    
    train_data_fold <- data[data$ID %in% training_stations, ]
    valid_data_fold <- data[data$ID %in% validation_stations, ]
    
    #train_data_fold <- data[data$estacion %in% training_stations, ]
    #valid_data_fold <- data[data$estacion %in% validation_stations, ]

    # Entrenar el modelo con el conjunto de entrenamiento
    rf_model <- train(PM25 ~ AOD_055 + ndvi + BCSMASS_dia + DUSMASS_dia + DUSMASS25_dia + OCSMASS_dia + SO2SMASS_dia +
                        SO4SMASS_dia + SSSMASS_dia + SSSMASS25_dia + blh_mean + sp_mean + d2m_mean + t2m_mean + v10_mean +
                        u10_mean + tp_mean + DEM + dayWeek, trControl = train_control,
                      data = train_data_fold, method = "rf", #trControl = trainControl(method = "none"),
                      importance = TRUE)
    
    # Realizar predicciones sobre el conjunto de validaci?n
    predictions <- predict(rf_model, valid_data_fold)
    
    # Calcular m?tricas de testeo
    rmse <- sqrt(mean((predictions - valid_data_fold$PM25)^2))
    r2 <- cor(predictions, valid_data_fold$PM25)^2  
    r <- cor(valid_data_fold$PM25, predictions, method = "pearson")
    mae <- mean(abs(valid_data_fold$PM25 - predictions))
    mape <- mean(abs((valid_data_fold$PM25 - predictions) / valid_data_fold$PM25)) * 100
    mse <- mean((valid_data_fold$PM25 - predictions)^2)
    medae <- median(abs(valid_data_fold$PM25 - predictions))
    min <- min(predictions)
    max <- max(predictions)
    
    # Metricas de entrenamiento
    predictions_train <- predict(rf_model, train_data_fold)
    rmse_train <- sqrt(mean((predictions_train - train_data_fold$PM25)^2))
    r2_train <- cor(predictions_train, train_data_fold$PM25)^2  
    r_train <- cor(train_data_fold$PM25, predictions_train, method = "pearson")
    mae_train <- mean(abs(train_data_fold$PM25 - predictions_train))
    mape_train <- mean(abs((train_data_fold$PM25 - predictions_train) / train_data_fold$PM25)) * 100
    mse_train <- mean((train_data_fold$PM25 - predictions_train)^2)
    medae_train <- median(abs(train_data_fold$PM25 - predictions_train))
    min_train <- min(predictions_train)
    max_train <- max(predictions_train)
    
    # Guardar el modelo con un nombre basado en la estaci?n que qued? fuera
    model_name <- paste0(dir,"modelo_estacion_", paste(validation_stations, collapse = "_"), ".RData")
    #save(rf_model, file = model_name)
    models[[i]] <- model_name  # Registrar el nombre del modelo
    
    # Guardar m?tricas en el DataFrame
    # R2	R	RMSE	MAE	MAPE	MSE	MedAE	Min	Max
    resultados <- data.frame(iteracion = i, estacion_fuera = paste(validation_stations, collapse = ","),
                             r2 = r2,r=r,rmse = rmse,mae=mae, mape=mape, mse=mse,
                             medae = medae, min=min, max=max,
                             
                             r2_train = r2_train,r_train=r_train, rmse_train = rmse_train,
                             mae_train=mae_train, mape_train=mape_train, mse_train=mse_train,
                             medae_train = medae_train, min_train=min_train, max_train=max_train
                             
                             
                             )#, modelo_archivo = model_name)
    resultados_rbind <- rbind(resultados_rbind, resultados)
    
    # Verificar si el modelo actual es mejor que el anterior
    # if (rmse < best_rmse) {
    #   best_rmse <- rmse
    #   best_model <- rf_model  
    # }
  }
  
  # Guardar todos los resultados en un archivo .RData
  #save(resultados_rbind, file = "resultados_metricas.RData")
  
  # Retornar el DataFrame con las m?tricas
  #return(resultados_rbind)
}


# Correr la funcion
cv_results <- spatial_cv(data, subsets,dir,numFolds=3)
08:51
# Ver los resultados de la validacin cruzada
cv_results$rmse_avg
cv_results$r2_avg
setwd(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/modelo",sep=""))
# getwd()
# save(lm_cv_model, file=paste("02-RLM-M",modelo,"_100125",estacion,".RData",sep=""))

write.csv(cv_results,paste("cv_results_",estacion,".csv",sep=""))
load("08-RF_cv_M6-261124_SP.RData")
