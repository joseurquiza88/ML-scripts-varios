# Cargar los paquetes necesarios
library(randomForest)
library(raster)
library(caret) #version 4.2.3
rm(list=ls())
# Generar datos aleatorios para las variables predictoras
set.seed(42)
# data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/merge_tot/08_TOT_merge_tot.csv")
# data_completo <- data[complete.cases(data),]
# data_completo$date <- as.Date(data_completo$date)
# data_completo$dayYear <- yday(data_completo$date)
# data_completo$month <- as.numeric(format(data_completo$date, "%m"))
# # Dividir el dataframe en 70% entrenamiento y 30% testeo
# train_index <- createDataPartition(data_completo$PM25, p = 0.7, list = FALSE)
# train_data <- data_completo[train_index, ]
# test_data <- data_completo[-train_index, ]


dir <- "D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/"
dir <- "D:/Josefina/Proyectos/ProyectoChile/SP/modelos/ParticionDataSet/"
setwd(dir)
train_data <- read.csv(paste(dir,"Modelo 1/M1_train.csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo 1/M1_test.csv",sep=""))
# Entrenar el modelo de regresión lineal múltiple
lm_model <- lm(PM25 ~ AOD_055 + ndvi +  BCSMASS +#LandCover
                 DUSMASS + DUSMASS25 + OCSMASS + SO2SMASS+
                 SO4SMASS + SSSMASS + SSSMASS25 + blh_mean +
                 sp_mean + d2m_mean + t2m_mean + v10_mean + 
                 u10_mean + tp_mean + DEM + dayWeek, data = train_data)
13:27
# Resumen del modelo
summary(lm_model)
print(lm_model)
# Hacer predicciones
predicciones <- predict(lm_model, newdata = test_data)

# Comparar predicciones con valores reales
resultado <- data.frame(Real = test_data$PM25, Predicho = predicciones)
print(resultado)


# Calcular el error cuadrÃ¡tico medio (RMSE)
rmse <- sqrt(mean((predicciones - test_data$PM25)^2))

# Calcular el coeficiente de determinaciÃ³n (RÂ²)
r_squared <- cor(predicciones, test_data$PM25)^2

# Mostrar las mÃ©tricas de evaluaciÃ³n
cat("RMSE:", rmse, "/n")
cat("R²:", r_squared, "/n")

####################################################################
####################################################################
####################################################################
dir <- "D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/"
setwd(dir)
train_data <- read.csv(paste(dir,"Modelo 1/M1_train.csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo 1/M1_test.csv",sep=""))
# Definir el control de entrenamiento con validaciÃ³n cruzada de 10 pliegues
train_control <- trainControl(method = "cv", number = 10)

# Entrenar el modelo de regresión lineal múltiple con validación cruzada
lm_cv_model <- train(PM25 ~ AOD_055 + ndvi +  LandCover + BCSMASS +
                       DUSMASS + DUSMASS25 + OCSMASS + SO2SMASS+
                       SO4SMASS + SSSMASS + SSSMASS25 + blh_mean +
                       sp_mean + d2m_mean + t2m_mean + v10_mean + 
                       u10_mean + tp_mean + DEM + dayWeek,
                     data = train_data, method = "lm", trControl = train_control)
lm_cv_model <- train(PM25 ~ AOD_055 + ndvi +  LandCover + BCSMASS_dia +
                       DUSMASS_dia + DUSMASS25_dia + OCSMASS_dia + SO2SMASS_dia +
                       SO4SMASS_dia + SSSMASS_dia + SSSMASS25_dia + blh_mean +
                       sp_mean + d2m_mean + t2m_mean + v10_mean + 
                       u10_mean + tp_mean + DEM + dayWeek,
                     data = train_data, method = "lm", trControl = train_control)
names(train_data)
# SP
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


lm_cv_model <- train(PM25 ~ AOD_055 + ndvi +   BCSMASS_dia + #LandCover
                       DUSMASS_dia + DUSMASS25_dia + OCSMASS_dia + SO2SMASS_dia +
                       SO4SMASS_dia + SSSMASS_dia + SSSMASS25_dia + blh_mean +
                       sp_mean + d2m_mean + t2m_mean + v10_mean + 
                       u10_mean + tp_mean + DEM + dayWeek,
                     data = train_data, method = "lm", trControl = train_control)

lm_cv_model <- train(PM25 ~ AOD_055,
                     data = train_data, method = "lm", trControl = train_control)

# Mostrar los resultados del modelo
print(lm_cv_model)

# Hacer predicciones
predicciones <- predict(lm_cv_model, newdata = test_data)
#predicciones <- exp(predicciones)



# Comparar predicciones con valores reales
resultado <- data.frame(Real = test_data$PM25, Predicho = predicciones)
#print(resultado)


rmse_cv <- sqrt(mean((predicciones - test_data$PM25)^2))
r_squared_cv <- cor(predicciones, test_data$PM25)^2
mae_cv <- mean(abs(test_data$PM25 - predicciones))
mape_cv <- mean(abs((test_data$PM25 - predicciones) / test_data$PM25)) * 100
mse_cv <- mean((test_data$PM25 - predicciones)^2)
medae_cv <- median(abs(test_data$PM25 - predicciones))
pearson_cor_cv <- cor(test_data$PM25, predicciones, method = "pearson")

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
predicciones_train <- predict(lm_cv_model, newdata = train_data)
#predicciones_train <- exp(predicciones_train)
# Comparar predicciones con valores reales
resultado_train <- data.frame(Real = train_data$PM25, Predicho = predicciones_train)



rmse_cv_train <- sqrt(mean((predicciones_train - train_data$PM25)^2))
r_squared_cv_train <- cor(predicciones_train, train_data$PM25)^2
mae_cv_train <- mean(abs(train_data$PM25 - predicciones_train))
mape_cv_train <- mean(abs((train_data$PM25 - predicciones_train) / train_data$PM25)) * 100
mse_cv_train <- mean((train_data$PM25 - predicciones_train)^2)
medae_cv_train <- median(abs(train_data$PM25 - predicciones_train))
pearson_cor_cv_train <- cor(train_data$PM25, predicciones_train, method = "pearson")

cat("R² train:", r_squared_cv_train, "\n")
print(paste("R pearson cv train:", round(pearson_cor_cv_train, 3)))
cat("RMSE train:", rmse_cv_train, "\n")
print(paste("MAE train cv:", round(mae_cv_train, 3)))
print(paste("MAPE train cv:", round(mape_cv_train, 2), "%"))
print(paste("MSE train:", round(mse_cv_train, 3)))
print(paste("MedAE traincv:", round(medae_cv_train, 3)))
min(predicciones_train)
max(predicciones_train)






# Guardar el modelo entrenado
getwd()
setwd("D:/Josefina/Proyectos/ProyectoChile/modelos/modelo")

# save(lm_model, file="01-RL_260824.RData")
save(lm_cv_model, file="02-RLM_cv_M1-141024.RData")


print("Modelo Random Forest entrenado y guardado en 'random_forest_model.RData'.")

################################################################################
################################################################################
# Algunos plots para evaluar el modelo 

#### ---- Gráfico de dispersión de Predicciones vs Valores Reales:
#muestra cómo se alinean las predicciones del modelo con los valores reales. 
#Idealmente, los puntos deben estar cerca de la línea y = x.
plot(test_data$PM25, lm_predictions, 
     xlab = "Valores Reales de PM2.5", 
     ylab = "Predicciones de PM2.5", 
     main = "Predicciones vs Valores Reales")
abline(0, 1, col = "red")  # Añadir una línea y = x


# Calcular los residuos
residuos <- test_data$PM25 - lm_predictions

#### ----  Gráfico de residuos vs. predicciones

# Ayuda a verificar si los residuos están distribuidos 
#aleatoriamente y si hay patrones sistemáticos en ellos. 
# Un patrón podría indicar problemas con el modelo, 
# como la falta de ajuste para una relación no lineal.
plot(lm_predictions, residuos, 
     xlab = "Predicciones de PM2.5", 
     ylab = "Residuos", 
     main = "Residuos vs Predicciones")
abline(h = 0, col = "red")  # Añadir una línea horizontal en y = 0

#### ---- Histograma de los residuos
#muestra si estos están distribuidos normalmente, lo cual es un supuesto 
#importante en la regresión lineal
hist(residuos, 
     breaks = 20, 
     xlab = "Residuos", 
     main = "Histograma de los Residuos")


#### ---- Q-Q plot de los residuos
#Compara la distribución de los residuos con una distribución normal. 
# Si los puntos siguen aproximadamente la línea recta, los residuos
# están normalmente distribuidos.
qqnorm(residuos)
qqline(residuos, col = "red")


#### ---- Gráfico de Cook's Distance

#muestra la distancia de Cook para cada observación, que indica el 
#impacto de cada punto en los coeficientes del modelo. Los puntos 
#con una gran distancia de Cook podrían ser influyentes.
plot(cooks.distance(lm_model), 
     type = "h", 
     ylab = "Cook's Distance", 
     main = "Gráfico de Cook's Distance")
abline(h = 4/(nrow(train_data)-length(lm_model$coefficients)), col = "red")


# Calcular residuos estandarizados
std_resid <- rstandard(lm_model)

#### ---- Gráfico de residuos estandarizados vs. ajustes
# compara los residuos estandarizados con los valores ajustados. 
# Es útil para detectar heterocedasticidad (variación no constante de los residuos).
plot(fitted(lm_model), std_resid, 
     xlab = "Valores Ajustados", 
     ylab = "Residuos Estandarizados", 
     main = "Residuos Estandarizados vs Valores Ajustados")
abline(h = 0, col = "red")  # Añadir una línea horizontal en y = 0

################################################################################
################################################################################
# cargar el modelo y aplicalo a otro set de datos

# Paso 1: Cargar el modelo
load("01-RL_cv_260824.RData")

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
predictions <- predict(lm_cv_model, newdata = r_stack_df)

# Crear un raster vac?o con la misma extensi?n y resoluci?n que el stack
pred_raster <- raster(r_stack)

# Asignar las predicciones al raster
pred_raster[] <- NA  # Inicia con valores NA

# Reinsertar las predicciones en las celdas correspondientes
pred_raster[!is.na(values(r_stack[[1]]))] <- predictions

getwd()

writeRaster(pred_raster, filename = "D:/Josefina/Proyectos/ProyectoChile/modelos/SalidaModelo/01-RL_cv_260824.tif", format = "GTiff", overwrite = TRUE)

