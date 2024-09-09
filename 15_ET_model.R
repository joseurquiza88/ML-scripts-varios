# Configurar la semilla para reproducibilidad
set.seed(42)

#Data modelo 1
test_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 1/M1_test.csv")
train_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 1/M1_train.csv")


# Definir el control de entrenamiento con validación cruzada de 10 pliegues
train_control <- trainControl(method = "cv", number = 10)

# Definir un grid para ajustar los hiperparámetros
ranger_grid <- expand.grid(
  mtry = c(5, 10, 15),           # Número de variables a considerar en cada división
  splitrule = "variance",        # Regla de división (variance para regresión)
  min.node.size = c(1, 5, 10)    # Tamaño mínimo del nodo
)

# Entrenar el modelo Extra Trees con validación cruzada usando ranger
et_model <- train(PM25 ~ AOD_055 + ndvi + LandCover + BCSMASS +
                    DUSMASS + DUSMASS25 + OCSMASS + SO2SMASS +
                    SO4SMASS + SSSMASS + SSSMASS25 + blh_mean +
                    sp_mean + d2m_mean + t2m_mean + v10_mean + 
                    u10_mean + tp_mean + DEM, 
                  data = train_data, 
                  method = "ranger", 
                  trControl = train_control, 
                  tuneGrid = ranger_grid,
                  importance = 'impurity')  # 'impurity' para importancia de variables
ExTrees_cv_model <- train(PM25 ~ AOD_055 + ndvi + LandCover + BCSMASS +
                       DUSMASS + DUSMASS25 + OCSMASS + SO2SMASS+
                       SO4SMASS + SSSMASS + SSSMASS25 + blh_mean +
                       sp_mean + d2m_mean + t2m_mean + v10_mean + 
                       u10_mean + tp_mean + DEM + dayWeek, data = train_data, 
                     method = "extraTrees", trControl = train_control,importance = TRUE)









# 16:17
# Mostrar los resultados del modelo
print(et_model)
print(et_model$results)

# Predecir en el conjunto de testeo
predictions <- predict(et_model, newdata = test_data)

# Calcular el error cuadrático medio (RMSE)
rmse <- sqrt(mean((predictions - test_data$PM25)^2))

# Calcular el coeficiente de determinación (R²)
r_squared <- cor(predictions, test_data$PM25)^2

# Mostrar las métricas de evaluación
cat("RMSE:", rmse, "\n")
cat("R²:", r_squared, "\n")


###########################################################
###########################################################
#######################################################
library(ggplot2)

# Crear un dataframe con valores reales y predicciones
resultado <- data.frame(Real = test_data$PM25, Predicho = predictions)

# ------- Gráfico de Predicción vs. Real
ggplot(resultado, aes(x = Real, y = Predicho)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Predicción vs Real", x = "Valor Real", y = "Valor Predicho") +
  theme_minimal()

# ------- Calcular los residuos
residuos <- resultado$Real - resultado$Predicho

# ------- Gráfico de Residuos vs Predicciones
ggplot(resultado, aes(x = Predicho, y = residuos)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residuos vs Predicciones", x = "Valor Predicho", y = "Residuos") +
  theme_minimal()


# ------- Histograma de Residuos
ggplot(data.frame(residuos), aes(x = residuos)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Histograma de Residuos", x = "Residuos", y = "Frecuencia") +
  theme_minimal()


# ------- Gráfico Q-Q de Residuos
qqnorm(residuos)
qqline(residuos, col = "red")

# ------- Gráfico de Importancia de Variables
# Si sed usas ranger, la importancia de las variables puede ser extraída y visualizada
importance_df <- data.frame(Variable = names(et_model$finalModel$variable.importance),
                            Importancia = et_model$finalModel$variable.importance)

ggplot(importance_df, aes(x = reorder(Variable, Importancia), y = Importancia)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Importancia de las Variables", x = "Variable", y = "Importancia") +
  theme_minimal()



# Guardar el modelo entrenado
getwd()
setwd("D:/Josefina/Proyectos/ProyectoChile/modelos/modelo")

save(et_model, file="01-et_model_260824.RData")

################################################################################
################################################################################
# cargar el modelo y aplicalo a otro set de datos

# Paso 1: Cargar el modelo
load("01-et_model_260824.RData")

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
predictions <- predict(et_model, newdata = r_stack_df)

# Crear un raster vac?o con la misma extensi?n y resoluci?n que el stack
pred_raster <- raster(r_stack)

# Asignar las predicciones al raster
pred_raster[] <- NA  # Inicia con valores NA

# Reinsertar las predicciones en las celdas correspondientes
pred_raster[!is.na(values(r_stack[[1]]))] <- predictions

getwd()

writeRaster(pred_raster, filename = "D:/Josefina/Proyectos/ProyectoChile/modelos/SalidaModelo/01-ET_model_260824.tif", format = "GTiff", overwrite = TRUE)





