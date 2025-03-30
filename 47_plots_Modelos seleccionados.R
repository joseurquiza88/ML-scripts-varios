# Metricas de interes de los modelos

##############################################################
################################################################
# Matriz de correlacion de todos los datos
estacion <- "MD"
data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/proceed/merge_tot/",estacion,"_merge_comp.csv",sep=""))
data$date <- strptime(data$date, format = "%Y-%m-%d")
data$dayWeek <- wday(data$date, week_start = 1)


data_subt <- data[,c( "PM25","AOD_055" ,"ndvi", "BCSMASS_dia",
                      "DUSMASS_dia" ,"DUSMASS25_dia" ,"OCSMASS_dia",
                      "SO2SMASS_dia","SO4SMASS_dia",  "SSSMASS_dia",
                      "SSSMASS25_dia","blh_mean","sp_mean","d2m_mean",
                      "t2m_mean","v10_mean","u10_mean","tp_mean","DEM","dayWeek")]
library(ggcorrplot)
library(viridis)
matriz_correlacion <- cor(data_subt, use = "complete.obs") # use="complete.obs" excluye los NA
colores_intensos <- brewer.pal(n = 11, name = "RdBu")
colores_personalizados <- colorRampPalette(c("darkblue", "blue", "white", "red", "darkred"))

# Visualizar la matriz de correlación con ggcorrplot
ggcorrplot(matriz_correlacion, 
           method = "square",#"circle",         # Método para graficar (círculos)
           type = "lower",            # Mostrar solo la mitad inferior
           #lab = TRUE,                # Mostrar valores de correlación
           title = "Matriz de Correlación",
           colors = colores_personalizados(11),
             #brewer.pal(n = 3, name = "RdBu"), # Paleta "Red-Blue" de RColorBrewer, # Paleta de colores
           lab_size = 0.2 )              # Tamaño de las etiquetas
##############################################################
################################################################

estacion <- "MX"
setwd(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/modelo",sep=""))

#load("02-RF_cv_M1-261224_MX.RData")
load("02-RF_cv_M1-080125_MD.RData")
load("02-RF_cv_M1-070225-SP.RData")
load("03-XGB_cv_M1-041024_CH.RData")
load("02-RF_cv_M1-120225-CH.RData")
load (paste(modelo,".RData",sep=""))
importancia <- varImp(rf_cv_model, scale = TRUE)
importancia <- varImp(xgb_tuned, scale = TRUE)
importancia <- varImp(xgb_model, scale = TRUE)
#plot(importancia, main = "Importancia de Variables M1")
importancia_df <- as.data.frame(importancia$importance)
importancia_df$Variable <- rownames(importancia_df)

ggplot(importancia_df, aes(x = reorder(Variable, Overall), y = Overall)) +
  
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = 50, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_classic()+ 
  
  labs(title = "Importancia de Variables",subtitle = "02-XGB-cv-TunGrid_M1-280224_MX" ,x = "Variables", y = "Importancia") + 
  theme(
  plot.title = element_text(size = 12),# face = "bold"), # Tamaño del título
  plot.subtitle = element_text(size = 11)             # Tamaño del subtítulo
)


##############################################################
################################################################
# Residuos
estacion <- "MD"
setwd(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/modelo",sep=""))
modelName <- paste("07-RF-ESP_cv_M6-110225-",estacion,sep="")
load("02-RF_cv_M1-070225-MX.RData")
load("02-RF_cv_M1-080125_MD.RData")
load("02-RF_cv_M1-070225-SP.RData")
load("02-RF_cv_M1-120225-CH.RData")
load("07-RF_esp_cv_M6-180225-CH.RData")
load("02-XGB-cv-TunGrid_M1-280224_CH.RData")
load("02-XGB_M1-260225_CH.RData")
#modelo 02-XGB_M1-260225_MD
# 07-RF-ESP_cv_M6-110225-MD
# 02-XGB-cv-TunGrid_M1-280224_MD
#07-RF-ESP_cv_M6-110225-SP
# 02-XGB-cv-TunGrid_M1-280224_SP
# 02-XGB_M1-260225_SP
#07-RF_ESP_cv_M6-180225-MX
# 02-XGB-cv-TunGrid_M1-280224_MX
# 02-XGB_M1-260225_MX
load(paste(modelName,".RData",sep=""))
modelo <- "1"
#Data modelo 1
test_data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))
train_data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))

# Predicciones en el conjunto de entrenamiento y testeo
y_train_pred <- predict(rf_cv_model, newdata = train_data)
y_test_pred <- predict(rf_cv_model, newdata = test_data)

y_train_pred <- predict(xgb_tuned, newdata = train_data)
y_test_pred <- predict(xgb_tuned, newdata = test_data)



# Separar las características y la variable objetivo
X <- train_data[ , c("AOD_055", "ndvi", "BCSMASS_dia", "DUSMASS_dia",
                     "DUSMASS25_dia","OCSMASS_dia", "SO2SMASS_dia",
                     "SO4SMASS_dia", "SSSMASS_dia", "SSSMASS25_dia", 
                     "blh_mean", "sp_mean", "d2m_mean",
                     "t2m_mean", "v10_mean", "u10_mean", "tp_mean",
                     "DEM","dayWeek")]

y <- train_data$PM25

X_test<- test_data[ , c("AOD_055", "ndvi", "BCSMASS_dia", "DUSMASS_dia",
                     "DUSMASS25_dia","OCSMASS_dia", "SO2SMASS_dia",
                     "SO4SMASS_dia", "SSSMASS_dia", "SSSMASS25_dia", 
                     "blh_mean", "sp_mean", "d2m_mean",
                     "t2m_mean", "v10_mean", "u10_mean", "tp_mean",
                     "DEM","dayWeek")]


y_test <- test_data$PM25

dtest <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)
dtrain <- xgb.DMatrix(data = as.matrix(X), label = y)
# Realizar predicciones
y_train_pred <- predict(xgb_model, newdata = dtrain)
y_test_pred <- predict(xgb_model, newdata = dtest)



# Calcular los residuos
residuals_train <- train_data$PM25 - y_train_pred
residuals_test <- test_data$PM25 - y_test_pred
# Crear un dataframe con los valores predichos y los residuos
data_test <- data.frame(predicted = y_test_pred,real=test_data$PM25, residuals = residuals_test)
data_train <- data.frame(predicted = y_train_pred, real=train_data$PM25,residuals = residuals_train)

###-- 
plot_residuos_test <- ggplot(data_test, aes(x = predicted, y = residuals)) +
  geom_point(color = "#2c7fb8",size=2,alpha=0.5) + # Puntos en azul
  geom_hline(yintercept = 0, color = "#023858",  size = 0.7) + # Línea horizontal
  #ggtitle("Test") +
  xlab("PM 2.5 Observado") +
  ylab("Residuos") +
  
  #scale_x_continuous(limits = c(0, 145),breaks = seq(0, 140, by = 20)) +  # Ticks cada 10 en el eje X
  #scale_x_continuous(limits = c(0, 150),breaks = seq(0, 150, by = 30)) +  # Ticks cada 10 en el eje X
  #scale_y_continuous(limits = c(-30, 90),breaks = seq(-30, 90, by = 30)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje X
  
  scale_y_continuous(limits = c(-60, 90),breaks = seq(-60, 90, by = 30)) +  # Ticks cada 10 en el eje Y
  theme_classic()+
  theme(
    plot.title = element_text(size = 10),  # Tamaño del título
    axis.title.x = element_text(size = 8),  # Tamaño etiqueta eje X
    axis.title.y = element_text(size = 8)   # Tamaño etiqueta eje Y
  )
#plot_residuos_test
# plot_residuos_train <- ggplot(data_train, aes(x = predicted, y = residuals)) +
#   geom_point(color = "#31a354",size=2,alpha=0.5) + # Puntos en azul
#   geom_hline(yintercept = 0, color = "#de2d26", linetype = "dashed", size = 0.7) + # Línea horizontal
#   #ggtitle("Train") +
#   xlab("Valores predichos (Train)") +
#   # scale_x_continuous(limits = c(0, 145),breaks = seq(0, 140, by = 20)) +  # Ticks cada 10 en el eje X
#   scale_x_continuous(limits = c(0, 165),breaks = seq(0, 160, by = 20)) +  # Ticks cada 10 en el eje X
#   scale_y_continuous(limits = c(-60, 100),breaks = seq(-60, 110, by = 40)) +  # Ticks cada 10 en el eje Y
#   
#   ylab("Residuos") +
#   theme_classic()+
#   theme(
#     plot.title = element_text(size = 10),  # Tamaño del título
#     axis.title.x = element_text(size = 8),  # Tamaño etiqueta eje X
#     axis.title.y = element_text(size = 8)   # Tamaño etiqueta eje Y
#   )
plot_residuos_test
# plot_residuos_train


# Combinar los gráficos en una fila y dos columnas
#combinacion_residuos<- grid.arrange(plot_residuos_train, plot_residuos_test, 
 #                                   nrow = 1, ncol = 2, top = "02-RF_cv_M1-261224_MX")


########################################################
##### ---- Gráfico de dispersión de Predicciones vs Valores Reales
#muestra cómo se alinean las predicciones del modelo SVR con los valores
#reales. La línea y = x sirve como referencia para una predicción perfecta.

###-- 

plot_y_test <- ggplot(data_test, aes(x = real, y = predicted)) +
  geom_point(color = "#2c7fb8",size=2,alpha=0.5) + # Puntos en azul
  geom_abline(intercept = 0, slope=1,color = "black",size = 0.7) + # Línea horizontal
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE, linetype = "dashed",size = 0.6) +  # Línea de regresión
  
  #ggtitle("Test") +
  xlab("PM 2.5 Observado") +
  ylab("Prediccion") +
  
  #scale_x_continuous(limits = c(0, 150),breaks = seq(0, 150, by = 30)) +  # Ticks cada 10 en el eje X
  #scale_y_continuous(limits = c(0,150),breaks = seq(0, 150, by = 30)) +    # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje X
  scale_y_continuous(limits = c(0,160),breaks = seq(0, 160, by = 40)) +    # Ticks cada 10 en el eje Y
  
  theme_classic()+
  theme(
    plot.title = element_text(size = 10),  # Tamaño del título
    axis.title.x = element_text(size = 8),  # Tamaño etiqueta eje X
    axis.title.y = element_text(size = 8)   # Tamaño etiqueta eje Y
  )
#plot_y_test
# plot_y_train <- ggplot(data_train, aes(y = predicted, x = real)) +
#   geom_point(color = "#31a354",size=2,alpha=0.5) + # Puntos en azul
#   geom_abline(intercept = 0, slope=1, color = "#de2d26", linetype = "dashed", size = 0.7) + # Línea horizontal
#   #ggtitle("Train") +
#   
#   # scale_x_continuous(limits = c(0, 180),breaks = seq(0, 185, by = 30)) +  # Ticks cada 10 en el eje X
#   # scale_y_continuous(limits = c(0,180),breaks = seq(0, 185, by = 30)) +   # Ticks cada 10 en el eje Y
#   
#   xlab("Observado") +
#   ylab("Prediccion") +
#   theme_classic()+
#   theme(
#     plot.title = element_text(size = 10),  # Tamaño del título
#     axis.title.x = element_text(size = 8),  # Tamaño etiqueta eje X
#     axis.title.y = element_text(size = 8)   # Tamaño etiqueta eje Y
#   )
#plot_y_test
#plot_y_train


# Combinar los gráficos en una fila y dos columnas
#combinacion_valores<- grid.arrange(plot_y_train, plot_y_test, 
 #                                  nrow = 1, ncol = 2, top = "02-RLM_cv_M1-141024")
####
# Combinacion residuos  +  prediccion
combinacion_plots<- grid.arrange(plot_y_test, plot_residuos_test, 
                                    nrow = 1, ncol = 2, top = modelName)


###################################
# histograma de residuos

data_test$modelo <- "Model"
ggplot(data_test, aes(x = residuals)) +
  geom_histogram(alpha = 0.3,color="black",fill="#2c7fb8") +
  
  scale_x_continuous(limits = c(-80,80),breaks = seq(-80,80, by = 40)) +   # Ticks cada 10 en el eje Y
  theme_classic()+
  #scale_y_continuous(limits = c(0,5000),breaks = seq(0,5000, by = 1000)) +
  scale_y_continuous(limits = c(0,6000),breaks = seq(0,6000, by = 1000)) +
  
  labs(#title = "C02-XGB_M1-260225_CH",
       x = modelName, y = "Residuos")



# Q-Q Plot
# Valores originales
qqnorm(data_test$residuals, main = "Q-Q Plot de Residuos")
qqline(data_test$residuals, col = "red", lwd = 2)
# Resiuods originales
residuos_std <- scale(data_test$residuals)  # Estandarización (media = 0, sd = 1)
qqnorm(residuos_std, main = modelName)
qqline(residuos_std, col = "red", lwd = 2)

shapiro.test(data_test$residuals)
