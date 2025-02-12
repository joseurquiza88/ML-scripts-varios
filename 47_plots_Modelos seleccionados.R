# Metricas de interes de los modelos

##############################################################
################################################################
# Matriz de correlacion de todos los datos
estacion <- "MX"
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

estacion <- "CH"
setwd(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/modelo",sep=""))

#load("02-RF_cv_M1-261224_MX.RData")
load("02-RF_cv_M1-080125_MD.RData")
load("02-RF_cv_M1-251124_SP.RData")
load("03-XGB_cv_M1-041024_CH.RData")
importancia <- varImp(rf_cv_model, scale = TRUE)

#plot(importancia, main = "Importancia de Variables M1")
importancia_df <- as.data.frame(importancia$importance)
importancia_df$Variable <- rownames(importancia_df)

ggplot(importancia_df, aes(x = reorder(Variable, Overall), y = Overall)) +
  
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = 50, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_classic()+ 
  
  labs(title = "Importancia de Variables",subtitle = "02-RF_cv_M1-251124_SP" ,x = "Variables", y = "Importancia") + 
  theme(
  plot.title = element_text(size = 12),# face = "bold"), # Tamaño del título
  plot.subtitle = element_text(size = 11)             # Tamaño del subtítulo
)


##############################################################
################################################################
# Residuos
estacion <- "CH"
setwd(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/modelo",sep=""))

load("02-RF_cv_M1-261224_MX.RData")
load("02-RF_cv_M1-080125_MD.RData")
load("02-RF_cv_M1-251124_SP.RData")
load("03-XGB_cv_M1-041024_CH.RData")

modelo <- "1"
#Data modelo 1
test_data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_",modelo,"/M",modelo,"_test_",estacion,".csv",sep=""))
train_data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/Modelo_",modelo,"/M",modelo,"_train_",estacion,".csv",sep=""))
# Para SP
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

# Predicciones en el conjunto de entrenamiento y testeo
y_train_pred <- predict(rf_cv_model, newdata = train_data)
y_test_pred <- predict(rf_cv_model, newdata = test_data)

y_train_pred <- predict(xgb_tuned, newdata = train_data)
y_test_pred <- predict(xgb_tuned, newdata = test_data)

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
plot_residuos_test
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
plot_y_test
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
                                    nrow = 1, ncol = 2, top = "03-XGB_cv_M1-041024_CH")


###################################

