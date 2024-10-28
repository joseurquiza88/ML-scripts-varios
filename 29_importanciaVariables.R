
# ---  OBJETIVO ---
#### Importancia de las variables de los distintos modelos de ML generados

## -- Leemos dataset de entrenamiento y testeo
dir <- "D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/"
setwd(dir)
train_data <- read.csv(paste(dir,"Modelo 1/M1_train.csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo 3/M3_test.csv",sep=""))

## -- Cargamos el modelo de interes

# Guardamos modelo
modelDir <- "D:/Josefina/Proyectos/ProyectoChile/modelos/modelo/"
model <- load(paste(modelDir,"02-RLM_cv_M1-141024.RData",sep=""))
model

##### ----- Modelo Random Forest -----   #####
# Vemos importancia de las variables
importancia <- varImp(rf_cv_model, scale = TRUE)
importancia <- varImp(lm_cv_model, scale = TRUE)
importancia <- varImp(decision_tree_model_cv, scale = TRUE)
importancia <- varImp(treeBag_model_cv, scale = TRUE)
# Suponiendo que tu modelo es xgb_cv_model y tienes un conjunto de datos de entrenamiento
importance <- xgb.importance(model = xgb_cv_model)

# Ver la importancia de las variables
print(importance)

print(importancia)
# Cuantas variables tenemos en el modelo = 20, corroboramos
length(importancia$importance$Overall) # 20
# Gráfico personalizado con ggplot2
importancia_df <- as.data.frame(importancia$importance)
importancia_df <- data.frame(Variable = importance$Feature,Overall=importance$Frequency)
importancia_df$Variable <- rownames(importancia_df)

ggplot(importancia_df, aes(x = reorder(Variable, Overall), y = Overall)) +

  geom_bar(stat = "identity", fill = "steelblue") +
  #geom_hline(yintercept = 50, linetype = "dashed", color = "red") +
  scale_y_continuous(limits = c(0, 100))+#,breaks = seq(0, 100, by = 20)) +  # Ticks cada 10 en el eje X
  
  coord_flip() +
  theme_classic()+
  labs(title = "Importancia 02-RLM_cv_M1-230924", x = "Variables", y = "Importancia")

ggplot(importancia_df, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = 50, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_classic()+
  labs(title = "Importancia 03-XGB_cv_M1-041024", x = "Variables", y = "Importancia")

##### ----- Modelo Random Forest -----   #####
# Vemos importancia de las variables