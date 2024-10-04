
# ---  OBJETIVO ---
#### Importancia de las variables de los distintos modelos de ML generados

## -- Leemos dataset de entrenamiento y testeo
dir <- "D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/"
setwd(dir)
train_data <- read.csv(paste(dir,"Modelo /M3_train.csv",sep=""))
test_data <- read.csv(paste(dir,"Modelo 3/M3_test.csv",sep=""))

## -- Cargamos el modelo de interes

# Guardamos modelo
modelDir <- "D:/Josefina/Proyectos/ProyectoChile/modelos/modelo/"
model <- load(paste(modelDir,"02-RF_cv_M1-250924.RData",sep=""))
model

##### ----- Modelo Random Forest -----   #####
# Vemos importancia de las variables
importancia <- varImp(rf_cv_model, scale = TRUE)
print(importancia)
# Cuantas variables tenemos en el modelo = 20, corroboramos
length(importancia$importance$Overall) # 20
# Gráfico personalizado con ggplot2
importancia_df <- as.data.frame(importancia$importance)
importancia_df$Variable <- rownames(importancia_df)

ggplot(importancia_df, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = 50, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_classic()+
  labs(title = "Importancia 02-RF_cv_M1-250924", x = "Variables", y = "Importancia")

