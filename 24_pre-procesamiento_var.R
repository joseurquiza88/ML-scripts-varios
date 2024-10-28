
### Pre-procesamiento de variables segun Bagheri et al., 2022

test_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 1/M1_test.csv")
train_data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/Modelo 1/M1_train.csv")

test_data$AOD_norm <- test_data$AOD_055 /test_data$blh_mean
train_data$AOD_norm <- train_data$AOD_055 /train_data$blh_mean
df <- data.frame(aod=test_data$AOD_055, aod_nom=test_data$AOD_norm)
