
#### PRUEBAS DE PARTICION  DE DATOS

rm(list=ls())
# Generar datos aleatorios para las variables predictoras
set.seed(42)
data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/merge_tot/09_TOT_merge_tot.csv")
# data_completo <- data[complete.cases(data),]
data_completo <- data[complete.cases(data$PM25_Completo),]
data_completo <- data_completo %>%
  dplyr::select(-PM25)
#data_completo2 <- data[complete.cases(data),]
data_completo$date <- strptime(data_completo$date, format = "%Y-%m-%d")
data_completo$dayWeek <- wday(data_completo$date, week_start = 1)

###### ------  Modelo 1 - Aleatorio  ------  ##### 
# Dividir el dataframe en 70% entrenamiento y 30% testeo
train_index <- createDataPartition(data_completo$PM25, p = 0.7, list = FALSE)
train_data <- data_completo[train_index, ]
test_data <- data_completo[-train_index, ]
dir <- "D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/"
setwd(dir)

write.csv(train_data, paste(dir,"Modelo 6/M6_train.csv",sep=""))
write.csv(test_data, paste(dir,"Modelo 6/M6_test.csv",sep=""))


################################################################
# Modelo 2 - por a√±os
# Vemos cuanto datos hay por a√±o
year = 2024
len_data <- nrow(data_completo[year(data_completo$date) == year ,])
len_data
# Para 2015 ==> 1584
# Para 2016 ==> 1558
# Para 2017 ==> 1567
# Para 2018 ==> 1930
# Para 2019 ==> 1917
# Para 2020 ==> 2175
# Para 2021 ==> 2127
# Para 2022 ==> 1993
# Para 2023 ==> 1784
# Para 2024 ==> 0
# Total = 16635
# 2021-2023 testeo 35.5% ----- 2015-2020 Entrenamiento 64.5%
# Dividimos los set de datos segun los a√±os
train_data <- data_completo[year(data_completo$date) == 2015 | year(data_completo$date) == 2016 |
                            year(data_completo$date) == 2017 | year(data_completo$date) == 2018 |
                              year(data_completo$date) == 2019 | year(data_completo$date) == 2020,]
test_data <- data_completo[year(data_completo$date) == 2021 | year(data_completo$date) == 2022 |
                            year(data_completo$date) == 2023,]
# Corroboramos que esten todos los datos
nrow(train_data) + nrow(test_data) == nrow(data_completo)

# Guardamos datos
dir <- "D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/"
setwd(dir)
write.csv(train_data, paste(dir,"Modelo 2/M2_train.csv",sep=""))
write.csv(test_data, paste(dir,"Modelo 2/M2_test.csv",sep=""))

################################################################
# Modelo 3 - por estacion
# Vemos cuanto datos hay por a√±o
estacion  = "QUI"
len_data <- nrow(data_completo[data_completo$estacion == estacion ,])
unique(data_completo$estacion) #"OHG" "BSQ" "CNA" "PDH" "FLD" "PTA" "CDE" "QUI"
len_data
# BSQ ==> 2119
# OHG ==> 2140
# CNA ==> 2182
# PDH ==> 2134
# FLD ==> 1961
# PTA ==> 2140
# CDE ==> 2084
# QUI ==> 1875

# Dejamos para testeo OHG que es la estacion del centro

train_data <- data_completo[data_completo$estacion != "OHG",]
test_data <- data_completo[data_completo$estacion == "OHG",]
                             
# Corroboramos que esten todos los datos
nrow(train_data) + nrow(test_data) == nrow(data_completo)

# Corroboramos que esten todos los datos
(nrow(train_data) / nrow(data_completo))* 100 # 87%
(nrow(test_data) / nrow(data_completo))*100 # 13%
# Guardamos datos
dir <- "D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/"
setwd(dir)
write.csv(train_data, paste(dir,"Modelo 3/M3_train.csv",sep=""))
write.csv(test_data, paste(dir,"Modelo 3/M3_test.csv",sep=""))

################################################################
# Modelo 4 - por estacion pero dejamos de lado otra estacion BSQ
# Dejamos para testeo BSQ

train_data <- data_completo[data_completo$estacion != "BSQ",]
test_data <- data_completo[data_completo$estacion == "BSQ",]

# Corroboramos que esten todos los datos
nrow(train_data) + nrow(test_data) == nrow(data_completo)

# Corroboramos que esten todos los datos
(nrow(train_data) / nrow(data_completo))* 100 # 87%
(nrow(test_data) / nrow(data_completo))*100 # 13%
# Guardamos datos
dir <- "D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/"
setwd(dir)
write.csv(train_data, paste(dir,"Modelo 4/M4_train.csv",sep=""))
write.csv(test_data, paste(dir,"Modelo 4/M4_test.csv",sep=""))

###### ------  Modelo 5 - Aleatorio eliminando outliers segun IQX  ------  ##### 
# Dividir el dataframe en 70% entrenamiento y 30% testeo
## Segun Bagheri 2022
# Calcular el primer cuartil (Q1)
Q1 <- quantile(data_completo$PM25, 0.25)

# Calcular el tercer cuartil (Q3)
Q3 <- quantile(data_completo$PM25, 0.75)

# Calcular el rango intercuartÌlico (IQR)
IQR_value <- IQR(data_completo$PM25)

# Filtrar los datos que cumplen la condiciÛn Q1 - IQR < AOD550 < Q3 + IQR
data_completo_filtered <- data_completo[data_completo$PM25 > (Q1 - IQR_value) & data_completo$PM25 < (Q3 + IQR_value), ]

# Imprimir los resultados
cat("Primer cuartil (Q1):", Q1, "\n")
cat("Tercer cuartil (Q3):", Q3, "\n")
hist(data_completo$PM25)
hist(data_completo_filtered$PM25)


train_index <- createDataPartition(data_completo_filtered$PM25, p = 0.7, list = FALSE)
train_data <- data_completo_filtered[train_index, ]
test_data <- data_completo_filtered[-train_index, ]
dir <- "D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/"
setwd(dir)
write.csv(train_data, paste(dir,"Modelo 5/M5_train.csv",sep=""))
write.csv(test_data, paste(dir,"Modelo 5/M5_test.csv",sep=""))


###### ------  Modelo 6 - Aleatorio eliminando outliers segun sd  ------  ##### 
# Dividir el dataframe en 70% entrenamiento y 30% testeo

# Calcular la media (µ)
mean_value <- mean(data_completo$PM25)

# Calcular la desviaciÛn est·ndar (??)
sd_value <- sd(data_completo$PM25)


# Calcular la puntuaciÛn Z
data_completo$z_score <- (data_completo$PM25 - mean_value) / sd_value

# Filtrar los datos que tengan un Z-score entre -3 y 3 (datos dentro de 3 desviaciones est·ndar)
data_completo_filtered <- data_completo[abs(data_completo$z_score) < 3, ]



train_index <- createDataPartition(data_completo_filtered$PM25, p = 0.7, list = FALSE)
train_data <- data_completo_filtered[train_index, ]
test_data <- data_completo_filtered[-train_index, ]
dir <- "D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/"
setwd(dir)
write.csv(train_data, paste(dir,"Modelo 5/M5_train.csv",sep=""))
write.csv(test_data, paste(dir,"Modelo 5/M5_test.csv",sep=""))