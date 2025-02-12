
#### PRUEBAS DE PARTICION  DE DATOS

rm(list=ls())
# Generar datos aleatorios para las variables predictoras
set.seed(42)
estacion <- "MX"
data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/proceed/merge_tot/",estacion,"_merge_comp.csv",sep=""))

data_completo <- data[complete.cases(data),]
## Agreego la variable numero de dias
data_completo$date <- strptime(data_completo$date, format = "%Y-%m-%d")
data_completo$dayWeek <- wday(data_completo$date, week_start = 1)

# Sin 2024 que lo usamos para verificar despues con los mapas
data_completo <- data_completo[year(data_completo$date) != 2024,]
# verifficamos que solo esten los años 2015-2023
unique(year(data_completo$date))
###### ------  Modelo 1 - Aleatorio  ------  ##### 

# Dividir el dataframe en 70% entrenamiento y 30% testeo
train_index <- createDataPartition(data_completo$PM25, p = 0.7, list = FALSE)
train_data <- data_completo[train_index, ]
test_data <- data_completo[-train_index, ]
dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/",sep="")
setwd(dir)
getwd()
write.csv(train_data, paste(dir,"Modelo_1/M1_train_",estacion,".csv",sep=""))
write.csv(test_data, paste(dir,"Modelo_1/M1_test_",estacion,".csv",sep=""))

################################################################
# Modelo 2 - para SP hago otra separacion aleatoria
data_completo_2<- data_completo[data_completo$AOD_055>0.1,]
train_index <- createDataPartition(data_completo_2$PM25, p = 0.7, list = FALSE)
train_data <- data_completo_2[train_index, ]
test_data <- data_completo_2[-train_index, ]

dir <- "D:/Josefina/Proyectos/ProyectoChile/SP/modelos/ParticionDataSet/"
setwd(dir)
getwd()
write.csv(train_data, paste(dir,"Modelo 2/M2_train.csv",sep=""))
write.csv(test_data, paste(dir,"Modelo 2/M2_test.csv",sep=""))

################################################################
# Modelo 2 - por a?osos
# Vemos cuanto datos hay por año
year = 2024
len_data <- nrow(data_completo[year(data_completo$date) == year ,])
len_data        #ST    #SP  
# Para 2015 ==> 1584   643
# Para 2016 ==> 1558   765
# Para 2017 ==> 1567   1398
# Para 2018 ==> 1930   1489
# Para 2019 ==> 1917   1681   
# Para 2020 ==> 2175   1812
# Para 2021 ==> 2127   1661
# Para 2022 ==> 1993   1539
# Para 2023 ==> 1784   1679
# Para 2024 ==> 0
# Total = 16635
# 2021-2023 testeo 35.5% ----- 2015-2020 Entrenamiento 64.5% ST
# 2021-2023 testeo 38.52% ----- 2015-2020 Entrenamiento 61.48% SP
# Dividimos los set de datos segun los años
train_data <- data_completo[year(data_completo$date) == 2015 | year(data_completo$date) == 2016 |
                            year(data_completo$date) == 2017 | year(data_completo$date) == 2018 |
                              year(data_completo$date) == 2019 | year(data_completo$date) == 2020
                            | year(data_completo$date) == 2021 ,]
test_data <- data_completo[year(data_completo$date) == 2022 |
                            year(data_completo$date) == 2023|year(data_completo$date) == 2024,]
# Corroboramos que esten todos los datos
nrow(train_data) + nrow(test_data) == nrow(data_completo)

# Guardamos datos
dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/",sep="")
setwd(dir)
write.csv(train_data, paste(dir,"Modelo_2/M2_train.csv",sep=""))
write.csv(test_data, paste(dir,"Modelo_2/M2_test.csv",sep=""))

################################################################
# Modelo 3 - por estacion
unique(data_completo$ID)
# Para MX 
# Vemos cuanto datos hay por año
# estacion  = "QUI"
estacion <- "6"
# len_data <- nrow(data_completo[data_completo$estacion == estacion ,])
len_data <- nrow(data_completo[data_completo$ID == estacion ,])
#unique(data_completo$estacion) #"OHG" "BSQ" "CNA" "PDH" "FLD" "PTA" "CDE" "QUI"
len_data

## ST
# BSQ ==> 2119
# OHG ==> 2140
# CNA ==> 2182
# PDH ==> 2134
# FLD ==> 1961
# PTA ==> 2140
# CDE ==> 2084
# QUI ==> 1875
##### SP
#ID   len
 # 1     5
 # 2     0
 # 3     1323
 # 4     0
 # 5     0
 # 6     1153
 # 7     1177
 # 8     5
 # 9     1056
 # 10    0
 # 11    1419
 # 12    627
 # 13    0
 # 14    0
 # 15    0
 # 16    1156
 # 17    829
 # 18    981
 # 19    3
 # 20    0
 # 21    4
 # 22    984
 # 23    971
 # 24    973

#Estaciones ID 3 1323, ID 6: 1153, ID:7 1177 TESTEO ==> 28.8%

# Dejamos para testeo OHG que es la estacion del centro

# train_data <- data_completo[data_completo$estacion != "OHG",]
# test_data <- data_completo[data_completo$estacion == "OHG",]

# train_data <- data_completo[data_completo$estacion != "EMB",]
# test_data <- data_completo[data_completo$estacion == "EMB",]
# Esto es para MX
train_data <- data_completo[data_completo$ID == 1 | data_completo$ID == 2 | data_completo$ID == 3 
                            | data_completo$ID == 4| data_completo$ID == 5 | data_completo$ID == 6
                            | data_completo$ID == 7  | data_completo$ID == 8 | data_completo$ID == 9
                            | data_completo$ID == 10 | data_completo$ID == 11 | data_completo$ID == 12
                            | data_completo$ID == 13 | data_completo$ID == 14 | data_completo$ID == 15 ,]
test_data <- data_completo[ data_completo$ID == 16 | data_completo$ID == 17 | data_completo$ID == 18
                            | data_completo$ID == 19 | data_completo$ID == 20 | data_completo$ID == 21
                            | data_completo$ID == 22 ,]

unique(data_completo$ID) # MD 12 28 38 69 78 79 80 81 82 83 84 85 86 87 88 90 94
# MD
# 12  614
# 28 597
# 38 475
# 69 261
# 78 329
# 79 847
# 80 356
# 81 571
# 82 537
# 83 519
# 84 335
# 85 375
# 86 327
# 87 564
# 88 351
# 90 369
# 94 261

# Esto es para MX

test_data <- data_completo[data_completo$ID == 12 | data_completo$ID == 28 
                            | data_completo$ID == 38| data_completo$ID == 69 
                             ,]
train_data  <- data_completo[ data_completo$ID == 78 |data_completo$ID == 79 | data_completo$ID == 80
                            | data_completo$ID == 81| data_completo$ID == 82
                            | data_completo$ID == 83 | data_completo$ID == 84
                            | data_completo$ID == 85 | data_completo$ID == 86
                            | data_completo$ID == 87 | data_completo$ID == 88
                            | data_completo$ID == 90 | data_completo$ID == 94,]

nrow (data_completo[data_completo$ID == 94,])
# Corroboramos que esten todos los datos
nrow(train_data) + nrow(test_data) == nrow(data_completo)

# Corroboramos que esten todos los datos         ST  SP      MX       MD
(nrow(train_data) / nrow(data_completo))* 100 # 87%  71.16%  75.43% #73.27
(nrow(test_data) / nrow(data_completo))*100   # 13%  28.84%  24.57%  #26.73

#Para BA
## BA
# 1 1617
# 2 163
# 3 69 #TEST
# 4 858
# 5 340 #TEST
# 6 409 #TEST
test_data <- data_completo[data_completo$ID == 3 
                           | data_completo$ID == 5 
                           | data_completo$ID == 6,]
train_data  <- data_completo[ data_completo$ID == 1 
                              |data_completo$ID == 2 
                              | data_completo$ID == 4,]




# Guardamos datos
estacion <- "BA"
dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/",sep="")
setwd(dir)

getwd()
write.csv(train_data, paste(dir,"Modelo_3/M3_train_",estacion,".csv",sep=""))
write.csv(test_data, paste(dir,"Modelo_3/M3_test_",estacion,".csv",sep=""))

################################################################
# Modelo 4 - por estacion pero dejamos de lado otra estacion BSQ
# Dejamos para testeo BSQ
# 
# train_data <- data_completo[data_completo$estacion != "BSQ",]
# test_data <- data_completo[data_completo$estacion == "BSQ",]


# SP  ESTACIONES ID 22,23,24 17 para testeo

train_data <- data_completo[data_completo$ID == 1 | data_completo$ID == 2 | data_completo$ID == 3 
                            | data_completo$ID == 4| data_completo$ID == 5 | data_completo$ID == 6 
                            | data_completo$ID == 7 |data_completo$ID == 8 | data_completo$ID == 9
                            | data_completo$ID == 10 | data_completo$ID == 11 | data_completo$ID == 12
                            | data_completo$ID == 13 | data_completo$ID == 14 | data_completo$ID == 15
                            | data_completo$ID == 16 |  data_completo$ID == 18
                            | data_completo$ID == 19 | data_completo$ID == 20 | data_completo$ID == 21
                            ,]
test_data <- data_completo[data_completo$ID == 22 | data_completo$ID == 23 
                           | data_completo$ID == 24| data_completo$ID == 17  ,]

train_data <- data_completo[data_completo$ID == 1 | data_completo$ID == 2 | data_completo$ID == 5 
                            | data_completo$ID == 4| data_completo$ID == 6,]
test_data <- data_completo[data_completo$ID == 3 ,]                          
# Corroboramos que esten todos los datos
nrow(train_data) + nrow(test_data) == nrow(data_completo)

# Corroboramos que esten todos los datos        ST   SP
(nrow(train_data) / nrow(data_completo))* 100 # 87%  70.74%
(nrow(test_data) / nrow(data_completo))*100   # 13%  29.66%
# Guardamos datos
dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/ParticionDataSet/",sep="")

setwd(dir)
write.csv(train_data, paste(dir,"Modelo_4/M4_train_",estacion,".csv",sep=""))
write.csv(test_data, paste(dir,"Modelo_4/M4_test_",estacion,".csv",sep=""))

write.csv(train_data, paste(dir,"Modelo_3/M3_train_",estacion,".csv",sep=""))
write.csv(test_data, paste(dir,"Modelo_3/M3_test_",estacion,".csv",sep=""))

###### ------  Modelo 5 - Aleatorio eliminando outliers segun IQX  ------  ##### 
# Dividir el dataframe en 70% entrenamiento y 30% testeo
## Segun Bagheri 2022
# Calcular el primer cuartil (Q1)
Q1 <- quantile(data_completo$PM25, 0.25)

# Calcular el tercer cuartil (Q3)
Q3 <- quantile(data_completo$PM25, 0.75)

# Calcular el rango intercuart?lico (IQR)
IQR_value <- IQR(data_completo$PM25)

# Filtrar los datos que cumplen la condici?n Q1 - IQR < AOD550 < Q3 + IQR
data_completo_filtered <- data_completo[data_completo$PM25 > (Q1 - IQR_value) & data_completo$PM25 < (Q3 + IQR_value), ]

# Imprimir los resultados
cat("Primer cuartil (Q1):", Q1, "\n")
cat("Tercer cuartil (Q3):", Q3, "\n")
hist(data_completo$PM25)
hist(data_completo_filtered$PM25)


train_index <- createDataPartition(data_completo_filtered$PM25, p = 0.7, list = FALSE)
train_data <- data_completo_filtered[train_index, ]
test_data <- data_completo_filtered[-train_index, ]
dir <- "D:/Josefina/Proyectos/ProyectoChile/MD/modelos/ParticionDataSet/"
setwd(dir)
write.csv(train_data, paste(dir,"Modelo_5/M5_train_",estacion,".csv",sep=""))
write.csv(test_data, paste(dir,"Modelo_5/M5_test_",estacion,".csv",sep=""))


###### ------  Modelo 6 - Aleatorio eliminando outliers segun sd  ------  ##### 
# Dividir el dataframe en 70% entrenamiento y 30% testeo

# Calcular la media (?)
mean_value <- mean(data_completo$PM25)

# Calcular la desviaci?n est?ndar (??)
sd_value <- sd(data_completo$PM25)


# Calcular la puntuaci?n Z
data_completo$z_score <- (data_completo$PM25 - mean_value) / sd_value

# Filtrar los datos que tengan un Z-score entre -3 y 3 (datos dentro de 3 desviaciones est?ndar)
data_completo_filtered <- data_completo[abs(data_completo$z_score) < 3, ]



train_index <- createDataPartition(data_completo_filtered$PM25, p = 0.7, list = FALSE)
train_data <- data_completo_filtered[train_index, ]
test_data <- data_completo_filtered[-train_index, ]
dir <- "D:/Josefina/Proyectos/ProyectoChile/modelos/ParticionDataSet/"
setwd(dir)
write.csv(train_data, paste(dir,"Modelo_5/M5_train.csv",sep=""))
write.csv(test_data, paste(dir,"Modelo_5/M5_test.csv",sep=""))

