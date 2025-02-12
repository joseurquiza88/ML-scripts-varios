
#Leer datos de MX
year<-2024
####años
for (i in 1:1){
  data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/MX/dataset/estaciones/process/",year,"PM25.csv",sep=""),na.strings = -99)
  names(data)
  data$date <- as.POSIXct( strptime (data$FECHA, format = "%d/%m/%Y"))
  
  data_comp <- data[complete.cases(data),]
  
  
  # Agrupar por la columna 'estacion' y calcular las estad?sticas
  df <- data_comp %>%
    group_by(date,estacion) %>%
    summarise( 
      # Primer valor de unidad
      mean = mean(PM25, na.rm = TRUE),               # Media
      min = min(PM25, na.rm = TRUE),                 # M?nimo
      max = max(PM25, na.rm = TRUE),                 # M?ximo
      sd = sd(PM25, na.rm = TRUE)
      # Desviaci?n est?ndar
      #,                         # Conteo de valores no NA
      #mean_subt = mean(data_subt$valor[data_subt$estacion == unique(estacion)], na.rm = TRUE) # Media para horas espec?ficas
    )
  
  
  # Uniion con dataframe generado
  
  data_estaciones <- read.csv("D:/Josefina/Proyectos/ProyectoChile/MX/dataset/estaciones/sitios_MX.csv")#,fileEncoding = "UTF-8")
  
  df_merged <- merge(data_estaciones, df, by = "estacion")
  
  
  df_estaciones <- data.frame(date=df_merged$date,ID = df_merged$ID,
                              estacion = df_merged$estacion,mean = df_merged$mean,
                              min = df_merged$min,max = df_merged$max, sd =df_merged$sd)
  # View(df_estaciones)  
  
  
  
  write.csv(df_estaciones,paste("D:/Josefina/Proyectos/ProyectoChile/MX/dataset/estaciones/process/",year,"PM25_process.csv",sep=""))
}


#### mes 2024

dir <- "D:/Josefina/Proyectos/ProyectoChile/MX/dataset/estaciones/process/2024/"

setwd(dir)

# Lista de archivos raster (ajusta la extensión según sea necesario)
lista_raster <- list.files(pattern = "*.csv") # Cambia la extensión si es necesario
df_estaciones_rbind <- data.frame()
i<-1
for (i in 1:length(lista_raster)){
  print(i)
  data <- read.csv(lista_raster[i],na.strings = "nr")
  names(data)
  data$date <- as.POSIXct( strptime (data$FECHA, format = "%d/%m/%Y"))
  
  data_comp <- data[complete.cases(data),]
  
  
  # Agrupar por la columna 'estacion' y calcular las estad?sticas
  df <- data_comp %>%
    group_by(date,estacion) %>%
    summarise( 
      # Primer valor de unidad
      mean = mean(PM25, na.rm = TRUE),               # Media
      min = min(PM25, na.rm = TRUE),                 # M?nimo
      max = max(PM25, na.rm = TRUE),                 # M?ximo
      sd = sd(PM25, na.rm = TRUE)
      # Desviaci?n est?ndar
      #,                         # Conteo de valores no NA
      #mean_subt = mean(data_subt$valor[data_subt$estacion == unique(estacion)], na.rm = TRUE) # Media para horas espec?ficas
    )
  
  
  # Uniion con dataframe generado
  
  data_estaciones <- read.csv("D:/Josefina/Proyectos/ProyectoChile/MX/dataset/estaciones/sitios_MX.csv")#,fileEncoding = "UTF-8")
  
  df_merged <- merge(data_estaciones, df, by = "estacion")
  
  
  df_estaciones <- data.frame(date=df_merged$date,ID = df_merged$ID,
                              estacion = df_merged$estacion,mean = df_merged$mean,
                              min = df_merged$min,max = df_merged$max, sd =df_merged$sd)
  # View(df_estaciones)  
  df_estaciones_rbind <- rbind(df_estaciones_rbind,df_estaciones)
  
  
  #write.csv(df_estaciones,paste("D:/Josefina/Proyectos/ProyectoChile/MX/dataset/estaciones/process/",year,"PM25_process.csv",sep=""))
}

write.csv(df_estaciones_rbind,"D:/Josefina/Proyectos/ProyectoChile/MX/dataset/estaciones/process/2024PM25_process.csv")


##### MERGE todos los años

data_2015 <- read.csv("D:/Josefina/Proyectos/ProyectoChile/MX/dataset/estaciones/process/2015PM25_process.csv")
data_2016 <- read.csv("D:/Josefina/Proyectos/ProyectoChile/MX/dataset/estaciones/process/2016PM25_process.csv")
data_2017 <- read.csv("D:/Josefina/Proyectos/ProyectoChile/MX/dataset/estaciones/process/2017PM25_process.csv")
data_2018 <- read.csv("D:/Josefina/Proyectos/ProyectoChile/MX/dataset/estaciones/process/2018PM25_process.csv")
data_2019 <- read.csv("D:/Josefina/Proyectos/ProyectoChile/MX/dataset/estaciones/process/2019PM25_process.csv")
data_2020 <- read.csv("D:/Josefina/Proyectos/ProyectoChile/MX/dataset/estaciones/process/2020PM25_process.csv")
data_2021 <- read.csv("D:/Josefina/Proyectos/ProyectoChile/MX/dataset/estaciones/process/2021PM25_process.csv")
data_2022 <- read.csv("D:/Josefina/Proyectos/ProyectoChile/MX/dataset/estaciones/process/2022PM25_process.csv")
data_2023 <- read.csv("D:/Josefina/Proyectos/ProyectoChile/MX/dataset/estaciones/process/2023PM25_process.csv")
data_2024 <- read.csv("D:/Josefina/Proyectos/ProyectoChile/MX/dataset/estaciones/process/2024PM25_process.csv")


df_data <- rbind(data_2015,data_2016, data_2017, data_2018, 
                 data_2019, data_2020, data_2021, data_2022,
                 data_2023,data_2024)
write.csv(df_data,"D:/Josefina/Proyectos/ProyectoChile/MX/proceed/06_estaciones/MX_estaciones.csv")
df_data$date <- as.POSIXct( strptime (df_data$date, format = "%Y-%m-%d"))
write.csv(data_comp,)
data_comp <- df_data[complete.cases(df_data),]
ggplot(data_comp, aes(x = date)) +
  # Línea para Registros.validados
  geom_line(aes(y = mean, color = "mean"), size = 0.3,na.rm = TRUE) +
  
  # Separar en subplots por estación
  facet_wrap(~ data_comp$estacion, scales = "free_y") +
  # 
  scale_y_continuous(limits = c(0,100),breaks = seq(0, 100, by = 20)) +  # Ticks cada 10 en el eje Y
  
  # Títulos y etiquetas
  labs(#title = "Modelo Salida_03-XGB_cv_M1-041024",
    x = "Date",
    y = "PM2.5",
    color = "Variables") +
  # Cambiar los colores de las líneas
  scale_color_manual(values = c("mean" = "#2ca25f")) +
  
  # Personalización del tema
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



  