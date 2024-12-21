

library(raster)

# Definir el directorio donde están tus archivos raster
rm(list = ls())
#dir_salida <- "D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/Prediccion_01-2024/Salida/Salida_02-XGB_cv_M4-300924/"
month <- c("01","02","03","04","05","06","07","08","09","10")#,"11","12")
year <- "2024"
i<-1
month <- c("01")
estacion <- "BA"
for (i in 1:length(month)){
  print(i)
  #dir_salida <- paste("D:/Josefina/Proyectos/ProyectoChile/modelos/Salidas/SalidasDiarias/Salida_03-XGB_cv_M1-041024/",year,"/",month[i],sep="")
  dir_salida <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/salidas/SalidasDiarias/Salida_02-RF_cv_M1-171224_BA/",year,"/",month[i],sep="")
  setwd(dir_salida)
  
  # Lista de archivos raster (ajusta la extensión según sea necesario)
  lista_raster <- list.files(pattern = "*.tif") # Cambia la extensión si es necesario
  lista_raster_recorte <- lista_raster
  #lista_raster_recorte
  len <- length(lista_raster_recorte)
  print(c(month[i],len))
  # Cargar los rasters en un RasterStack
  raster_stack <- stack(lista_raster_recorte)
  
  # Calcular el promedio mensual y desviacion estandar
  promedio_mensual <- calc(raster_stack, fun = mean, na.rm = TRUE)
  #sd_mensual <- calc(raster_stack, fun = sd, na.rm = TRUE)
  # Calcular el coeficiente de variacion
  #coef_Var <- (sd_mensual / promedio_mensual) * 100
  #plot(promedio_mensual)
  modelo <- substr(lista_raster[1],15,35)
  # Guardar el resultado en un nuevo archivo raster
  #dir_salida <- "D:/Josefina/Proyectos/ProyectoChile/modelos/Salidas/SalidasMensuales/Salida_03-XGB_cv_M1-041024/"
  #dir_salida <- "D:/Josefina/Proyectos/ProyectoChile/modelos/Salidas/SalidasMensuales/Salida_03-XGB_cv_M1-041024/Coef_Var/"
  dir_salida <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/salidas/SalidasMensuales/Salida_02-RF_cv_M1-171224_BA/",year,"/",sep="")
  
  writeRaster(promedio_mensual, filename = paste(dir_salida,"mensual_",month[i],"-",year,"-",modelo,".tif",sep=""), format = "GTiff", overwrite = TRUE)
}
###########################################################################
############################################################################
data_estaciones_2024 <- read.csv("D:/Josefina/Proyectos/ProyectoChile/dataset/estaciones/diarios/PM25_2024_tot_validados.csv")
data_estaciones_2024$date <- as.POSIXct(as.character(data_estaciones_2024$date), format = "%y%m%d")

data_estaciones_2024$validados


# Instalar y cargar el paquete dplyr si no está instalado
# install.packages("dplyr")
library(dplyr)

# Leer el archivo CSV
data_estaciones_2024 <- read.csv("D:/Josefina/Proyectos/ProyectoChile/dataset/estaciones/diarios/PM25_2024_tot_validados.csv")
data_estaciones_2024 <- read.csv("D:/Josefina/Proyectos/ProyectoChile/dataset/estaciones/diarios/PM25_tot.csv")
data_estaciones_2024 <- read.csv("D:/Josefina/Proyectos/ProyectoChile/dataset/estaciones/diarios/PM25_tot.csv",colClasses = c("FECHA..YYMMDD."  = "character"))


# Convertir la columna 'date' a formato POSIXct
data_estaciones_2024$date <- as.POSIXct(as.character(data_estaciones_2024$FECHA..YYMMDD.), format = "%y%m%d")
data_estaciones_2024 <- data_estaciones_2024[year(data_estaciones_2024$date) >= 2024,]
data_estaciones_2024 <- data_estaciones_2024[complete.cases(data_estaciones_2024$estacion),]
# Agregar una columna de año y mes
data_estaciones_2024 <- data_estaciones_2024 %>%
  mutate(year = format(date, "%Y"),
         month = format(date, "%m"))

# Calcular promedios mensuales por estación
promedios_mensuales <- data_estaciones_2024 %>%
  group_by(estacion, year, month) %>%     # Agrupar por estación, año y mes
  summarise(promedio_validados = mean(Registros.completos, na.rm = TRUE)) %>% # Calcular promedio ignorando NA
  ungroup()                                # Desagrupar

# Convertir el dataframe a formato más limpio (opcional)
# promedios_mensuales <- promedios_mensuales %>%
#   mutate(fecha = as.Date(paste(year, month, "01", sep = "-"))) %>% # Crear una fecha a partir del año y mes
#   select(estacion, fecha, promedio_validados) # Seleccionar columnas de interés

# Mostrar el dataframe resultante
print(promedios_mensuales)

# Instalar y cargar el paquete ggplot2 si no está instalado
# install.packages("ggplot2")
library(ggplot2)

# Asegúrate de que el dataframe 'promedios_mensuales' esté en el formato correcto
# Convertir el mes a un factor para asegurarnos de que se ordene correctamente
promedios_mensuales$month2 <- factor(promedios_mensuales$month, levels = 1:12, 
                                    labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                                               "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))
promedios_mensuales$yearMonth <- paste(promedios_mensuales$month,"-",promedios_mensuales$year,sep="")
library(ggplot2)
library(lubridate)

# Convertir la columna yearMonth a un formato Date si es un string
library(lubridate)
promedios_mensuales <- promedios_mensuales[promedios_mensuales$estacion]
# Supongamos que la columna yearMonth tiene el formato "MM-YYYY"
promedios_mensuales$yearMonth2 <- as.Date(paste0("01-", promedios_mensuales$yearMonth), format = "%d-%m-%Y")
promedios_mensuales <- promedios_mensuales[promedios_mensuales$estacion != "TLG",]
# Ahora la columna yearMonth está en formato de fecha Date
# Si la columna ya está en formato correcto pero es un factor, puedes usar factor(levels=unique())

# Crear el gráfico yearMonth
ggplot(promedios_mensuales, aes(x = month, y = promedio_validados, color = estacion, group = estacion)) +
  geom_line(size = 0.5) +                    # Agregar líneas
  geom_point(size = 1.5) +                   # Agregar puntos en las líneas
  labs(title = "Promedios Mensuales de PM2.5 por Estación", 
       x = "Mes", 
       y = "Promedio PM2.5 (validados)") +
  theme_classic() + 
  scale_y_continuous(limits = c(0, 60),breaks = seq(0, 60, by = 20)) +  # Ticks cada 10 en el eje Y
  
  # Usar un tema minimalista
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotar los nombres de los meses
  scale_color_discrete(name = "Estación")  # Cambiar el título de la leyenda



library(ggplot2)
library(scales)  # Para usar date_breaks y date_labels

plot <- ggplot(promedios_mensuales, aes(x = yearMonth2, y = promedio_validados, color = estacion, group = estacion)) +
  geom_line(size = 0.5) +                    # Agregar líneas
  geom_point(size = 1.5) +                   # Agregar puntos en las líneas
  labs(title = "Promedios Mensuales de PM2.5 por Estación", 
       x = "Mes", 
       y = "Promedio PM2.5 (ug/m3)") +
  theme_classic() + 
  # scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 20)) +  # Ticks cada 20 en el eje Y
  
  # Asegurar que aparezcan todos los ticks de fechas en el eje X
  scale_x_date(date_breaks = "1 month", date_labels = "%m-%Y") +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotar los nombres de los meses
  scale_color_discrete(name = "Estación")  # Cambiar el título de la leyenda

# Guardar el gráfico en formato PNG
ggsave("D:/Josefina/Proyectos/ProyectoChile/plots/SeriesTemporales/SerieTemp_SINCA_2023-2024.png", plot = plot, width = 10, height = 6, dpi = 300)



#########################################3
setwd("D:/Josefina/Proyectos/ProyectoChile/modelos/Salidas/SalidasMensuales/Salida_03-XGB_cv_M1-041024/Coef_Var/")
lista_raster <- list.files(pattern = "*.tif") 

df_rbind <- data.frame()
for (x in 1:length(lista_raster)){
  print(x)
  data <- raster(lista_raster[x])
  name <- substr(lista_raster[x],17,23)
  mean <- cellStats(data, stat = 'mean', na.rm = TRUE)
  df <- data.frame(name=name, mean = mean)
  df_rbind <- rbind(df_rbind,df)
}
df_rbind$date <- as.Date(paste0("01-", df_rbind$name), format = "%d-%m-%Y")
ggplot(df_rbind) +
  geom_col(mapping = aes(x = date, y = mean, fill = date))+ theme_classic()


library(lubridate)
library(ggplot2)

# Asegúrate de que la columna `date` esté en formato Date
df_rbind$date <- as.Date(df_rbind$date)

# Crear el gráfico con colores según el año
ggplot(df_rbind) +
  geom_col(mapping = aes(x = date, y = mean, fill = factor(year(date)))) +
  theme_classic() +
  scale_y_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 20)) +  # Ticks cada 10 en el eje Y
  labs(fill = "Año",y= "Coef Var (%)", title = "03-XGB_cv_M1-041024")  # Agregar etiqueta para la leyenda
