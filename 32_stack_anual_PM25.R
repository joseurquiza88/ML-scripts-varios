

library(raster)

# Definir el directorio donde están tus archivos raster
year<-2024
year <- c(2015,2016,2017,2018,2019,2020,2021,2022,2023)
i<-1
modelo <- "Salida_03-XGB_cv_M1-041024"
estacion <- "CH"
for (i in 1:length(year)){
  #dir_salida <- paste("D:/Josefina/Proyectos/ProyectoChile/modelos/Salidas/SalidasDiarias/Salida_03-XGB_cv_M1-041024/",year[i],"/",sep="")
  #dir_salida <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion"/modelos/salidas/SalidasDiarias/",modelo,"/",year[i],"/",sep="")
  dir_salida <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/salidas/SalidasDiarias/",modelo,"/",year,"/",sep="")
  
  setwd(dir_salida)
  
  # Lista de archivos raster (ajusta la extensión según sea necesario)
  lista_raster <- list.files(pattern = "*.tif") # Cambia la extensión si es necesario
  lista_raster_recorte <- lista_raster
  #lista_raster_recorte
  len <- length(lista_raster_recorte)
  print(c(year[i],len))
  # Cargar los rasters en un RasterStack
  raster_stack <- stack(lista_raster_recorte)
  
  # Calcular el promedio anual
  promedio_anual <- calc(raster_stack, fun = mean, na.rm = TRUE)
  #sd_anual <- calc(raster_stack, fun = sd, na.rm = TRUE)
  # Calcular el coeficiente de variacion
  #coef_Var <- (sd_anual / promedio_anual) * 100
  #plot(promedio_anual)
  modelo2 <- substr(lista_raster[1],18,35)
  # Guardar el resultado en un nuevo archivo raster
  # dir_salida <- "D:/Josefina/Proyectos/ProyectoChile/modelos/Salidas/SalidasAnuales/Salida_03-XGB_cv_M1-041024/"
  dir_salida <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/Salidas/SalidasAnuales/",modelo,"/",sep="")
  writeRaster(promedio_anual, filename = paste(dir_salida,"Promedio_anual_",year[i],"-",modelo,".tif",sep=""), format = "GTiff", overwrite = TRUE)
}

writeRaster(promedio_anual, filename = paste(dir_salida,"Promedio_anual_2024_01-07","-",modelo2,".tif",sep=""), format = "GTiff", overwrite = TRUE)


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
data_estaciones_2024 <- data_estaciones_2024[year(data_estaciones_2024$date) >= 2023,]
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
promedios_mensuales <- promedios_mensuales %>%
  mutate(fecha = as.Date(paste(year, month, "01", sep = "-"))) %>% # Crear una fecha a partir del año y mes
  select(estacion, fecha, promedio_validados) # Seleccionar columnas de interés

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

# Supongamos que la columna yearMonth tiene el formato "MM-YYYY"
promedios_mensuales$yearMonth2 <- as.Date(paste0("01-", promedios_mensuales$yearMonth), format = "%d-%m-%Y")
promedios_mensuales <- promedios_mensuales[promedios_mensuales$estacion != "TLG",]
# Ahora la columna yearMonth está en formato de fecha Date
# Si la columna ya está en formato correcto pero es un factor, puedes usar factor(levels=unique())

# Crear el gráfico
ggplot(promedios_mensuales, aes(x = yearMonth, y = promedio_validados, color = estacion, group = estacion)) +
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
