

library(raster)

# Definir el directorio donde están tus archivos raster

dir_salida <- "D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/Prediccion_01-2024/Salida/Salida_02-XGB_cv_M4-300924/"
setwd(dir_salida)

# Lista de archivos raster (ajusta la extensión según sea necesario)
lista_raster <- list.files(pattern = "*.tif") # Cambia la extensión si es necesario
lista_raster_recorte <- lista_raster[112:128]
lista_raster_recorte
length(lista_raster_recorte)
# Cargar los rasters en un RasterStack
raster_stack <- stack(lista_raster_recorte)

# Calcular el promedio mensual
promedio_mensual <- calc(raster_stack, fun = mean, na.rm = TRUE)
plot(promedio_mensual)
modelo <- substr(lista_raster[1],15,33)
# Guardar el resultado en un nuevo archivo raster
dir_salida <- "D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/Prediccion_01-2024/SalidasMensuales/"
writeRaster(promedio_mensual, filename = paste(dir_salida,"promedio_mensual_05-2024-",modelo,".tif",sep=""), format = "GTiff", overwrite = TRUE)

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

# Convertir la columna 'date' a formato POSIXct
data_estaciones_2024$date <- as.POSIXct(as.character(data_estaciones_2024$date), format = "%y%m%d")

# Agregar una columna de año y mes
data_estaciones_2024 <- data_estaciones_2024 %>%
  mutate(year = format(date, "%Y"),
         month = format(date, "%m"))

# Calcular promedios mensuales por estación
promedios_mensuales <- data_estaciones_2024 %>%
  group_by(estacion, year, month) %>%     # Agrupar por estación, año y mes
  summarise(promedio_validados = mean(validados, na.rm = TRUE)) %>% # Calcular promedio ignorando NA
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
promedios_mensuales$month2 <- factor(promedios_mensuales$month, levels = 1:12)#, 
                                    labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                                               "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))

# Crear el gráfico
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


