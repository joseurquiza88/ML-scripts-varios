library(terra)
library(raster)
library(dplyr)
library(gdalUtils)

# Configuración inicial
rm(list = ls())
estacion <- "CH"
dir_raster <- paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/dataset/01_NDVI/")
setwd(dir_raster)

# Lista de archivos HDF
id <- dir(dir_raster, pattern = ".hdf")

# Leer estaciones consideradas
data_estaciones <- read.csv(paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/dataset/estaciones/sitios_", estacion, ".csv"))
data_estaciones <- data_estaciones[data_estaciones$Considerado == "SI", ]
puntos <- data_estaciones
raster_template <- raster(paste0("D:/Josefina/Proyectos/ProyectoChile/", estacion, "/dataset/rasterTemplate/raster_template.tif"))
crs_project <- "+proj=longlat +datum=WGS84"

# Extraer valores NDVI
df_rbind <- data.frame()
i<-1

for (i in 1:length(id)) {
  print(paste("Procesando archivo:", i))
  file.name <- id[i]
  sds <- get_subdatasets(file.name)
  
  # Convertir SDS a TIFF
  gdal_translate(sds[1], dst_dataset = paste0('ndvi_', basename(file.name), '.tiff'))
  nvdi <- raster(paste0('ndvi_', basename(file.name), '.tiff'))
  proj4string(nvdi) <- CRS(as.character(nvdi@crs))
  
  # Reproyectar y aplicar factor de escala
  nvdi <- projectRaster(nvdi, crs = crs_project)
  nvdi_factor <- nvdi * 0.0001
  rst_resampling <- raster::resample(nvdi_factor, raster_template)
  
  # Extraer valores de puntos
  valores_raster <- extract(rst_resampling, puntos[, c("long", "lat")])
  date <- as.Date(substr(file.name, 10, 16), format = "%Y%j")
  
  # Dataframe temporal
  df <- data.frame(
    date = date,
    ndvi = valores_raster,
    estacion = puntos$estacion,
    ID = puntos$ID
  )
  
  # Unir resultados
  df_rbind <- rbind(df_rbind, df)
  
  # Limpiar archivos temporales
  file.remove(dir('./', paste0('ndvi_', basename(file.name), '*')))
}

# Filtrar por ID (Ejemplo: interpolar para un punto específico)
id_punto <- unique(df_rbind$ID)[1]  # Seleccionar un ID (ejemplo)
df_punto <- df_rbind %>% filter(ID == id_punto & !is.na(ndvi)) %>% arrange(date)

# Interpolación Spline
fechas_diarias <- seq(min(df_punto$date), max(df_punto$date), by = "day")
spline_ndvi <- spline(x = df_punto$date, y = df_punto$ndvi, xout = fechas_diarias)


# Crear un dataframe con fechas diarias y valores NDVI interpolados
df_interpolado <- data.frame(date = spline_ndvi$x, ndvi_interpolado = spline_ndvi$y)
df_interpolado$date <- as.Date(df_interpolado$date, origin = "1970-01-01")
# Mostrar los primeros resultados
View(df_interpolado)
