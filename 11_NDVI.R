
# https://github.com/solrepresa/AQ-Valencia/blob/1732eab4542b64bb9429d7a89e9a4611b52acd12/src/AC_15.R#L7
rm(list=ls())

### Objetivo: Trabajar con NDVI de MOD13A3

crs_project = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Uso imagen del STACK de MODIS MCD19A2 como modelo para crear raster
# tomo el stack porq tiene el tamaño que me interesa conservar

# MODIS <- raster("/home/usuario/Sol/aire_comunitat/stack/month/AOD_mes_01_max.tif")
# MODIS <- st_read("D:/Josefina/Proyectos/ProyectoChile/shape/data_referencia/santiago_4326.shp")
# 
# raster_template <- raster(nrows = 239, ncols = 158, #100m de resolucion aprox
#                           crs = crs_project, 
#                           ext = extent(MODIS))  # toma las extensiones
#coords <- data.frame(x = -70.659566, y = -33.465694) #01 ohg  # Ejemplo con coordenadas de longitud y latitud
#coords <- data.frame(x = -70.66517052, y = -33.54606688) #02 BSQ
#coords <- data.frame(x = -70.7503877, y = -33.43798487) #03 PDH
#coords <- data.frame(x = -70.732100142, y = -33.43301075) #04 CNA
#coords <- data.frame(x = -70.59475058, y = -33.59134682	) #05 PTA
#coords <- data.frame(x = -70.732100142, y = -33.43301075) #06 FLD
#coords <- data.frame(x = -70.52346222, y = -33.37639222) #07 CDE
coords <- data.frame(x = -70.74822755, y = -33.36576262) #08 QUI
estacion <- "QUI"
num_estacion <- "08"


punto <- SpatialPoints(coords, proj4string = CRS(crs_project))



dir_raster = "D:/Josefina/Proyectos/ProyectoChile/dataset/NDVI/"
setwd(dir_raster)

id <- dir(dir_raster, pattern = ".hdf")
df_rbind <- data.frame()
for (i in 1:length(id)){
  print(i)
  file.name <- id[i]
  sds <- get_subdatasets(file.name)
  # Optical_Depth_055
  gdal_translate(sds[1], dst_dataset = paste0('nvdi', basename(file.name), '.tiff'))
  nvdi <- raster(paste0('nvdi', basename(file.name), '.tiff'))
  crs_project <- "+proj=longlat +datum=WGS84"
  # gdal_translate(sds[6], dst_dataset = paste0('qa', basename(file.name), '.tiff'))
  # qa <- raster(paste0('qa', basename(file.name), '.tiff'))
  SINU <- as.character(nvdi@crs)
  proj4string(nvdi) <- CRS(SINU)
  nvdi <- projectRaster(nvdi,crs = crs_project)
  nvdi_factor <- nvdi*0.0001   #factor de escala 
  
  # Template
  ext = extent(-70.89626564382297,-70.54544288411844,-33.61652852593954,-33.33591657301113)#
  km_per_degree <- 111  # Aproximadamente 111 km por grado
  pixel_size_km <- 1  # Tamaño de píxel deseado en km
  resolution_degrees <- pixel_size_km / km_per_degree
  nrows <- (ext@ymax - ext@ymin) / resolution_degrees
  ncols <- (ext@xmax - ext@xmin) / resolution_degrees
  nrows <- ceiling(nrows)
  ncols <- ceiling(ncols)
  raster_template <- raster(nrows = nrows, ncols = ncols, crs = crs_project, ext = ext)
  
  rst_resampling <- raster::resample(nvdi_factor, raster_template)
  # Definir las coordenadas del punto
  valor <- raster::extract(rst_resampling, punto)
  date = substr(file.name,10,16)
  df <- data.frame(date =date, ndvi=valor, estacion=estacion)
  df_rbind <- rbind(df_rbind,df)
  file.remove(dir('./', paste0('nvdi', basename(file.name), '*')))

}

View(df_rbind)
write.csv(df_rbind,paste("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/NDVI/",num_estacion,"_",estacion,"_NDVI.csv",sep=""))
######################################################
#######################################################

