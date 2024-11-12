# 
# # https://github.com/solrepresa/AQ-Valencia/blob/1732eab4542b64bb9429d7a89e9a4611b52acd12/src/AC_15.R#L7
rm(list=ls())

dir_raster = "D:/Josefina/Proyectos/ProyectoChile/SP/dataset/01_NDVI/"

setwd(dir_raster)

id <- dir(dir_raster, pattern = ".hdf")


data_estacciones <- read.csv("D:/Josefina/Proyectos/ProyectoChile/SP/dataset/sitios.csv")
data_estacciones <- data_estacciones[data_estacciones$Considerado == "SI",]
puntos <- data_estacciones
puntos$estacion <- puntos$Nombre.sitio 

crs_project <- "+proj=longlat +datum=WGS84"



raster_template <- raster("D:/Josefina/Proyectos/ProyectoChile/SP/dataset/rasterTemplate/raster_template.tif")
df_rbind<- data.frame()


i<-1
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
  

  rst_resampling <- raster::resample(nvdi_factor, raster_template)
  # Definir las coordenadas del punto
  # valor <- raster::extract(rst_resampling, punto)
  valores_raster <- extract(rst_resampling, puntos[, c("long", "lat")])
  # Unir los valores del raster al dataframe original
  puntos_con_valores <- puntos %>%
    mutate(valor_raster = valores_raster)
  date = substr(file.name,10,16)
  #df <- data.frame(date =date, ndvi=valor, estacion=estacion)
  df <- data.frame (date= date, ndvi=puntos_con_valores$valor_raster,estacion=puntos_con_valores$estacion, ID = puntos_con_valores$ID)
  df_rbind <- rbind(df_rbind,df)
  file.remove(dir('./', paste0('nvdi', basename(file.name), '*')))

}

View(df_rbind)
write.csv(df_rbind,"D:/Josefina/Proyectos/ProyectoChile/SP/proceed/01_NDVI/SP_NDVI.csv")
######################################################
#######################################################

