# ## Land Cover
# # Producto satelital MCD12Q1
# # Resolucion 500 m- anual
# 
# # Ejemplo con una imagen
rm(list=ls())
# 


data_estacciones <- read.csv("D:/Josefina/Proyectos/ProyectoChile/SP/dataset/sitios.csv")
data_estacciones <- data_estacciones[data_estacciones$Considerado == "SI",]
puntos <- data_estacciones
puntos$estacion <- puntos$Nombre.sitio 

crs_project <- "+proj=longlat +datum=WGS84"

dire <- "D:/Josefina/Proyectos/ProyectoChile/SP/dataset/02_LandCover"

setwd(dire)

raster_template <- raster("D:/Josefina/Proyectos/ProyectoChile/SP/dataset/rasterTemplate/raster_template.tif")
df_rbind<- data.frame()



id <- list.files(path = getwd(),
                 pattern = "*.hdf",
                 full.names = FALSE)

i<-1
#LandCover_function <- function(file, latlong.range = NULL, border.shp = NULL) {
for (i in 1:length(id)){
  print(i)
  file <- id[i]
  landCover_tot <- data.frame()
  f_LC_Type1_tot <- data.frame()
  sds <- get_subdatasets(file)
  
  year <- substr(file,10,13)
  tile <- substr(file,18,23)
  
  #Image_Optical_Depth_Land_And_Ocean"
  gdal_translate(sds[1], dst_dataset = paste0('LC_Type1', basename(file), '.tiff'))#, b = nUHnd) # mask is UHnd number
  LC_Type1<- raster(paste0('LC_Type1', basename(file), '.tiff'))
  f_LC_Type1_wgs84 <- projectRaster(LC_Type1,
                                    crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

  
  gdal_translate(sds[2], dst_dataset = paste0('LC_Type2', basename(file), '.tiff'))#, b = nUHnd) # mask is UHnd number
  LC_Type2<- raster(paste0('LC_Type2', basename(file), '.tiff'))
  f_LC_Type2_wgs84 <- projectRaster(LC_Type2,crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
 
  f_LC_Type1_wgs84_resampling <- raster::resample(f_LC_Type1_wgs84 , raster_template)
  f_LC_Type2_wgs84_resampling <- raster::resample(f_LC_Type2_wgs84 , raster_template)
  # 

  
  valores_raster_Type1 <- extract(f_LC_Type1_wgs84_resampling, puntos[, c("long", "lat")])
  valores_raster_Type2 <- extract(f_LC_Type2_wgs84_resampling, puntos[, c("long", "lat")])
  puntos_con_valores <- puntos %>%
    mutate(valores_raster_Type1 = valores_raster_Type1) %>%
    mutate(valores_raster_Type2 = valores_raster_Type2)
  
  file.remove(dir('./', paste0('LC_Type1', basename(file), '*')))
  file.remove(dir('./', paste0('LC_Type2', basename(file), '*')))
  # file.remove(dir('./', paste0('LC_Type3', basename(file), '*')))
  # file.remove(dir('./', paste0('LC_Type4', basename(file), '*')))
  # file.remove(dir('./', paste0('LC_Type5', basename(file), '*')))
  df <- data.frame (year= year,tile = tile, Type1=puntos_con_valores$valores_raster_Type1,
                    Type2=puntos_con_valores$valores_raster_Type2,
                    estacion=puntos_con_valores$estacion, 
                    ID = puntos_con_valores$ID)
  
  df_rbind <- rbind(df_rbind,df)
  
}


dire <- "D:/Josefina/Proyectos/ProyectoChile/SP/dataset/02_LandCover"

write.csv(df_rbind,"D:/Josefina/Proyectos/ProyectoChile/SP/proceed/02_LandCover/SP_LandCover.csv")
