  # Cargar la librería necesaria

library(raster)
variable <- c("blh","d2m","sp","v10","u10","t2m")
i<- 12 #mes
n_dias <- 31  # Número de días del mes

31*6
for (p in 1:length(variable)){

  # _raster <- raster("D:/Josefina/Proyectos/ProyectoChile/talca/dataset/01_NDVI/NDVI_raster/NDVI_raster.tif")
  estacion <- "BA"
  ndvi_raster <- raster(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/dataset/rasterTemplate/raster_template.tif",sep=""))
  
  dir_era <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/dataset/meteoSatelital/2022/",sep="")
  dir_era_guardado <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/dataset_ejemplo/Prediccion_2022",sep="")
  
  
  
  setwd(dir_era)
  id <- list.files(path = dir_era,
                   pattern = "*.nc",
                   full.names = FALSE)
  crs_project <- "+proj=longlat +datum=WGS84"
  
  #for (j in 1:length(id)){
    #print(i)
    era5 <- id[i]
  
    nombre_era <- substr(era5, 0, 10)
  
  # Parámetros del archivo
  
  n_horas <- 24 # Número de horas por día
  
  # Lista vacía para almacenar las imágenes diarias
  data_ERA_t2m_daily <- list()
 
  # Bucle para calcular la media diaria
  for (dia in 1:n_dias) {
    print(c("--------",dia,"--------"))
    print(c("***", variable[p],"**"))
   
    # Inicializar una lista para almacenar las 24 bandas del día
    daily_bands <- list()
   
    for (hora in 1:n_horas) {
      #print(hora)
      # Calcular el índice de la banda correspondiente (1 a 744)
      banda <- (dia - 1) * n_horas + hora
      
      # Cargar la banda específica usando raster()
      MIRRAraster <- raster(era5, varname=variable[p], b=banda)
      fecha <- as.Date(as.POSIXct(MIRRAraster@z[[1]], origin = "1970-01-01", tz = "UTC"))
      SINU <- as.character(MIRRAraster@crs)
      # Agregar la banda a la lista
      daily_bands[[hora]] <- MIRRAraster
    }
    
    # Crear un brick con las 24 bandas del día
    daily_brick <- brick(daily_bands)
    
    # Calcular la media diaria
    daily_mean <- calc(daily_brick, mean, na.rm=TRUE)
    
    
    
    daily_mean_proj <- projectRaster(daily_mean,crs = crs_project)
    resampled_daily_mean <- raster::resample(daily_mean_proj, ndvi_raster,method = "bilinear")
    cropped_daily_mean  <- crop(resampled_daily_mean, extent(ndvi_raster))
    
    
    
    # Almacenar el raster promedio diario en la lista principal
    #data_ERA_t2m_daily[[dia]] <- daily_mean
    writeRaster(cropped_daily_mean, paste(dir_era_guardado,"/tiff/05_ERA5/",fecha,"-",variable[p],"_raster",sep=""), format="GTiff", overwrite=TRUE)
    
  }
}
# Combinar todas las imágenes diarias en un solo objeto brick
#daily_brick_final <- brick(data_ERA_t2m_daily)

# Ahora `daily_brick_final` contiene una imagen por cada día del mes con la media diaria
