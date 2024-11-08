library(gdata)
library(maptools)
library(dismo) #kfold
library(caret)
library(parallel)
# SANTAIAGO
# dir_salida <- "D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/Prediccion_2022/tiff/00_MAIAC/00_MAIAC_IDW/"
# dir_maiac <- "D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/Prediccion_2022/tiff/00_MAIAC/"

#TALCA
dir_salida <- "D:/Josefina/Proyectos/ProyectoChile/talca/modelos/dataset_ejemplo/Prediccion_2015/tiff/00_MAIAC/00_MAIAC_IDW/"
dir_maiac <- "D:/Josefina/Proyectos/ProyectoChile/talca/modelos/dataset_ejemplo/Prediccion_2015/tiff/00_MAIAC/"


setwd(dir_maiac)
crs_project = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

fs <- list.files(path = dir_maiac, 
                 pattern = "tif",
                 full.names = FALSE)

# Crear raster template >>> spatial Pixels DF
# ndvi_raster <- raster("D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/Prediccion_2024/tiff/01_NDVI/2024001-NDVI_raster.tif")
ndvi_raster <- raster("D:/Josefina/Proyectos/ProyectoChile/talca/dataset/01_NDVI/NDVI_raster/NDVI_raster.tif")


raster_template <- ndvi_raster
projection(raster_template) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
idw.grid <- rasterToPoints(raster_template, spatial = TRUE)
gridded(idw.grid) <- TRUE   #SpatialPixelsDataFrame

RMSE_IDW <- data.frame()
kfold = 5 # numero de k-fold cross validation

i<-144

for( i in 1:length(fs)){
  print(i)
  raster_fs <- raster(fs[i], sep="")
  filename <- paste("IDW-", fs[i], sep="")
  # cuantos valores NA hay
  num_na <- sum(is.na(raster_fs[]))
  if (num_na !=0){
    raster_points <- as.data.frame(rasterToPoints(raster_fs))
    ###### ---- santiago
    # length(mosaic_r.055)= 1056, el 10% 105
    ###### ---- santiago
    #as.data.frame(rasterToPoints(raster_template)) ## 891 pixeles, el 10% ==> 90 pixeles
    # if(nrow(raster_points) > 3776){  #### control: raster con un 10% de datos
    # if(nrow(raster_points) > 105){ ## SANTIAGO
    if(nrow(raster_points) > 5){
      coordinates(raster_points) <- ~x+y
      #raster_points@proj4string
      projection(raster_points) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      names(raster_points)[1] <- "AOD"
      
      set.seed(513)  # 5-fold cross-validation
      
      kf <- kfold(nrow(raster_points), k = kfold)
      
      rmse <- rep(NA, kfold)
      for (k in 1:kfold) {   
        test <- raster_points[kf == k, ]
        train <- raster_points[kf != k, ]
        kat.idw <- gstat::idw(AOD ~ 1, train, idw.grid, 
                              debug.level = -1,   #para ver grado de progreso
                              idp = 5) # IDW power
        
        final.idw <- raster(kat.idw)
        t <- SpatialPoints(test, proj4string = CRS(crs_project)) #convierto formato para aplicar extract
        #plot(final.idw)
        model <- extract(final.idw, t)
        # rmse[k] <- caret::RMSE(test$AOD, model)
        rmse[k] <- sqrt(mean((model - test$AOD)^2))
        
        }
      
      tabla <- data.frame(archivo = filename, RMSE = mean(rmse, na.rm = TRUE))
      RMSE_IDW <- rbind(RMSE_IDW, tabla)
      
      
      # Guardar
      writeRaster(final.idw, paste(dir_salida, filename, sep = ""), 
                  format = "GTiff",
                  overwrite = TRUE)
      
      rm(raster_template, train, test, kat.idw,raster_fs)
      
    }
  }
  else{
    writeRaster(raster_fs, paste(dir_salida, filename, sep = ""), 
                format = "GTiff",
                overwrite = TRUE)
    rm(raster_fs)
  }
  
}
  #########################
#Guardamos csv del RMSE Medio de la interpolacion
getwd()
write.csv(RMSE_IDW,"MAIAC_RMSE_IDW.csv")
