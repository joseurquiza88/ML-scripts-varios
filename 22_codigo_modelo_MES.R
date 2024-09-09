rm(list=ls())
ndvi_raster <- raster("D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/Prediccion_01-2024/tiff/01_NDVI/2024001-NDVI_raster.tif")

###########################################################################
# -----------------------   01 MAIAC  ------------------------------
###########################################################################

#data_maiac <- "00_MAIAC/MCD19A2.A2019001.h12v12.061.2023123150048.hdf"

dir_maiac <- "D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/Prediccion_01-2024/00_MAIAC"
dir_maiac_guardado <- "D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/Prediccion_01-2024/"
setwd(dir_maiac)
id <- list.files(path = dir_maiac,
                 pattern = "*.hdf",
                 full.names = FALSE)
crs_project <- "+proj=longlat +datum=WGS84"
for (i in 1:length(id)){
  print(i)
  data_maiac <- id[i]
  nombre_maiac <- subtr(id,10,17)
  sds <- get_subdatasets(data_maiac)
  orbit <- unlist(strsplit(orbit, split = ' ')) # Seperate the string array by spaces
  sub.idx <- which(nchar(orbit) != 0) # Remove NA strings
  orbit <- orbit[sub.idx]
  
  # --- For each orbit --- #

  # Lista para guardar los rasters procesados
  rasters_list <- list()
  for (nband in 1 : length(orbit)) {
    
    #print(paste('Band:', orbit[nband]))
    
    # --- Convert the data to raster --- #
    
    # Optical_Depth_055
    gdal_translate(sds[2], dst_dataset = paste0('tmp055', basename(file.name), '.tiff'), b = nband)
    # print(sds[2])
    r.055 <- raster(paste0('tmp055', basename(file.name), '.tiff'))
    
    # AOD_QA
    gdal_translate(sds[6], dst_dataset = paste0('tmpqa', basename(file.name), '.tiff'), b = nband)
    # print(sds[6])
    r.QA <- raster(paste0('tmpqa', basename(file.name), '.tiff'))
    SINU <- as.character(r.055@crs)
    proj4string(r.055) <- CRS(SINU)
    proj4string(r.QA) <- CRS(SINU)
    r.055 <- projectRaster(r.055,crs = crs_project)
    r.QA <- projectRaster(r.QA,crs = crs_project)
    r.055  <- r.055 * 0.001   #factor de escala
    # 1) Aplicar mascara de calidad (QA= 0000) a imagenes MODIS
    r.QA[ r.QA] <- as.integer(substring(intToBin(r.QA[r.QA]), 4, 7)) #nos quedamos con los bits 8-11
    r.QA[ r.QA != 0] <- NA
    #Aplicar máscara
    r.055 <- mask(r.055, r.QA)
    # Agregar el raster procesado a la lista
    rasters_list[[nband]] <- r.055
  }
  #Crear el mosaico
  if (length(rasters_list) > 1) {
    # Crear el mosaico a partir de los rasters
    mosaic_r.055 <- do.call(mosaic, c(rasters_list, fun = mean)) # Puedes usar otra función si prefieres
    # Guardar el mosaico resultante
    
  } else {
    # Si solo hay un raster, no se necesita mosaico
    mosaic_r.055 <- rasters_list[[1]]
  }
  cropped_r.055 <- crop(mosaic_r.055, extent(ndvi_raster))
  # Imagen AOD MAIAC
  MAIAC_raster <- cropped_r.055
  # Eliminar tiff generados
  file.remove(dir('./', paste0('tmp055', basename(data_maiac), '*')))
  file.remove(dir('./', paste0('tmpQA', basename(data_maiac), '*')))
  ## Guardamos raster 
  writeRaster(MAIAC_raster, filename = paste(dir_maiac_guardado,"/tiff/",nombre_maiac,"-MAIAC_raster",sep = ""), format = "GTiff", overwrite = TRUE)
  
}


###########################################################################
# -----------------------   05 ERA  ------------------------------
###########################################################################

#05 ERA
#####01
# nameVar <- c("t2m", "d2m", "sp", "u10", "v10", "blh", "tp")

dir_era <- "D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/Prediccion_01-2024/05_ERA5"
dir_era_guardado <- "D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/Prediccion_01-2024/"
setwd(dir_era)
id <- list.files(path = dir_era,
                 pattern = "*.nc",
                 full.names = FALSE)
crs_project <- "+proj=longlat +datum=WGS84"
for (i in 1:length(id)){
  print(i)
  era5 <- id[i]
  nombre_era <- substr(era5,0,10)
  data_ERA_t2m <- brick(era5,varname="t2m")
  data_ERA_t2m_mean <- calc(data_ERA_t2m, mean, na.rm=TRUE)
  SINU <- as.character(data_ERA_t2m_mean@crs)
  data_ERA_t2m_mean <- projectRaster(data_ERA_t2m_mean,crs = crs_project)
  resampled_ERA_t2m <- raster::resample(data_ERA_t2m_mean, ndvi_raster,method = "bilinear")
  cropped_ERA_t2m  <- crop(resampled_ERA_t2m, extent(ndvi_raster))
  cropped_ERA_t2m <- cropped_ERA_t2m$layer -273.15
  #02
  data_ERA_d2m <- brick(era5,varname="d2m")
  data_ERA_d2m_mean <- calc(data_ERA_d2m, mean, na.rm=TRUE)
  SINU <- as.character(data_ERA_d2m_mean@crs)
  data_ERA_d2m_mean <- projectRaster(data_ERA_d2m_mean,crs = crs_project)
  resampled_ERA_d2m <- raster::resample(data_ERA_d2m_mean, ndvi_raster,method = "bilinear")
  cropped_ERA_d2m  <- crop(resampled_ERA_d2m, extent(ndvi_raster))
  cropped_ERA_d2m <- cropped_ERA_d2m$layer -273.15
  
  #03
  data_ERA_sp <- brick(era5,varname="sp")
  data_ERA_sp_mean <- calc(data_ERA_sp, mean, na.rm=TRUE)
  SINU <- as.character(data_ERA_sp_mean@crs)
  data_ERA_sp_mean <- projectRaster(data_ERA_sp_mean,crs = crs_project)
  resampled_ERA_sp <- raster::resample(data_ERA_sp_mean, ndvi_raster,method = "bilinear")
  cropped_ERA_sp  <- crop(resampled_ERA_sp, extent(ndvi_raster))
  
  #04
  data_ERA_u10 <- brick(era5,varname="u10")
  data_ERA_u10_mean <- calc(data_ERA_u10, mean, na.rm=TRUE)
  SINU <- as.character(data_ERA_u10_mean@crs)
  data_ERA_u10_mean <- projectRaster(data_ERA_u10_mean,crs = crs_project)
  resampled_ERA_u10 <- raster::resample(data_ERA_u10_mean, ndvi_raster,method = "bilinear")
  cropped_ERA_u10  <- crop(resampled_ERA_u10, extent(ndvi_raster))
  
  ### 05
  data_ERA_v10 <- brick(era5,varname="v10")
  data_ERA_v10_mean <- calc(data_ERA_v10, mean, na.rm=TRUE)
  SINU <- as.character(data_ERA_v10_mean@crs)
  data_ERA_v10_mean <- projectRaster(data_ERA_v10_mean,crs = crs_project)
  resampled_ERA_v10 <- raster::resample(data_ERA_v10_mean, ndvi_raster,method = "bilinear")
  cropped_ERA_v10  <- crop(resampled_ERA_v10, extent(ndvi_raster))
  
  ### 06
  data_ERA_blh <- brick(era5,varname="blh")
  data_ERA_blh_mean <- calc(data_ERA_blh, mean, na.rm=TRUE)
  SINU <- as.character(data_ERA_blh_mean@crs)
  data_ERA_blh_mean <- projectRaster(data_ERA_blh_mean,crs = crs_project)
  resampled_ERA_blh <- raster::resample(data_ERA_blh_mean, ndvi_raster,method = "bilinear")
  cropped_ERA_blh  <- crop(resampled_ERA_blh, extent(ndvi_raster))
  
  ### 07
  data_ERA_tp <- brick(era5,varname="tp")
  data_ERA_tp_mean <- calc(data_ERA_tp, mean, na.rm=TRUE)
  SINU <- as.character(data_ERA_tp_mean@crs)
  data_ERA_tp_mean <- projectRaster(data_ERA_tp_mean,crs = crs_project)
  resampled_ERA_tp <- raster::resample(data_ERA_tp_mean, ndvi_raster,method = "bilinear")
  cropped_ERA_tp  <- crop(resampled_ERA_tp, extent(ndvi_raster))
  
  ## guardamos
  
  writeRaster(cropped_ERA_blh, paste(dir_era_guardado,"/tiff/05_ERA5/",nombre_era,"-BLH_raster",sep=""), format="GTiff", overwrite=TRUE)
  writeRaster(cropped_ERA_t2m, paste(dir_era_guardado,"/tiff/05_ERA5/",nombre_era,"-T2M_raster",sep=""), format="GTiff", overwrite=TRUE)
  writeRaster(cropped_ERA_d2m, paste(dir_era_guardado,"/tiff/05_ERA5/",nombre_era,"-D2M_raster",sep=""), format="GTiff", overwrite=TRUE)
  writeRaster(cropped_ERA_sp, paste(dir_era_guardado,"/tiff/05_ERA5/",nombre_era,"-SP_raster",sep=""), format="GTiff", overwrite=TRUE)
  writeRaster(cropped_ERA_tp, paste(dir_era_guardado,"/tiff/05_ERA5/",nombre_era,"-TP_raster",sep=""), format="GTiff", overwrite=TRUE)
  writeRaster(cropped_ERA_v10, paste(dir_era_guardado,"/tiff/05_ERA5/",nombre_era,"-V10_raster",sep=""), format="GTiff", overwrite=TRUE)
  writeRaster(cropped_ERA_u10, paste(dir_era_guardado,"/tiff/05_ERA5/",nombre_era,"-U10_raster",sep=""), format="GTiff", overwrite=TRUE)

}
###########################################################################
# -----------------------   04 MERRA  ------------------------------
###########################################################################

#04 MERRA
#nameVar <- c("BCSMASS","DMSSMASS", "DUSMASS","DUSMASS25", "OCSMASS", "SO2SMASS", "SO4SMASS", "SSSMASS","SSSMASS25")
#####01
# data_merra_BCSMASS <- raster("04_MERRA-2/MERRA2_400.tavg1_2d_aer_Nx.20190101.SUB.nc",varname="BCSMASS")
#merra <- "04_MERRA-2/MERRA2_400.tavg1_2d_aer_Nx.20240101.SUB.nc"

dir_merra <- "D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/Prediccion_01-2024/04_MERRA-2"
dir_merra_guardado <- "D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/Prediccion_01-2024/"
setwd(dir_merra)
id <- list.files(path = dir_merra,
                 pattern = "*.nc",
                 full.names = FALSE)
crs_project <- "+proj=longlat +datum=WGS84"
for(i in 1:length(id)){
  print(i)
  merra <- id[i]
  
  # nombre_merra <- substr(merra,39,46)
  nombre_merra <- substr(merra,28,35)
  data_merra_BCSMASS <- raster(merra,varname="BCSMASS")
  
  SINU <- as.character(data_merra_BCSMASS@crs)
  data_merra_BCSMASS <- projectRaster(data_merra_BCSMASS,crs = crs_project)
  # Resamplear el raster original a la nueva resolución de 1km
  resampled_merra_BCSMASS <- raster::resample(data_merra_BCSMASS, ndvi_raster,method = "bilinear")
  cropped_merra_BCSMASS <- crop(resampled_merra_BCSMASS, extent(ndvi_raster))
  
  # 02
  data_merra_DMSSMASS <- raster(merra,varname="DMSSMASS")
  SINU <- as.character(data_merra_DMSSMASS@crs)
  data_merra_DMSSMASS <- projectRaster(data_merra_DMSSMASS,crs = crs_project)
  # Resamplear el raster original a la nueva resolución de 1km
  resampled_merra_DMSSMASS <- raster::resample(data_merra_DMSSMASS, ndvi_raster,method = "bilinear")
  cropped_merra_DMSSMASS <- crop(resampled_merra_DMSSMASS, extent(ndvi_raster))
  
  # 03
  data_merra_DUSMASS <- raster(merra,varname="DUSMASS")
  SINU <- as.character(data_merra_DUSMASS@crs)
  data_merra_DUSMASS <- projectRaster(data_merra_DUSMASS,crs = crs_project)
  # Resamplear el raster original a la nueva resolución de 1km
  resampled_merra_DUSMASS <- raster::resample(data_merra_DUSMASS, ndvi_raster,method = "bilinear")
  cropped_merra_DUSMASS <- crop(resampled_merra_DUSMASS, extent(ndvi_raster))
  
  # 04
  data_merra_DUSMASS25 <- raster(merra,varname="DUSMASS25")
  SINU <- as.character(data_merra_DUSMASS25@crs)
  data_merra_DUSMASS25 <- projectRaster(data_merra_DUSMASS25,crs = crs_project)
  # Resamplear el raster original a la nueva resolución de 1km
  resampled_merra_DUSMASS25 <- raster::resample(data_merra_DUSMASS25, ndvi_raster,method = "bilinear")
  cropped_merra_DUSMASS25 <- crop(resampled_merra_DUSMASS25, extent(ndvi_raster))
  
  # 05
  data_merra_OCSMASS <- raster(merra,varname="OCSMASS")
  SINU <- as.character(data_merra_OCSMASS@crs)
  data_merra_OCSMASS <- projectRaster(data_merra_OCSMASS,crs = crs_project)
  # Resamplear el raster original a la nueva resolución de 1km
  resampled_merra_OCSMASS <- raster::resample(data_merra_OCSMASS, ndvi_raster,method = "bilinear")
  cropped_merra_OCSMASS <- crop(resampled_merra_OCSMASS, extent(ndvi_raster))
  
  
  # 06
  #nameVar <- c("BCSMASS","DMSSMASS", "DUSMASS","DUSMASS25", "OCSMASS", "SO2SMASS", "SO4SMASS", "SSSMASS","SSSMASS25")
  data_merra_SO2SMASS <- raster(merra,varname="SO2SMASS")
  SINU <- as.character(data_merra_SO2SMASS@crs)
  data_merra_SO2SMASS <- projectRaster(data_merra_SO2SMASS,crs = crs_project)
  # Resamplear el raster original a la nueva resolución de 1km
  resampled_merra_SO2SMASS <- raster::resample(data_merra_SO2SMASS, ndvi_raster,method = "bilinear")
  cropped_merra_SO2SMASS <- crop(resampled_merra_SO2SMASS, extent(ndvi_raster))
  
  # 07
  #nameVar <- c(", "OCSMASS", "SO2SMASS", "SO4SMASS", "SSSMASS","SSSMASS25")
  
  data_merra_SO4SMASS <- raster(merra,varname="SO4SMASS")
  SINU <- as.character(data_merra_SO4SMASS@crs)
  data_merra_SO4SMASS <- projectRaster(data_merra_SO4SMASS,crs = crs_project)
  # Resamplear el raster original a la nueva resolución de 1km
  resampled_merra_SO4SMASS <- raster::resample(data_merra_SO4SMASS, ndvi_raster,method = "bilinear")
  cropped_merra_SO4SMASS <- crop(resampled_merra_SO4SMASS, extent(ndvi_raster))
  
  # 08
  #nameVar <- c(", "OCSMASS", "SO2SMASS", "SO4SMASS", "SSSMASS","SSSMASS25")
  data_merra_SSSMASS <- raster(merra,varname="SSSMASS")
  SINU <- as.character(data_merra_SSSMASS@crs)
  data_merra_SSSMASS <- projectRaster(data_merra_SSSMASS,crs = crs_project)
  # Resamplear el raster original a la nueva resolución de 1km
  resampled_merra_SSSMASS <- raster::resample(data_merra_SSSMASS, ndvi_raster,method = "bilinear")
  cropped_merra_SSSMASS <- crop(resampled_merra_SSSMASS, extent(ndvi_raster))
  
  # 09
  #nameVar <- c(", "OCSMASS", "SO2SMASS", "SO4SMASS", "SSSMASS25","SSSMASS2525")
  data_merra_SSSMASS25 <- raster(merra,varname="SSSMASS25")
  SINU <- as.character(data_merra_SSSMASS25@crs)
  data_merra_SSSMASS25 <- projectRaster(data_merra_SSSMASS25,crs = crs_project)
  # Resamplear el raster original a la nueva resolución de 1km
  resampled_merra_SSSMASS25 <- raster::resample(data_merra_SSSMASS25, ndvi_raster,method = "bilinear")
  cropped_merra_SSSMASS25 <- crop(resampled_merra_SSSMASS25, extent(ndvi_raster))
  
  # Guardar el raster procesado
  writeRaster(cropped_merra_BCSMASS, paste(dir_merra_guardado,"/tiff/04_MERRA-2/",nombre_merra,"-BCSMASS_raster",sep=""), format="GTiff", overwrite=TRUE)
  writeRaster(cropped_merra_DMSSMASS,paste(dir_merra_guardado,"/tiff/04_MERRA-2/",nombre_merra,"-DMSSMASS_raster",sep=""), format="GTiff", overwrite=TRUE)
  writeRaster(cropped_merra_DUSMASS, paste(dir_merra_guardado,"/tiff/04_MERRA-2/",nombre_merra,"-DUSMASS_raster",sep=""), format="GTiff", overwrite=TRUE)
  writeRaster(cropped_merra_DUSMASS25, paste(dir_merra_guardado,"/tiff/04_MERRA-2/",nombre_merra,"-DUSMASS25_raster",sep=""), format="GTiff", overwrite=TRUE)
  writeRaster(cropped_merra_OCSMASS, paste(dir_merra_guardado,"/tiff/04_MERRA-2/",nombre_merra,"-OCSMASS_raster",sep=""), format="GTiff", overwrite=TRUE)
  writeRaster(cropped_merra_SO2SMASS, paste(dir_merra_guardado,"/tiff/04_MERRA-2/",nombre_merra,"-SO2SMASS_raster",sep=""), format="GTiff", overwrite=TRUE)
  writeRaster(cropped_merra_SO4SMASS, paste(dir_merra_guardado,"/tiff/04_MERRA-2/",nombre_merra,"-SO4SMASS_raster",sep=""), format="GTiff", overwrite=TRUE)
  writeRaster(cropped_merra_SSSMASS, paste(dir_merra_guardado,"/tiff/04_MERRA-2/",nombre_merra,"-SSSMASS_raster",sep=""), format="GTiff", overwrite=TRUE)
  writeRaster(cropped_merra_SSSMASS25, paste(dir_merra_guardado,"/tiff/04_MERRA-2/",nombre_merra,"-SSSMASS25_raster",sep=""), format="GTiff", overwrite=TRUE)
}
