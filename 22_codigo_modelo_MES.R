##rm(list=ls())
#NDVI
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
# 08:51
for (i in 1:length(id)){
  print(i)
  data_maiac <- id[i]
  nombre_maiac <- substr(data_maiac,10,16)
  sds <- get_subdatasets(data_maiac)
  info <- GDALinfo(data_maiac,returnScaleOffset=FALSE)
  subdataset_metadata <- attr(info,"mdata")
  orbitas<-(subdataset_metadata)[59]#58 Y 59
  
  orbit <- gsub(pattern = 'Orbit_time_stamp=', replacement = '', x = orbitas) # Remove "Orbit_time_stamp="
  
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
    gdal_translate(sds[2], dst_dataset = paste0('tmp055', basename(data_maiac), '.tiff'), b = nband)
    # print(sds[2])
    r.055 <- raster(paste0('tmp055', basename(data_maiac), '.tiff'))
    
    # AOD_QA
    gdal_translate(sds[6], dst_dataset = paste0('tmpqa', basename(data_maiac), '.tiff'), b = nband)
    # print(sds[6])
    r.QA <- raster(paste0('tmpqa', basename(data_maiac), '.tiff'))
    SINU <- as.character(r.055@crs)
    proj4string(r.055) <- CRS(SINU)
    proj4string(r.QA) <- CRS(SINU)
    r.055 <- projectRaster(r.055,crs = crs_project)
    r.QA <- projectRaster(r.QA,crs = crs_project)
    r.055  <- r.055 * 0.001   #factor de escala
    cropped_r.055 <- crop(r.055, extent(ndvi_raster))
    cropped_QA <- crop(r.QA, extent(ndvi_raster))
    # 1) Aplicar mascara de calidad (QA= 0000) a imagenes MODIS
    cropped_QA[ cropped_QA] <- as.integer(substring(intToBin(cropped_QA[cropped_QA]), 4, 7)) #nos quedamos con los bits 8-11
    cropped_QA[ cropped_QA != 0] <- NA
    #Aplicar máscara
    r.055 <- mask(cropped_r.055, cropped_QA)
    # Agregar el raster procesado a la lista
    rasters_list[[nband]] <- r.055
    file.remove(dir('./', paste0('tmp055', basename(data_maiac), '*')))
    file.remove(dir('./', paste0('tmpqa', basename(data_maiac), '*')))
    
  }
  #Crear el mosaico
  if (length(rasters_list) > 1) {
    # Crear el mosaico a partir de los rasters
    mosaic_r.055 <- do.call(mosaic, c(rasters_list, fun = mean)) 
    # Guardar el mosaico resultante
    
  } else {
    # Si solo hay un raster, no se necesita mosaico
    mosaic_r.055 <- rasters_list[[1]]
  }
  
    ## Guardamos raster 
  writeRaster(mosaic_r.055, filename = paste(dir_maiac_guardado,"/tiff/00_MAIAC/",nombre_maiac,"-MAIAC_raster",sep = ""), format = "GTiff", overwrite = TRUE)
  
}
# cuantos valores NA hay
num_na <- sum(is.na(mosaic_r.055[]))

# Mostrar la cantidad de valores NA
print(num_na)

# Codigo para interpolacion  IDW
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

###########################################################################
# -----------------------   06 dayweek  ------------------------------
###########################################################################
#01-01-2016 dayweek = 2
dir_era <- "D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/Prediccion_01-2024/05_ERA5"
dir_weekDay_guardado <- "D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/Prediccion_01-2024/"
setwd(dir_era)
id <- list.files(path = dir_era,
                 pattern = "*.nc",
                 full.names = FALSE)
crs_project <- "+proj=longlat +datum=WGS84"
for (i in 1:length(id)){
  print(i)
  era5 <- id[i]
  nombre_weekDay <- substr(era5,0,10)
  # Crear un nuevo raster con la misma extensión, resolución y proyección
  dayWeek_raster <- raster(extent(ndvi_raster), 
                           nrows = nrow(ndvi_raster), 
                           ncols = ncol(ndvi_raster), 
                           crs = crs(ndvi_raster))
  
  # Raster con el numero de dia
  values(dayWeek_raster) <-  wday(strptime(nombre_weekDay, format = "%Y-%m-%d"))
  # print(values(dayWeek_raster)[1])
  writeRaster(dayWeek_raster, paste(dir_weekDay_guardado,"/tiff/06_weekDay/",nombre_weekDay,"-weekDay_raster",sep=""), format="GTiff", overwrite=TRUE)

}

###########################################################################
###########################################################################
#Usar version de r 2.3.4
###########################################################################
# -----------------------   Todas las variables ---------------------------
###########################################################################
#rm(list=ls())
#df_rbind <- data.frame()
# Suponiendo que mi_dataframe es el objeto que no quieres eliminar
rm(list = setdiff(ls(), "df_rbind"))
setwd("D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/Prediccion_01-2024/tiff/")
dir_salida <- "D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/Prediccion_01-2024/Salida/Salida_01-XGB_cv_M1-100924/"
fechaInteres <- as.Date("01-01-2024", format = "%d-%m-%Y")
#Modelos
dir_modelos <- "D:/Josefina/Proyectos/ProyectoChile/modelos/modelo/"
#load(paste(dir_modelos,"01-RF_cv_M1-050924.RData",sep=""))
#load(paste(dir_modelos,"01-RF_cv_M2-050924.RData",sep=""))
#load(paste(dir_modelos,"01-RF_cv_M3-050924.RData",sep=""))
# load(paste(dir_modelos,"01-RF_cv_M4-060924.RData",sep=""))
# load(paste(dir_modelos,"02-RF_cv_M1-090924.RData",sep=""))
load(paste(dir_modelos,"01-XGB_cv_M1-100924.RData",sep=""))
for (i in 1:1){

  ################# -----     00 MAIAC     -----
  # 2024001-MAIAC_raster.tif
  # Convertir a día juliano respecto al 1 de enero del mismo año
  dayJulian <- as.numeric(fechaInteres - as.Date(paste0(format(fechaInteres, "%Y"), "-01-01"))) + 1
  yearInteres <- year(fechaInteres)
  if(nchar(dayJulian)==1){
    sep = "00"
  }
  if (nchar(dayJulian)==2) {
    sep = "0"
  }
  
  if (nchar(dayJulian)==3) {
    sep = ""
  }
  maiacDate <- paste(yearInteres,sep,dayJulian,sep = "")
  MAIAC_raster <- raster(paste("00_MAIAC/00_MAIAC_IDW/IDW-",maiacDate,"-MAIAC_raster.tif",sep=""))
  plot(MAIAC_raster)
  
  ################### -----     NDVI     -----
  # Como es modis tambien la fecha es juliana pero ojo es un dato mensual
  fechaNDVI<- as.Date("01-01-2024", format = "%d-%m-%Y")
  yearNdvi <- year(fechaNDVI)
  dayJulianNDVI <- as.numeric(fechaNDVI - as.Date(paste0(format(fechaNDVI, "%Y"), "-01-01"))) + 1
  
  if(nchar(dayJulianNDVI)==1){
    sep = "00"
  }
  if (nchar(dayJulianNDVI)==2) {
    sep = "0"
  }
  
  if (nchar(dayJulianNDVI)==3) {
    sep = ""
  }
  NDVIDate <- paste(yearInteres,sep,dayJulianNDVI,sep = "")
  
  NDVI_raster <- raster(paste("01_NDVI/",NDVIDate,"-NDVI_raster.tif",sep=""))
  plot(NDVI_raster)
  
  ################# -----     LandCover     -----
  #Este dato es anual por lo que tambien tenemos que setear la fecha dierente
  # Pero es MODIS = Dia juliano
  # MCD12Q1.A2024001.h12v12.061.2022169161028.hdf
  
  
  yearLandCover <- year(fechaNDVI)
  LandCoverDate <- paste(yearLandCover,"001",sep = "")
  LandCover_raster <- raster(paste("02_LandCover/",LandCoverDate,"-LandCover_raster.tif",sep=""))
  
  plot(LandCover_raster)
  
  ################# -----     DEM     -----
  
  DEM_raster <- raster("03_DEM/DEM_raster.tif")
  plot(DEM_raster)
  ################# -----     MERRA-2     -----
  fechaInteres_MERRA <- gsub("-", "", fechaInteres)
  
  ## BCSMASS
  BCSMASS_raster <- raster(paste("04_MERRA-2/",fechaInteres_MERRA,"-BCSMASS_raster.tif",sep=""))
  #plot(BCSMASS_raster)
  
  ## DMSSMASS
  DMSSMASS_raster <- raster(paste("04_MERRA-2/",fechaInteres_MERRA,"-DMSSMASS_raster.tif",sep=""))
  #plot(DMSSMASS_raster)
  
  ## DUSMASS
  DUSMASS_raster <- raster(paste("04_MERRA-2/",fechaInteres_MERRA,"-DUSMASS_raster.tif",sep=""))
  #plot(DUSMASS_raster)
  
  ## DUSMASS25
  DUSMASS25_raster <- raster(paste("04_MERRA-2/",fechaInteres_MERRA,"-DUSMASS25_raster.tif",sep=""))
  #plot(DUSMASS25_raster)
  
  ## OCSMASS
  OCSMASS_raster <- raster(paste("04_MERRA-2/",fechaInteres_MERRA,"-OCSMASS_raster.tif",sep=""))
  #plot(OCSMASS_raster)
  
  ## SO2SMASS
  SO2SMASS_raster <- raster(paste("04_MERRA-2/",fechaInteres_MERRA,"-SO2SMASS_raster.tif",sep=""))
  #plot(SO2SMASS_raster)
  
  ## SO4SMASS
  SO4SMASS_raster <- raster(paste("04_MERRA-2/",fechaInteres_MERRA,"-SO4SMASS_raster.tif",sep=""))
  #plot(SO4SMASS_raster)
  
  ## SSSMASS
  SSSMASS_raster <- raster(paste("04_MERRA-2/",fechaInteres_MERRA,"-SSSMASS_raster.tif",sep=""))
  #plot(SSSMASS_raster)
  
  ## SSSMASS25
  SSSMASS25_raster <- raster(paste("04_MERRA-2/",fechaInteres_MERRA,"-SSSMASS25_raster.tif",sep=""))
  #plot(SSSMASS25_raster)
  
  ################# -----     ERA5     -----
  ## BLH     -----
  BLH_raster <- raster(paste("05_ERA5/",fechaInteres,"-BLH_raster.tif",sep=""))
  #plot(BLH_raster)
  
  ## D2M
  D2M_raster <- raster(paste("05_ERA5/",fechaInteres,"-D2M_raster.tif",sep=""))
  #plot(D2M_raster)
  
  ## T2M
  T2M_raster <- raster(paste("05_ERA5/",fechaInteres,"-T2M_raster.tif",sep=""))
  #plot(T2M_raster)
  
  ## TP
  TP_raster <- raster(paste("05_ERA5/",fechaInteres,"-TP_raster.tif",sep=""))
  #plot(TP_raster)
  
  ## SP
  SP_raster <- raster(paste("05_ERA5/",fechaInteres,"-SP_raster.tif",sep=""))
  #plot(SP_raster)
  
  ## V10
  V10_raster <- raster(paste("05_ERA5/",fechaInteres,"-V10_raster.tif",sep=""))
  #plot(V10_raster)
  
  ## U10
  U10_raster <- raster(paste("05_ERA5/",fechaInteres,"-U10_raster.tif",sep=""))
  #plot(U10_raster)
  
  ################# -----     DayWeek     -----
  ################# -----     ERA5     -----
  ## BLH     -----
  dayWeek_raster <- raster(paste("06_weekDay/",fechaInteres,"-weekDay_raster.tif",sep=""))
  #plot(dayWeek_raster)
  
  ##### STACK
  
  r_stack <- stack(MAIAC_raster,NDVI_raster,LandCover_raster,
                   BCSMASS_raster ,DUSMASS_raster,DUSMASS25_raster,
                   OCSMASS_raster,SO2SMASS_raster, SO4SMASS_raster,
                   SSSMASS_raster ,SSSMASS25_raster,BLH_raster,
                   SP_raster, D2M_raster, T2M_raster,V10_raster, 
                   U10_raster,TP_raster,DEM_raster,dayWeek_raster)
  
  #plot(r_stack)
  
  r_stack_df <- as.data.frame(r_stack, na.rm = TRUE)
  names(r_stack_df)
  
  names(r_stack_df) <- c( "AOD_055" ,"ndvi","LandCover","BCSMASS",
                          "DUSMASS","DUSMASS25",
                          "OCSMASS","SO2SMASS",
                          "SO4SMASS","SSSMASS",
                          "SSSMASS25","blh_mean" ,
                          "sp_mean","d2m_mean",
                          "t2m_mean","v10_mean",
                          "u10_mean" ,"tp_mean" , 
                          "DEM","dayWeek")
  

###############################################################
##################################################################
##############################################################
####

# Aplicar el modelo
predictions <- predict(rf_cv_model, newdata = r_stack_df)
# Crear un raster vac?o con la misma extensi?n y resoluci?n que el stack
pred_raster <- raster(r_stack)

# Asignar las predicciones al raster
pred_raster[] <- NA  # Inicia con valores NA

# Reinsertar las predicciones en las celdas correspondientes
pred_raster[!is.na(values(r_stack[[1]]))] <- predictions

#getwd()


name_salida <- paste(dir_salida,"PM-",fechaInteres,"-01-RF_cv_M1-050924.tif",sep="")
writeRaster(pred_raster, filename = name_salida, format = "GTiff", overwrite = TRUE)

##############################################################
# Definir coordenadas (por ejemplo, latitud y longitud)
#for (x in 1:1){
  # Dataframe con coordenadas y nombres de estaciones
  puntos <- data.frame(
    lon = c(-70.659566, -70.66517052, -70.73210014, -70.58813772, -70.52346222, -70.7503877, -70.59475058, -70.74822755),
    lat = c(-33.465694, -33.54606688, -33.43301075, -33.51610874, -33.37639222, -33.43798487, -33.59134682, -33.36576262),
    estacion = c("OHG", "BSQ", "CNA", "FLD", "CDE", "PDH", "PTA", "QUI")
  )
  
  # Extraer los valores del raster en las coordenadas especificadas
  valores_raster <- extract(pred_raster, puntos[, c("lon", "lat")])
  
  # Unir los valores del raster al dataframe original
  puntos_con_valores <- puntos %>%
    mutate(valor_raster = valores_raster)
  
  # Mostrar el dataframe resultante
  # print(puntos_con_valores)
  puntos_con_valores$date <- fechaInteres
  
  df_rbind <- rbind(df_rbind,puntos_con_valores)
}

write.csv(df_rbind, paste(dir_salida,"salida_02-RF_cv_M1-090924_01-2024.csv",sep=""))



#manualmente lo unimos con las estaciones sinca
dir_salida <- "D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/Prediccion_01-2024/Salida/"

data_pm <- read.csv(paste(dir_salida,"salida_01-RF.csv",sep=""))
data_pm <- data_pm[complete.cases(data_pm),]
# Crear scatter plot con línea de regresión
modelo <- lm(X02_RF.CV.M1.090924 ~ valor_sinca, data = data_pm)

# Calcular R²
r2 <- summary(modelo)$r.squared

# Calcular RMSE (Root Mean Squared Error)
rmse <- sqrt(mean((data_pm$X02_RF.CV.M1.090924 - predict(modelo))^2))

# Calcular Bias
bias <- mean(data_pm$X02_RF.CV.M1.090924 - data_pm$valor_sinca)

pearson <- cor(data_pm$valor_sinca, data_pm$X02_RF.CV.M1.090924, method = "pearson")
# Crear scatter plot con línea de regresión
ggplot(data_pm, aes(x = valor_sinca, y = X02_RF.CV.M1.090924)) +
  geom_point(color = "#99d8c9", ,alpha= 0.7,size = 2) +  # Puntos del scatter plot
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Línea de regresión lineal
  labs(x = "SINCA", y = "Prediccion", title = "Prediccion 01-2024 02-RF_cv_M1-090924") +
  theme_classic() +
  geom_text(aes(x = 8, y = 20,
                label = paste("r2 = ",round(r2,2))),
            stat = "unique",
            size = 2.5, color = "Black")+
  geom_text(aes(x = 7.8, y = 19.3,
                label = paste("r = ",round(pearson,2))),
            stat = "unique",
            size = 2.5, color = "Black")+

  geom_text(aes(x = 8.23, y = 18.7,
                label = paste("RMSE = ",round(rmse,2))),
            stat = "unique",
            size = 2.5, color = "Black") +

  
  geom_text(aes(x = 8.10, y = 18,
                label = paste("Bias = ",round(bias,2))),
            stat = "unique",
            size = 2.5, color = "Black")


data_pm$Error_RF.CV.M1 <- data_pm$valor_sinca - data_pm$RF.CV.M1

# Graficar
ggplot(data_pm, aes(x = RF.CV.M1, y = Error_RF.CV.M1)) +
  geom_point(color = "#99d8c9", alpha = 0.7, size = 2) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  labs(x = "Valor Predicho", y = "Error", title = "Error de Predicción") +
  theme_classic()
