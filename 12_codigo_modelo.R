### --------------------------------------------------------------- ########

## Objetivo: leer todos los archivos de las variables y aplicarle el modelo
# para generar un mapa de predeccion de PM2.5
###############################################################################
# Leemos shapefile de refocorte
crs_project <- "+proj=longlat +datum=WGS84"
# Reproyectar el polígono al CRS del raster si es necesario
polygon_shapefile <-  readOGR("D:/Josefina/Proyectos/ProyectoChile/shape/data_referencia/santiago_4326.shp")
#01 Generamos un Raster template
#ext = extent(-70.89626564382297,-70.54544288411844,-33.61652852593954,-33.33591657301113)#
ext = extent(polygon_shapefile)
km_per_degree <- 111  # Aproximadamente 111 km por grado
pixel_size_km <- 1  # Tama?o de p?xel deseado en km
# Calcular la resoluci?n en grados
resolution_degrees <- pixel_size_km / km_per_degree
# Calcular el n?mero de filas y columnas basados en la resoluci?n
nrows <- (ext@ymax - ext@ymin) / resolution_degrees
ncols <- (ext@xmax - ext@xmin) / resolution_degrees
# Redondear a n?meros enteros para filas y columnas
nrows <- ceiling(nrows)
ncols <- ceiling(ncols)
# Crear el raster con la extensi?n y resoluci?n adecuada
raster_template <- raster(nrows = nrows, ncols = ncols, crs = crs_project, ext = extent(polygon_shapefile))

#seteamos directorio
setwd("D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/")
## 02 Abrimos datos de cada una de las variables
# -----------------------   00 MAIAC  ------------------------------
# Esto es con la version 4.1.1 de r sino no funciona
#MAIAC
# https://lpdaac.usgs.gov/products/mcd19a2v061/
data_maiac <- "00_MAIAC/MCD19A2.A2019001.h12v12.061.2023123150048.hdf"
# data_maiac <- raster(data_maiac)
sds <- get_subdatasets(data_maiac)

# Optical_Depth_055
gdal_translate(sds[2], dst_dataset = paste0('tmp055', basename(data_maiac), '1.tiff'), b = 1)
r.055_1 <- raster(paste0('tmp055', basename(data_maiac), '1.tiff'))

gdal_translate(sds[2], dst_dataset = paste0('tmp055', basename(data_maiac), '2.tiff'), b = 2)
r.055_2 <- raster(paste0('tmp055', basename(data_maiac), '2.tiff'))

gdal_translate(sds[2], dst_dataset = paste0('tmp055', basename(data_maiac), '3.tiff'), b = 3)
r.055_3 <- raster(paste0('tmp055', basename(data_maiac), '3.tiff'))

# gdal_translate(sds[2], dst_dataset = paste0('tmp055', basename(data_maiac), '4.tiff'), b = 4)
# r.055_4 <- raster(paste0('tmp055', basename(data_maiac), '4.tiff'))

# AQ
# Optical_Depth_055
gdal_translate(sds[6], dst_dataset = paste0('tmpQA', basename(data_maiac), '1.tiff'), b = 1)
r.QA_1 <- raster(paste0('tmpQA', basename(data_maiac), '1.tiff'))

gdal_translate(sds[6], dst_dataset = paste0('tmpQA', basename(data_maiac), '2.tiff'), b = 2)
r.QA_2 <- raster(paste0('tmpQA', basename(data_maiac), '2.tiff'))

gdal_translate(sds[6], dst_dataset = paste0('tmpQA', basename(data_maiac), '3.tiff'), b = 3)
r.QA_3 <- raster(paste0('tmpQA', basename(data_maiac), '3.tiff'))

# Reproyectar de sinu a 4324
SINU <- as.character(r.055_1@crs)
proj4string(r.055_1) <- CRS(SINU)
proj4string(r.055_2) <- CRS(SINU)
proj4string(r.055_3) <- CRS(SINU)
# proj4string(r.055_4) <- CRS(SINU)
proj4string(r.QA_1) <- CRS(SINU)
proj4string(r.QA_2) <- CRS(SINU)
proj4string(r.QA_3) <- CRS(SINU)

#AOD
r.055_1 <- projectRaster(r.055_1,crs = crs_project)
r.055_2 <- projectRaster(r.055_2,crs = crs_project)
r.055_3 <- projectRaster(r.055_3,crs = crs_project)
#QA
r.QA_1 <- projectRaster(r.QA_1,crs = crs_project)
r.QA_2 <- projectRaster(r.QA_2,crs = crs_project)
r.QA_3 <- projectRaster(r.QA_3,crs = crs_project)
# r.055_4 <- projectRaster(r.055_4,crs = crs_project)
#Factor de escala
r.055_1  <- r.055_1 * 0.001   #factor de escala
r.055_2  <- r.055_2 * 0.001   #factor de escala
r.055_3  <- r.055_3 * 0.001   #factor de escala

# 1) Aplicar mascara de calidad (QA= 0000) a imagenes MODIS
# intToBien convierte el numero a binario
# substring se queda con los characteres de interes
# integer transforma character a numero
r.QA_1[ r.QA_1] <- as.integer(substring(intToBin(r.QA_1[r.QA_1]), 4, 7)) #nos quedamos con los bits 8-11
r.QA_1[ r.QA_1 != 0] <- NA
r.QA_2[ r.QA_2] <- as.integer(substring(intToBin(r.QA_2[r.QA_2]), 4, 7)) #nos quedamos con los bits 8-11
r.QA_2[ r.QA_2 != 0] <- NA
r.QA_3[ r.QA_3] <- as.integer(substring(intToBin(r.QA_3[r.QA_3]), 4, 7)) #nos quedamos con los bits 8-11
r.QA_3[ r.QA_3 != 0] <- NA

#Aplicar máscara
r.055_1 <- mask(r.055_1, r.QA_1)
r.055_2 <- mask(r.055_2, r.QA_2)
r.055_3 <- mask(r.055_3, r.QA_3)
# Unimos todas las imagenes
mosaic_r.055 <- mosaic(r.055_1, r.055_2, r.055_3, fun = mean)


# Opcional: Puedes también ajustar el tamaño de los píxeles al área del polígono
# raster_clipped <- crop(mosaic_r.055, extent(polygon_shapefile))
# Recortamos al area de interes
cropped_r.055 <- crop(mosaic_r.055, extent(raster_template))

# Imagen AOD MAIAC
MAIAC_raster <- cropped_r.055
# Eliminar tiff generados
file.remove(dir('./', paste0('tmp055', basename(data_maiac), '*')))
file.remove(dir('./', paste0('tmpQA', basename(data_maiac), '*')))
## Guardamos raster 
writeRaster(MAIAC_raster, filename = "tiff/MAIAC_raster", format = "GTiff", overwrite = TRUE)

#################################################################################
#################################################################################

# -----------------------   01 NDVI  ------------------------------
###########################################################################
rm(list=ls())
#01 NDVI
#https://lpdaac.usgs.gov/products/mod13a3v061/
data_ndvi <- "01_NDVI/MOD13A3.A2019001.h12v12.061.2020286171223.hdf"
crs_project <- "+proj=longlat +datum=WGS84"
sds <- get_subdatasets(data_ndvi)
# Optical_Depth_055
gdal_translate(sds[1], dst_dataset = paste0('ndvi', basename(data_ndvi), '.tiff'))
ndvi <- raster(paste0('ndvi', basename(data_ndvi), '.tiff'))
SINU <- as.character(ndvi@crs)
proj4string(ndvi) <- CRS(SINU)
ndvi <- projectRaster(ndvi,crs = crs_project)
ndvi <- ndvi* 	0.0001   #factor de escala 

# Recortamos al area de interes
cropped_ndvi <- crop(ndvi, extent(raster_template))

# Imagen ndvi
ndvi_raster <- cropped_ndvi
# Eliminar tiff generados
file.remove(dir('./', paste0('ndvi', basename(data_ndvi), '*')))

## Guardamos raster 
writeRaster(ndvi_raster, filename = "tiff/NDVI_raster", format = "GTiff", overwrite = TRUE)

###########################################################################
# -----------------------   02 Land cover  ------------------------------
###########################################################################
rm(list=ls())
#02 Land cover MCD12Q1 500m
# https://lpdaac.usgs.gov/products/mcd12q1v061/
data_LandCover <- "02_LandCover/MCD12Q1.A2019001.h12v12.061.2022169161028.hdf"
crs_project <- "+proj=longlat +datum=WGS84"
sds <- get_subdatasets(data_LandCover)

#Image_Optical_Depth_Land_And_Ocean"
gdal_translate(sds[1], dst_dataset = paste0('LC_Type1', basename(data_LandCover), '.tiff'))
LC_Type1<- raster(paste0('LC_Type1', basename(data_LandCover), '.tiff'))
SINU <- as.character(LC_Type1@crs)
proj4string(LC_Type1) <- CRS(SINU)
LC_Type1 <- projectRaster(LC_Type1,crs = crs_project)
plot(cropped_LC_Type1)
# Cora
# Recortamos al area de interes

cropped_LC_Type1 <- crop(LC_Type1, extent(ndvi_raster))
# Resamplear el raster original a la nueva resolución de 1km
resampled_LC_Type1 <- raster::resample(cropped_LC_Type1, ndvi_raster, method = "ngb")

# Eliminar tiff generados
file.remove(dir('./', paste0('LC_Type1', basename(data_ndvi), '*')))
# Guardar el raster procesado
writeRaster(resampled_LC_Type1, "tiff/LandCover_raster", format="GTiff", overwrite=TRUE)

###########################################################################
# -----------------------   03 DEM  ------------------------------
###########################################################################
rm(list=ls())
#03 DEM 30m
data_DEM <- "03_DEM/S34W071.hgt"
crs_project <- "+proj=longlat +datum=WGS84"
DEM <- rast(data_DEM)
writeRaster(DEM, "dem.tif",  overwrite = TRUE)

dem_raster<- raster('dem.tif')
plot(dem_raster)
SINU <- as.character(dem_raster@crs)
proj4string(dem_raster) <- CRS(SINU)
dem_raster <- projectRaster(dem_raster,crs = crs_project)
# Recortamos al area de interes
cropped_DEM <- crop(dem_raster, extent(ndvi_raster))

# Resamplear el raster original a la nueva resolución de 1km
resampled_dem <- raster::resample(cropped_DEM, ndvi_raster, method = "ngb")

# Eliminar tiff generados
file.remove(dir('./', paste0('dem', '*')))
# Guardar el raster procesado
writeRaster(resampled_dem, "tiff/DEM_raster", format="GTiff", overwrite=TRUE)


###########################################################################
# -----------------------   04 MERRA  ------------------------------
###########################################################################
rm(list=ls())
#04 MERRA
#nameVar <- c("BCSMASS","DMSSMASS", "DUSMASS","DUSMASS25", "OCSMASS", "SO2SMASS", "SO4SMASS", "SSSMASS","SSSMASS25")
#####01
data_merra_BCSMASS <- raster("04_MERRA-2/MERRA2_400.tavg1_2d_aer_Nx.20190101.SUB.nc",varname="BCSMASS")
SINU <- as.character(data_merra_BCSMASS@crs)
data_merra_BCSMASS <- projectRaster(data_merra_BCSMASS,crs = crs_project)
# Resamplear el raster original a la nueva resolución de 1km
resampled_merra_BCSMASS <- raster::resample(data_merra_BCSMASS, ndvi_raster,method = "bilinear")
cropped_merra_BCSMASS <- crop(resampled_merra_BCSMASS, extent(ndvi_raster))

# 02
data_merra_DMSSMASS <- raster("04_MERRA-2/MERRA2_400.tavg1_2d_aer_Nx.20190101.SUB.nc",varname="DMSSMASS")
SINU <- as.character(data_merra_DMSSMASS@crs)
data_merra_DMSSMASS <- projectRaster(data_merra_DMSSMASS,crs = crs_project)
# Resamplear el raster original a la nueva resolución de 1km
resampled_merra_DMSSMASS <- raster::resample(data_merra_DMSSMASS, ndvi_raster,method = "bilinear")
cropped_merra_DMSSMASS <- crop(resampled_merra_DMSSMASS, extent(ndvi_raster))

# 03
data_merra_DUSMASS <- raster("04_MERRA-2/MERRA2_400.tavg1_2d_aer_Nx.20190101.SUB.nc",varname="DUSMASS")
SINU <- as.character(data_merra_DUSMASS@crs)
data_merra_DUSMASS <- projectRaster(data_merra_DUSMASS,crs = crs_project)
# Resamplear el raster original a la nueva resolución de 1km
resampled_merra_DUSMASS <- raster::resample(data_merra_DUSMASS, ndvi_raster,method = "bilinear")
cropped_merra_DUSMASS <- crop(resampled_merra_DUSMASS, extent(ndvi_raster))

# 04
data_merra_DUSMASS25 <- raster("04_MERRA-2/MERRA2_400.tavg1_2d_aer_Nx.20190101.SUB.nc",varname="DUSMASS25")
SINU <- as.character(data_merra_DUSMASS25@crs)
data_merra_DUSMASS25 <- projectRaster(data_merra_DUSMASS25,crs = crs_project)
# Resamplear el raster original a la nueva resolución de 1km
resampled_merra_DUSMASS25 <- raster::resample(data_merra_DUSMASS25, ndvi_raster,method = "bilinear")
cropped_merra_DUSMASS25 <- crop(resampled_merra_DUSMASS25, extent(ndvi_raster))

# 05
data_merra_OCSMASS <- raster("04_MERRA-2/MERRA2_400.tavg1_2d_aer_Nx.20190101.SUB.nc",varname="OCSMASS")
SINU <- as.character(data_merra_OCSMASS@crs)
data_merra_OCSMASS <- projectRaster(data_merra_OCSMASS,crs = crs_project)
# Resamplear el raster original a la nueva resolución de 1km
resampled_merra_OCSMASS <- raster::resample(data_merra_OCSMASS, ndvi_raster,method = "bilinear")
cropped_merra_OCSMASS <- crop(resampled_merra_OCSMASS, extent(ndvi_raster))


# 06
#nameVar <- c("BCSMASS","DMSSMASS", "DUSMASS","DUSMASS25", "OCSMASS", "SO2SMASS", "SO4SMASS", "SSSMASS","SSSMASS25")
data_merra_SO2SMASS <- raster("04_MERRA-2/MERRA2_400.tavg1_2d_aer_Nx.20190101.SUB.nc",varname="SO2SMASS")
SINU <- as.character(data_merra_SO2SMASS@crs)
data_merra_SO2SMASS <- projectRaster(data_merra_SO2SMASS,crs = crs_project)
# Resamplear el raster original a la nueva resolución de 1km
resampled_merra_SO2SMASS <- raster::resample(data_merra_SO2SMASS, ndvi_raster,method = "bilinear")
cropped_merra_SO2SMASS <- crop(resampled_merra_SO2SMASS, extent(ndvi_raster))

# 07
#nameVar <- c(", "OCSMASS", "SO2SMASS", "SO4SMASS", "SSSMASS","SSSMASS25")

data_merra_SO4SMASS <- raster("04_MERRA-2/MERRA2_400.tavg1_2d_aer_Nx.20190101.SUB.nc",varname="SO4SMASS")
SINU <- as.character(data_merra_SO4SMASS@crs)
data_merra_SO4SMASS <- projectRaster(data_merra_SO4SMASS,crs = crs_project)
# Resamplear el raster original a la nueva resolución de 1km
resampled_merra_SO4SMASS <- raster::resample(data_merra_SO4SMASS, ndvi_raster,method = "bilinear")
cropped_merra_SO4SMASS <- crop(resampled_merra_SO4SMASS, extent(ndvi_raster))

# 08
#nameVar <- c(", "OCSMASS", "SO2SMASS", "SO4SMASS", "SSSMASS","SSSMASS25")
data_merra_SSSMASS <- raster("04_MERRA-2/MERRA2_400.tavg1_2d_aer_Nx.20190101.SUB.nc",varname="SSSMASS")
SINU <- as.character(data_merra_SSSMASS@crs)
data_merra_SSSMASS <- projectRaster(data_merra_SSSMASS,crs = crs_project)
# Resamplear el raster original a la nueva resolución de 1km
resampled_merra_SSSMASS <- raster::resample(data_merra_SSSMASS, ndvi_raster,method = "bilinear")
cropped_merra_SSSMASS <- crop(resampled_merra_SSSMASS, extent(ndvi_raster))

# 09
#nameVar <- c(", "OCSMASS", "SO2SMASS", "SO4SMASS", "SSSMASS25","SSSMASS2525")
data_merra_SSSMASS25 <- raster("04_MERRA-2/MERRA2_400.tavg1_2d_aer_Nx.20190101.SUB.nc",varname="SSSMASS25")
SINU <- as.character(data_merra_SSSMASS25@crs)
data_merra_SSSMASS25 <- projectRaster(data_merra_SSSMASS25,crs = crs_project)
# Resamplear el raster original a la nueva resolución de 1km
resampled_merra_SSSMASS25 <- raster::resample(data_merra_SSSMASS25, ndvi_raster,method = "bilinear")
cropped_merra_SSSMASS25 <- crop(resampled_merra_SSSMASS25, extent(ndvi_raster))

# Guardar el raster procesado
writeRaster(cropped_merra_BCSMASS, "tiff/BCSMASS_raster", format="GTiff", overwrite=TRUE)
writeRaster(cropped_merra_DMSSMASS, "tiff/DMSSMASS_raster", format="GTiff", overwrite=TRUE)
writeRaster(cropped_merra_DUSMASS, "tiff/DUSMASS_raster", format="GTiff", overwrite=TRUE)
writeRaster(cropped_merra_DUSMASS25, "tiff/DUSMASS25_raster", format="GTiff", overwrite=TRUE)
writeRaster(cropped_merra_OCSMASS, "tiff/OCSMASS_raster", format="GTiff", overwrite=TRUE)
writeRaster(cropped_merra_SO2SMASS, "tiff/SO2SMASS_raster", format="GTiff", overwrite=TRUE)
writeRaster(cropped_merra_SO4SMASS, "tiff/SO4SMASS_raster", format="GTiff", overwrite=TRUE)
writeRaster(cropped_merra_SSSMASS, "tiff/SSSMASS_raster", format="GTiff", overwrite=TRUE)
writeRaster(cropped_merra_SSSMASS25, "tiff/SSSMASS25_raster", format="GTiff", overwrite=TRUE)


###########################################################################
# -----------------------   05 ERA  ------------------------------
###########################################################################
rm(list=ls())
#05 ERA
#####01
# nameVar <- c("t2m", "d2m", "sp", "u10", "v10", "blh", "tp")
data_ERA_t2m <- brick("05_ERA5/2019-01-01_download.nc",varname="t2m")
data_ERA_t2m_mean <- calc(data_ERA_t2m_2, mean, na.rm=TRUE)
SINU <- as.character(data_ERA_t2m_mean@crs)
data_ERA_t2m_mean <- projectRaster(data_ERA_t2m_mean,crs = crs_project)
resampled_ERA_t2m <- raster::resample(data_ERA_t2m_mean, ndvi_raster,method = "bilinear")
cropped_ERA_t2m  <- crop(resampled_ERA_t2m, extent(ndvi_raster))
cropped_ERA_t2m <- cropped_ERA_t2m$layer -273.15
#02
data_ERA_d2m <- brick("05_ERA5/2019-01-01_download.nc",varname="d2m")
data_ERA_d2m_mean <- calc(data_ERA_d2m, mean, na.rm=TRUE)
SINU <- as.character(data_ERA_d2m_mean@crs)
data_ERA_d2m_mean <- projectRaster(data_ERA_d2m_mean,crs = crs_project)
resampled_ERA_d2m <- raster::resample(data_ERA_d2m_mean, ndvi_raster,method = "bilinear")
cropped_ERA_d2m  <- crop(resampled_ERA_d2m, extent(ndvi_raster))
cropped_ERA_d2m <- cropped_ERA_d2m$layer -273.15

#03
data_ERA_sp <- brick("05_ERA5/2019-01-01_download.nc",varname="sp")
data_ERA_sp_mean <- calc(data_ERA_sp, mean, na.rm=TRUE)
SINU <- as.character(data_ERA_sp_mean@crs)
data_ERA_sp_mean <- projectRaster(data_ERA_sp_mean,crs = crs_project)
resampled_ERA_sp <- raster::resample(data_ERA_sp_mean, ndvi_raster,method = "bilinear")
cropped_ERA_sp  <- crop(resampled_ERA_sp, extent(ndvi_raster))

#04
data_ERA_u10 <- brick("05_ERA5/2019-01-01_download.nc",varname="u10")
data_ERA_u10_mean <- calc(data_ERA_u10, mean, na.rm=TRUE)
SINU <- as.character(data_ERA_u10_mean@crs)
data_ERA_u10_mean <- projectRaster(data_ERA_u10_mean,crs = crs_project)
resampled_ERA_u10 <- raster::resample(data_ERA_u10_mean, ndvi_raster,method = "bilinear")
cropped_ERA_u10  <- crop(resampled_ERA_u10, extent(ndvi_raster))

### 05
data_ERA_v10 <- brick("05_ERA5/2019-01-01_download.nc",varname="v10")
data_ERA_v10_mean <- calc(data_ERA_v10, mean, na.rm=TRUE)
SINU <- as.character(data_ERA_v10_mean@crs)
data_ERA_v10_mean <- projectRaster(data_ERA_v10_mean,crs = crs_project)
resampled_ERA_v10 <- raster::resample(data_ERA_v10_mean, ndvi_raster,method = "bilinear")
cropped_ERA_v10  <- crop(resampled_ERA_v10, extent(ndvi_raster))

### 06
data_ERA_blh <- brick("05_ERA5/2019-01-01_download.nc",varname="blh")
data_ERA_blh_mean <- calc(data_ERA_blh, mean, na.rm=TRUE)
SINU <- as.character(data_ERA_blh_mean@crs)
data_ERA_blh_mean <- projectRaster(data_ERA_blh_mean,crs = crs_project)
resampled_ERA_blh <- raster::resample(data_ERA_blh_mean, ndvi_raster,method = "bilinear")
cropped_ERA_blh  <- crop(resampled_ERA_blh, extent(ndvi_raster))

### 07
data_ERA_tp <- brick("05_ERA5/2019-01-01_download.nc",varname="tp")
data_ERA_tp_mean <- calc(data_ERA_tp, mean, na.rm=TRUE)
SINU <- as.character(data_ERA_tp_mean@crs)
data_ERA_tp_mean <- projectRaster(data_ERA_tp_mean,crs = crs_project)
resampled_ERA_tp <- raster::resample(data_ERA_tp_mean, ndvi_raster,method = "bilinear")
cropped_ERA_tp  <- crop(resampled_ERA_tp, extent(ndvi_raster))
## guardamos
writeRaster(cropped_ERA_blh, "tiff/BLH_raster", format="GTiff", overwrite=TRUE)
writeRaster(cropped_ERA_t2m, "tiff/T2M_raster", format="GTiff", overwrite=TRUE)
writeRaster(cropped_ERA_d2m, "tiff/D2M_raster", format="GTiff", overwrite=TRUE)
writeRaster(cropped_ERA_sp, "tiff/SP_raster", format="GTiff", overwrite=TRUE)
writeRaster(cropped_ERA_tp, "tiff/TP_raster", format="GTiff", overwrite=TRUE)
writeRaster(cropped_ERA_v10, "tiff/V10_raster", format="GTiff", overwrite=TRUE)
writeRaster(cropped_ERA_u10, "tiff/U10_raster", format="GTiff", overwrite=TRUE)




###########################################################################
# -----------------------   04 MERRA DIA  ------------------------------
###########################################################################
rm(list=ls())
#04 MERRA
#nameVar <- c("BCSMASS","DMSSMASS", "DUSMASS","DUSMASS25", "OCSMASS", "SO2SMASS", "SO4SMASS", "SSSMASS","SSSMASS25")
#####01
data_merra_BCSMASS_Dia <- raster("04_MERRA-2_Dia/MERRA2_400.tavg1_2d_aer_Nx.20190101.SUB.nc",varname="BCSMASS")
SINU <- as.character(data_merra_BCSMASS_Dia@crs)
data_merra_BCSMASS_Dia <- projectRaster(data_merra_BCSMASS_Dia,crs = crs_project)
# Resamplear el raster original a la nueva resolución de 1km
resampled_merra_BCSMASS_Dia <- raster::resample(data_merra_BCSMASS_Dia, ndvi_raster,method = "bilinear")
cropped_merra_BCSMASS_Dia <- crop(resampled_merra_BCSMASS_Dia, extent(ndvi_raster))

# 02
data_merra_DMSSMASS_Dia <- raster("04_MERRA-2_Dia/MERRA2_400.tavg1_2d_aer_Nx.20190101.SUB.nc",varname="DMSSMASS")
SINU <- as.character(data_merra_DMSSMASS_Dia@crs)
data_merra_DMSSMASS_Dia <- projectRaster(data_merra_DMSSMASS_Dia,crs = crs_project)
# Resamplear el raster original a la nueva resolución de 1km
resampled_merra_DMSSMASS_Dia <- raster::resample(data_merra_DMSSMASS_Dia, ndvi_raster,method = "bilinear")
cropped_merra_DMSSMASS_Dia <- crop(resampled_merra_DMSSMASS_Dia, extent(ndvi_raster))

# 03
data_merra_DUSMASS_Dia <- raster("04_MERRA-2_Dia/MERRA2_400.tavg1_2d_aer_Nx.20190101.SUB.nc",varname="DUSMASS")
SINU <- as.character(data_merra_DUSMASS_Dia@crs)
data_merra_DUSMASS_Dia <- projectRaster(data_merra_DUSMASS_Dia,crs = crs_project)
# Resamplear el raster original a la nueva resolución de 1km
resampled_merra_DUSMASS_Dia <- raster::resample(data_merra_DUSMASS_Dia, ndvi_raster,method = "bilinear")
cropped_merra_DUSMASS_Dia <- crop(resampled_merra_DUSMASS_Dia, extent(ndvi_raster))

# 04
data_merra_DUSMASS25_Dia <- raster("04_MERRA-2_Dia/MERRA2_400.tavg1_2d_aer_Nx.20190101.SUB.nc",varname="DUSMASS25")
SINU <- as.character(data_merra_DUSMASS25_Dia@crs)
data_merra_DUSMASS25_Dia <- projectRaster(data_merra_DUSMASS25_Dia,crs = crs_project)
# Resamplear el raster original a la nueva resolución de 1km
resampled_merra_DUSMASS25_Dia <- raster::resample(data_merra_DUSMASS25_Dia, ndvi_raster,method = "bilinear")
cropped_merra_DUSMASS25_Dia <- crop(resampled_merra_DUSMASS25_Dia, extent(ndvi_raster))

# 05
data_merra_OCSMASS_Dia <- raster("04_MERRA-2_Dia/MERRA2_400.tavg1_2d_aer_Nx.20190101.SUB.nc",varname="OCSMASS")
SINU <- as.character(data_merra_OCSMASS_Dia@crs)
data_merra_OCSMASS_Dia <- projectRaster(data_merra_OCSMASS_Dia,crs = crs_project)
# Resamplear el raster original a la nueva resolución de 1km
resampled_merra_OCSMASS_Dia <- raster::resample(data_merra_OCSMASS_Dia, ndvi_raster,method = "bilinear")
cropped_merra_OCSMASS_Dia <- crop(resampled_merra_OCSMASS_Dia, extent(ndvi_raster))


# 06
#nameVar <- c("BCSMASS","DMSSMASS", "DUSMASS","DUSMASS25", "OCSMASS", "SO2SMASS", "SO4SMASS", "SSSMASS","SSSMASS25")
data_merra_SO2SMASS_Dia <- raster("04_MERRA-2_Dia/MERRA2_400.tavg1_2d_aer_Nx.20190101.SUB.nc",varname="SO2SMASS")
SINU <- as.character(data_merra_SO2SMASS_Dia@crs)
data_merra_SO2SMASS_Dia <- projectRaster(data_merra_SO2SMASS_Dia,crs = crs_project)
# Resamplear el raster original a la nueva resolución de 1km
resampled_merra_SO2SMASS_Dia <- raster::resample(data_merra_SO2SMASS_Dia, ndvi_raster,method = "bilinear")
cropped_merra_SO2SMASS_Dia <- crop(resampled_merra_SO2SMASS_Dia, extent(ndvi_raster))

# 07
#nameVar <- c(", "OCSMASS", "SO2SMASS", "SO4SMASS", "SSSMASS","SSSMASS25")

data_merra_SO4SMASS_Dia <- raster("04_MERRA-2_Dia/MERRA2_400.tavg1_2d_aer_Nx.20190101.SUB.nc",varname="SO4SMASS")
SINU <- as.character(data_merra_SO4SMASS_Dia@crs)
data_merra_SO4SMASS_Dia <- projectRaster(data_merra_SO4SMASS_Dia,crs = crs_project)
# Resamplear el raster original a la nueva resolución de 1km
resampled_merra_SO4SMASS_Dia <- raster::resample(data_merra_SO4SMASS_Dia, ndvi_raster,method = "bilinear")
cropped_merra_SO4SMASS_Dia <- crop(resampled_merra_SO4SMASS_Dia, extent(ndvi_raster))

# 08
#nameVar <- c(", "OCSMASS", "SO2SMASS", "SO4SMASS", "SSSMASS","SSSMASS25")
data_merra_SSSMASS_Dia <- raster("04_MERRA-2_Dia/MERRA2_400.tavg1_2d_aer_Nx.20190101.SUB.nc",varname="SSSMASS")
SINU <- as.character(data_merra_SSSMASS_Dia@crs)
data_merra_SSSMASS_Dia <- projectRaster(data_merra_SSSMASS_Dia,crs = crs_project)
# Resamplear el raster original a la nueva resolución de 1km
resampled_merra_SSSMASS_Dia <- raster::resample(data_merra_SSSMASS_Dia, ndvi_raster,method = "bilinear")
cropped_merra_SSSMASS_Dia <- crop(resampled_merra_SSSMASS_Dia, extent(ndvi_raster))

# 09
#nameVar <- c(", "OCSMASS", "SO2SMASS", "SO4SMASS", "SSSMASS25","SSSMASS2525")
data_merra_SSSMASS25_Dia <- raster("04_MERRA-2_Dia/MERRA2_400.tavg1_2d_aer_Nx.20190101.SUB.nc",varname="SSSMASS25")
SINU <- as.character(data_merra_SSSMASS25_Dia@crs)
data_merra_SSSMASS25_Dia <- projectRaster(data_merra_SSSMASS25_Dia,crs = crs_project)
# Resamplear el raster original a la nueva resolución de 1km
resampled_merra_SSSMASS25_Dia <- raster::resample(data_merra_SSSMASS25_Dia, ndvi_raster,method = "bilinear")
cropped_merra_SSSMASS25_Dia <- crop(resampled_merra_SSSMASS25_Dia, extent(ndvi_raster))

# Guardar el raster procesado
writeRaster(cropped_merra_BCSMASS_Dia, "tiff/BCSMASS_raster_Dia", format="GTiff", overwrite=TRUE)
writeRaster(cropped_merra_DMSSMASS_Dia, "tiff/DMSSMASS_raster_Dia", format="GTiff", overwrite=TRUE)
writeRaster(cropped_merra_DUSMASS_Dia, "tiff/DUSMASS_raster_Dia", format="GTiff", overwrite=TRUE)
writeRaster(cropped_merra_DUSMASS25_Dia, "tiff/DUSMASS25_raster", format="GTiff", overwrite=TRUE)
writeRaster(cropped_merra_OCSMASS_Dia, "tiff/OCSMASS_raster_Dia", format="GTiff", overwrite=TRUE)
writeRaster(cropped_merra_SO2SMASS_Dia, "tiff/SO2SMASS_raster_Dia", format="GTiff", overwrite=TRUE)
writeRaster(cropped_merra_SO4SMASS_Dia, "tiff/SO4SMASS_raster_Dia", format="GTiff", overwrite=TRUE)
writeRaster(cropped_merra_SSSMASS_Dia, "tiff/SSSMASS_raster_Dia", format="GTiff", overwrite=TRUE)
writeRaster(cropped_merra_SSSMASS25_Dia, "tiff/SSSMASS25_raster_Dia", format="GTiff", overwrite=TRUE)










###########################################################################
# -----------------------   Todas las variables ---------------------------
###########################################################################
rm(list=ls())
# Leemos todas las variables generadas
# Seteamos diretorio
setwd("./tiff")
## 00 MAIAC
MAIAC_raster <- raster("MAIAC_raster.tif")
plot(MAIAC_raster)

## NDVI
NDVI_raster <- raster("NDVI_raster.tif")
plot(NDVI_raster)

## LandCover
LandCover_raster <- raster("LandCover_raster.tif")
plot(LandCover_raster)

## DEM
DEM_raster <- raster("DEM_raster.tif")
plot(DEM_raster)

## BCSMASS
BCSMASS_raster <- raster("BCSMASS_raster.tif")
plot(BCSMASS_raster)

## DMSSMASS
DMSSMASS_raster <- raster("DMSSMASS_raster.tif")
plot(DMSSMASS_raster)

## DUSMASS
DUSMASS_raster <- raster("DUSMASS_raster.tif")
plot(DUSMASS_raster)

## DUSMASS25
DUSMASS25_raster <- raster("DUSMASS25_raster.tif")
plot(DUSMASS25_raster)

## OCSMASS
OCSMASS_raster <- raster("OCSMASS_raster.tif")
plot(OCSMASS_raster)

## SO2SMASS
SO2SMASS_raster <- raster("SO2SMASS_raster.tif")
plot(SO2SMASS_raster)

## SO4SMASS
SO4SMASS_raster <- raster("SO4SMASS_raster.tif")
plot(SO4SMASS_raster)

## SSSMASS
SSSMASS_raster <- raster("SSSMASS_raster.tif")
plot(SSSMASS_raster)

## SSSMASS25
SSSMASS25_raster <- raster("SSSMASS25_raster.tif")
plot(SSSMASS25_raster)

## BLH
BLH_raster <- raster("BLH_raster.tif")
plot(BLH_raster)

## D2M
D2M_raster <- raster("D2M_raster.tif")
plot(D2M_raster)

## T2M
T2M_raster <- raster("T2M_raster.tif")
plot(T2M_raster)

## TP
TP_raster <- raster("TP_raster.tif")
plot(TP_raster)

## SP
SP_raster <- raster("SP_raster.tif")
plot(SP_raster)

## V10
V10_raster <- raster("V10_raster.tif")
plot(V10_raster)

## U10
U10_raster <- raster("U10_raster.tif")
plot(U10_raster)

##### STACK

r_stack <- stack(MAIAC_raster, NDVI_raster, LandCover_raster, DEM_raster,
                 BCSMASS_raster, DMSSMASS_raster, DUSMASS_raster, DUSMASS25_raster,
                 OCSMASS_raster,SO2SMASS_raster,SO4SMASS_raster, SSSMASS_raster,
                 SSSMASS25_raster, BLH_raster, D2M_raster,T2M_raster,TP_raster, 
                 SP_raster, V10_raster, U10_raster)

plot(r_stack)


######
###########################################################################
# -----------------------   Aplicar el modelo  ---------------------------
###########################################################################


# Paso 1: Cargar el modelo
load("D:/Josefina/Proyectos/ProyectoChile/modelos/random_forest_model.RData")

# Convertir el RasterStack en un data frame para la predicción
r_stack_df <- as.data.frame(r_stack, na.rm = TRUE)

# Aplicar el modelo de Random Forest al data frame
predictions <- predict(rf_model, newdata = r_stack_df)

# Crear un raster vacío con la misma extensión y resolución que el stack
pred_raster <- raster(r_stack)

# Asignar las predicciones al raster
pred_raster[] <- NA  # Inicia con valores NA

# Reinsertar las predicciones en las celdas correspondientes
pred_raster[!is.na(values(r_stack[[1]]))] <- predictions

getwd()
# Guardar el raster de predicciones
writeRaster(pred_raster, filename = "D:/Josefina/Proyectos/ProyectoChile/modelos/predicted_raster2.tif", format = "GTiff", overwrite = TRUE)

print("Predicciones aplicadas y guardadas en 'predicted_raster.tif'.")
