
#Raster template

ext = extent(-70.89626564382297,-70.54544288411844,-33.61652852593954,-33.33591657301113)#
#raster_template <- raster(nrows = 86, ncols = 87,   crs = crs_project, ext =ext)
crs_project <- "+proj=longlat +datum=WGS84"  # Ejemplo de CRS, ajusta seg?n sea necesario

# Calcular la resoluci?n del p?xel en grados
# Asumiendo que las coordenadas est?n en grados, por ejemplo, si 1 grado = 111 km
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
raster_template <- raster(nrows = nrows, ncols = ncols, crs = crs_project, ext = ext)

################################################################################
##############################################################################
#data ERA
file.name <- "D:/Josefina/Proyectos/ProyectoChile/dataset/meteoSatelital/2015/01-2015/2015-01-05_download.nc"
nameVar <- c("t2m", "d2m", "sp", "u10", "v10", "blh", "tp")
crs_project <- "+proj=longlat +datum=WGS84"  # Ejemplo de CRS, ajusta seg?n sea necesario

###### ------       T2M       ----------  ####3
MIRRAraster_t2m <- raster(file.name,varname="t2m")
# Calcular el promedio de las 24 bandas
average_raster_t2m <- calc(MIRRAraster_t2m, fun = mean)
MIRRAraster2_t2m <- projectRaster(average_raster_t2m,
                                crs = crs_project,
                                method = "bilinear")
rst_resampling_t2m <- raster::resample(MIRRAraster2_t2m, raster_template)

###### ------       blh       ----------  ####3
MIRRAraster_blh <- raster(file.name,varname="blh")
# Calcular el promedio de las 24 bandas
average_raster_blh <- calc(MIRRAraster_blh, fun = mean)
MIRRAraster2_blh <- projectRaster(average_raster_blh,
                                  crs = crs_project,
                                  method = "bilinear")
rst_resampling_blh<- raster::resample(MIRRAraster2_blh, raster_template)
###### ------       d2m       ----------  ####3
MIRRAraster_d2m <- raster(file.name,varname="d2m")
# Calcular el promedio de las 24 bandas
average_raster_d2m <- calc(MIRRAraster_d2m, fun = mean)
MIRRAraster2_d2m <- projectRaster(average_raster_d2m,
                                  crs = crs_project,
                                  method = "bilinear")
rst_resampling_d2m<- raster::resample(MIRRAraster2_d2m, raster_template)


###### ------       sp       ----------  ####3
MIRRAraster_sp <- raster(file.name,varname="sp")
# Calcular el promedio de las 24 bandas
average_raster_sp <- calc(MIRRAraster_sp, fun = mean)
MIRRAraster2_sp <- projectRaster(average_raster_sp,
                                  crs = crs_project,
                                  method = "bilinear")
rst_resampling_sp<- raster::resample(MIRRAraster2_sp, raster_template)


###### ------       v10       ----------  ####3
MIRRAraster_v10 <- raster(file.name,varname="v10")
# Calcular el promedio de las 24 bandas
average_raster_v10 <- calc(MIRRAraster_v10, fun = mean)
MIRRAraster2_v10 <- projectRaster(average_raster_v10,
                                 crs = crs_project,
                                 method = "bilinear")
rst_resampling_v10<- raster::resample(MIRRAraster2_v10, raster_template)

###### ------       u10       ----------  ####3
MIRRAraster_u10 <- raster(file.name,varname="u10")
# Calcular el promedio de las 24 bandas
average_raster_u10 <- calc(MIRRAraster_u10, fun = mean)
MIRRAraster2_u10 <- projectRaster(average_raster_u10,
                                  crs = crs_project,
                                  method = "bilinear")
rst_resampling_u10<- raster::resample(MIRRAraster2_u10, raster_template)


###### ------       tp       ----------  ####3
MIRRAraster_tp <- raster(file.name,varname="tp")
# Calcular el promedio de las 24 bandas
average_raster_tp <- calc(MIRRAraster_tp, fun = mean)
MIRRAraster2_tp <- projectRaster(average_raster_tp,
                                  crs = crs_project,
                                  method = "bilinear")
rst_resampling_tp <- raster::resample(MIRRAraster2_tp, raster_template)

r_stack <- stack(rst_resampling_t2m,rst_resampling_blh,rst_resampling_d2m,
                 rst_resampling_sp,rst_resampling_v10,rst_resampling_u10,rst_resampling_tp)
# Stack
#r_stack <- stack(AOD_550,blh_mean, sp_mean,d2m_mean,t2m_mean,v10_mean,u10_mean,tp_mean)

# Asignar nombres a las capas
names(r_stack) <- c("blh_mean", "sp_mean","d2m_mean","t2m_mean","v10_mean","u10_mean","tp_mean")

###################################################################################
# Esto es con la version 4.1.1 de r sino no funciona
#MAIAC
data_sat <- "D:/Josefina/Proyectos/ProyectoChile/dataset/MAIAC/MCD19A2.A2015018.h12v12.061.2023071062256.hdf"
data_maiac <- raster(data_sat)
sds <- get_subdatasets(data_sat)
nband<-2
# Optical_Depth_055
gdal_translate(sds[2], dst_dataset = paste0('tmp055', basename(data_sat), '.tiff'), b = 1)
r.055_1 <- raster(paste0('tmp055', basename(data_sat), '.tiff'))

gdal_translate(sds[2], dst_dataset = paste0('tmp055', basename(data_sat), '.tiff'), b = 2)
r.055_2 <- raster(paste0('tmp055', basename(data_sat), '.tiff'))

gdal_translate(sds[2], dst_dataset = paste0('tmp055', basename(data_sat), '.tiff'), b = 3)
r.055_3 <- raster(paste0('tmp055', basename(data_sat), '.tiff'))

gdal_translate(sds[2], dst_dataset = paste0('tmp055', basename(data_sat), '.tiff'), b = 4)
r.055_4 <- raster(paste0('tmp055', basename(data_sat), '.tiff'))

# Suponiendo que r.055 tiene el CRS correcto
r.055_projected_1 <- projectRaster(r.055_1, crs = crs(raster_template))
# Recortar el ráster utilizando la extensión del ráster de referencia
cropped_raster_2 <- crop(r.055_projected_1, extent(raster_template))

# Suponiendo que r.055 tiene el CRS correcto
r.055_projected_2 <- projectRaster(r.055_2, crs = crs(raster_template))
# Recortar el ráster utilizando la extensión del ráster de referencia
cropped_raster_2 <- crop(r.055_projected_2, extent(raster_template))

# Suponiendo que r.055 tiene el CRS correcto
r.055_projected_3 <- projectRaster(r.055_3, crs = crs(raster_template))
# Recortar el ráster utilizando la extensión del ráster de referencia
cropped_raster_3 <- crop(r.055_projected_3, extent(raster_template))# 

r.055_projected_4 <- projectRaster(r.055_4, crs = crs(raster_template))
# Recortar el ráster utilizando la extensión del ráster de referencia
cropped_raster_4 <- crop(r.055_projected_4, extent(raster_template))# 

cropped_raster_aod <- stack(cropped_raster_1,cropped_raster_2,cropped_raster_3,cropped_raster_4)
cropped_raster_aod <- calc(cropped_raster_aod,fun =mean)

writeRaster(cropped_raster_aod, filename = "D:/Josefina/Proyectos/ProyectoChile/modelos/cropped_raster_aod.tif", format = "GTiff", overwrite = TRUE)
writeRaster(rst_resampling_blh, filename = "D:/Josefina/Proyectos/ProyectoChile/modelos/rst_resampling_blh.tif", format = "GTiff", overwrite = TRUE)
# Resamplear la segunda imagen para que tenga la misma resolución y extensión que la primera
cropped_raster_aod_ext <- crop(cropped_raster_aod, extent(rst_resampling_blh))# 
raster2_resampled <- resample(cropped_raster_aod, rst_resampling_blh, method="bilinear")

# Stack
r_stack <- stack(raster2_resampled,rst_resampling_blh,rst_resampling_sp,
                 rst_resampling_d2m, rst_resampling_t2m,
                 rst_resampling_v10,rst_resampling_u10,rst_resampling_tp)


# Asignar nombres a las capas
names(r_stack) <- c("AOD_550","blh_mean", "sp_mean","d2m_mean","t2m_mean","v10_mean","u10_mean","tp_mean")
writeRaster(r_stack, filename = "D:/Josefina/Proyectos/ProyectoChile/modelos/r_stack_todas.tif", format = "GTiff", overwrite = TRUE)

##############################################################################
#"""""""" modelo

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

