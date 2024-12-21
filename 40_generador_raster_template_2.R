# sp

#ndvi_raster <- "D:/Josefina/Proyectos/ProyectoChile/SP/dataset/01_NDVI/MOD13A3.A2015001.h13v11.061.2021319205855.hdf"
ndvi_raster <- "D:/Josefina/Proyectos/ProyectoChile/MX/dataset/01_NDVI/MOD13A3.A2015091.h08v07.061.2021323202627.hdf"
setwd("D:/Josefina/Proyectos/ProyectoChile/MX/dataset/01_NDVI/")
file.name <- ndvi_raster
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

ext = extent(-46.95512 , -46.08302 ,-23.84281, -23.36005 )##

#raster_template <- raster(nrows = 86, ncols = 87,   crs = crs_project, ext =ext)
# Ejemplo de CRS, ajusta seg?n sea necesario

# Calcular la resoluci?n del p?xel en grados
# Asumiendo que las coordenadas est?n en grados, por ejemplo, si 1 grado = 111 km
km_per_degree <- 111.320  # Aproximadamente 111 km por grado
pixel_size_km <- 1  # Tama?o de p?xel deseado en km

# Calcular la resoluci?n en grados
resolution_degrees <- pixel_size_km / km_per_degree

# Calcular el n?mero de filas y columnas basados en la resoluci?n
nrows <- (ext@ymax - ext@ymin) / resolution_degrees
ncols <- (ext@xmax - ext@xmin) / resolution_degrees

# Redondear a n?meros enteros para filas y columnas
nrows <- ceiling(nrows)
ncols <- ceiling(ncols)

crs_project <- "+proj=longlat +datum=WGS84"
# Crear el raster con la extensi?n y resoluci?n adecuada
raster_template <- raster(nrows = nrows, ncols = ncols, crs = crs_project, ext = ext)


rst_resampling <- raster::resample(nvdi_factor, raster_template)

file.remove(dir('./', paste0('nvdi', basename(file.name), '*')))


# values(raster_template) <- runif(ncell(raster_template), min=0, max=100)
# plot(raster_template)
writeRaster(rst_resampling, "D:/Josefina/Proyectos/ProyectoChile/SP/dataset/rasterTemplate/raster_template_3.tif", format="GTiff", overwrite=TRUE)
