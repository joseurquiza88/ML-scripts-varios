setwd("D:/Josefina/Proyectos/ProyectoChile/Talca/dataset/NDVI/NDVI_raster")

data_ndvi <- "MOD13A3.A2019001.h12v12.061.2020286171223.hdf"
sds <- get_subdatasets(data_ndvi)
gdal_translate(sds[1], dst_dataset = paste0('nvdi', basename(data_ndvi), '.tiff'))
nvdi <- raster(paste0('nvdi', basename(data_ndvi), '.tiff'))
crs_project <- "+proj=longlat +datum=WGS84"

SINU <- as.character(nvdi@crs)
proj4string(nvdi) <- CRS(SINU)
nvdi <- projectRaster(nvdi,crs = crs_project)
nvdi_factor <- nvdi*0.0001   #factor de escala 
# talca
#abajo izq / arriba der
ext = extent(-71.79976,-71.506606,-35.55334,-35.313416)

km_per_degree <- 111  # Aproximadamente 111 km por grado
pixel_size_km <- 1  # Tama?o de p?xel deseado en km
resolution_degrees <- pixel_size_km / km_per_degree
nrows <- (ext@ymax - ext@ymin) / resolution_degrees
ncols <- (ext@xmax - ext@xmin) / resolution_degrees
nrows <- ceiling(nrows)
ncols <- ceiling(ncols)
raster_template <- raster(nrows = nrows, ncols = ncols, crs = crs_project, ext = ext)


rst_resampling <- raster::resample(nvdi_factor, raster_template)
writeRaster(rst_resampling, 
            "NDVI_raster.tif", 
            format = "GTiff",  # GeoTIFF es un formato común para rasters
            overwrite = TRUE)  # Sobrescribe el
# Convertir el raster a shapefile para visualización en QGIS
vector_data <- rasterToPolygons(rst_resampling, dissolve = TRUE)
crs(vector_data) <- crs(rst_resampling)

# Guardar el shapefile
writeOGR(vector_data, ".", "DEM_resampled2", driver = "ESRI Shapefile")
