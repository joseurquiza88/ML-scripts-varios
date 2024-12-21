
#Generar grilla en formato shapefile a aprtir de una imagen tiff de referencia
# Leer el raster
raster_file <- "D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/tiff/NDVI_raster.tif"  # Reemplaza con la ruta a tu archivo raster
raster_data <- raster(raster_file)
raster_data <- nvdi_factor 
# Convertir el raster en polígonos (grilla)
raster_polygons <- rasterToPolygons(raster_data, dissolve = TRUE)
plot(raster_polygons)
class(raster_polygons)
dir <- "D:/Josefina/Proyectos/ProyectoChile/shape/data_referencia/"
# Guardar la grilla como un shapefile
writeOGR(raster_polygons, dsn = paste(dir,"grilla_1km.shp",sep=""), layer = "grilla_1km", driver = "ESRI Shapefile")


dir <- "D:/Josefina/Proyectos/ProyectoChile/shape/data_referencia/"
