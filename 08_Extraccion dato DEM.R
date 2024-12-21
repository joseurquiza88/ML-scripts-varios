# Topograf?a del terreno
# Los datos de elevaci?n del terreno del ?rea de la Comunidad Valenciana fueron descargados del
# sitio web del Consorcio de Informaci?n Espacial del CGIAR [Jarvis et al., 2008] con una
# resoluci?n de 3 segundos de arco (90 m en el ecuador) y cobertura global
# (http://srtm.csi.cgiar.org/). 

# https://www.earthdata.nasa.gov/sensors/srtm
# 
# https://search.earthdata.nasa.gov/search/granules?p=C2763266360-LPCLOUD&pg[0][v]=f&pg[0][gsk]=-start_date&q=SRTM&sb[0]=-70.89258%2C-33.67673%2C-70.44434%2C-33.27248&tl=1724144331.039!3!!&lat=-33.40430741352033&long=-70.56298828125&zoom=7


library(terra)

# Cargar el archivo .hgt como un objeto SpatRaster
# dem <- rast("D:/Josefina/Proyectos/ProyectoChile/DEM/S34W071.hgt")
dem <- rast("D:/Josefina/Proyectos/ProyectoChile/Talca/dataset/DEM/S36W072.hgt")
# Mostrar informaci?n b?sica del raster
print(dem)

# Visualizar el DEM
plot(dem)

## Guardar el DEM como GeoTIFF
writeRaster(dem, "D:/Josefina/Proyectos/ProyectoChile/DEM/tiff/S34W071.tif",  overwrite = TRUE)
writeRaster(dem, "D:/Josefina/Proyectos/ProyectoChile/Talca/dataset/DEM/S36W072.tif",  overwrite = TRUE)

##################################################################
###############################################################
#Extraer datos de elevacion del dem
library(terra)

# Cargar el archivo raster
dem <- rast("D:/Josefina/Proyectos/ProyectoChile/DEM/tiff/S34W071.tif")

# Coordenadas del punto de inter?s (longitud y latitud en el CRS del raster)
# punto <- data.frame(lon=-70.659566, lat= -33.465694)#OHG
# punto <- data.frame(lon=-70.66517052, lat= -33.54606688)#BSQ
# punto <- data.frame(lon=-70.73210014, lat= -33.43301075)#CNA
# punto <- data.frame(lon=-70.58813772, lat= -33.51610874)#FLD
# punto <- data.frame(lon=-70.52346222, lat= -33.37639222)#CDE
# punto <- data.frame(lon=-70.7503877, lat= -33.43798487	)#PDH
# punto <- data.frame(lon=-70.59475058, lat= -33.59134682	)#PTA
punto <- data.frame(lon=-70.74822755, lat= -33.36576262)#QUI
# Extraer la elevaci?n en el punto
elevacion <- extract(dem, punto)

# Mostrar la elevaci?n
print(elevacion)

########################################################################
####################################################################
##################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # 

## 2 - Recortar mosaico  

crs_project <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "
# 
# ## Shape
#Santiago
# shape <- readShapePoly("D:/Josefina/Proyectos/ProyectoChile/shape/data_referencia/santiago_4326.shp",
#                        proj4string = CRS(crs_project))

polygon_shapefile <- readShapePoly("D:/Josefina/Proyectos/ProyectoChile/shape/data_referencia/talca_4326.shp",
                       proj4string = CRS(crs_project))
# 
# Cargar el archivo raster
# dem <- raster("D:/Josefina/Proyectos/ProyectoChile/DEM/tiff/S34W071.tif")
dem <- raster("D:/Josefina/Proyectos/ProyectoChile/Talca/dataset/DEM/S36W072.tif")

# # Recortar
data_recorte <- crop(dem, polygon_shapefile)  #recorto imagen para Valencia

# 
# # Resampling >> Uso imagen MODIS MCD19A2 como modelo para crear raster
# MCD19A2  <- raster("stack/month/AOD_mes_01_max.tif")
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

raster_template <- ndvi_raster


# raster_template <- raster(nrows = 239, ncols = 158,
#                           crs = crs_project ,
#                           ext = extent(shape))  # toma las extensiones
#raster_template <- raster(nrows = 86, ncols = 87,   crs = crs_project, ext =ext)


data_resampling <- raster::resample(data_recorte, raster_template)


# Guardar
# writeRaster(data_resampling , 
#             "D:/Josefina/Proyectos/ProyectoChile/DEM/tiff/DEM_resampled2.tif", 
#             format = "GTiff",
#             overwrite = TRUE)

writeRaster(data_resampling , 
            "D:/Josefina/Proyectos/ProyectoChile/talca/dataset/DEM/DEM_resampled.tif", 
            format = "GTiff",
            overwrite = TRUE)



#########ìPasamos a vector para verlo en el qgis
vector_data <- rasterToPolygons(data_resampling, dissolve = TRUE)

crs(vector_data) <- crs(data_resampling)

# Guardar el archivo como Shapefile
writeOGR(vector_data, "D:/Josefina/Proyectos/ProyectoChile/talca/dataset/DEM/","DEM_resampled" , driver = "ESRI Shapefile")



dem <- raster("D:/Josefina/Proyectos/ProyectoChile/SP/dataset/DEM/S36W072.tif")



#############################################################################
############################################################################
dem <- raster("D:/Josefina/Proyectos/ProyectoChile/SP/dataset/03_DEM/S24W047.hgt")
ndvi_raster <- raster("D:/Josefina/Proyectos/ProyectoChile/SP/dataset/rasterTemplate/raster_template.tif")
data_crop <- raster::resample(dem, ndvi_raster)
data_resampling <- raster::resample(data_crop, ndvi_raster)

writeRaster(data_resampling , 
            "D:/Josefina/Proyectos/ProyectoChile/SP/dataset/03_DEM/DEM_resampled.tif", 
            format = "GTiff",
            overwrite = TRUE)
