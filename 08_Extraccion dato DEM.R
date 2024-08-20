# Topografía del terreno
# Los datos de elevación del terreno del área de la Comunidad Valenciana fueron descargados del
# sitio web del Consorcio de Información Espacial del CGIAR [Jarvis et al., 2008] con una
# resolución de 3 segundos de arco (90 m en el ecuador) y cobertura global
# (http://srtm.csi.cgiar.org/). 

# https://www.earthdata.nasa.gov/sensors/srtm
# 
# https://search.earthdata.nasa.gov/search/granules?p=C2763266360-LPCLOUD&pg[0][v]=f&pg[0][gsk]=-start_date&q=SRTM&sb[0]=-70.89258%2C-33.67673%2C-70.44434%2C-33.27248&tl=1724144331.039!3!!&lat=-33.40430741352033&long=-70.56298828125&zoom=7


library(terra)

# Cargar el archivo .hgt como un objeto SpatRaster
dem <- rast("D:/Josefina/Proyectos/ProyectoChile/DEM/S34W071.hgt")

# Mostrar información básica del raster
print(dem)

# Visualizar el DEM
plot(dem)

## Guardar el DEM como GeoTIFF
writeRaster(dem, "D:/Josefina/Proyectos/ProyectoChile/DEM/tiff/S34W071.tif",  overwrite = TRUE)

##################################################################
###############################################################
#Extraer datos de elevacion del dem
library(terra)

# Cargar el archivo raster
dem <- rast("D:/Josefina/Proyectos/ProyectoChile/DEM/tiff/S34W071.tif")

# Coordenadas del punto de interés (longitud y latitud en el CRS del raster)
# punto <- data.frame(lon=-70.659566, lat= -33.465694)#OHG
# punto <- data.frame(lon=-70.66517052, lat= -33.54606688)#BSQ
# punto <- data.frame(lon=-70.73210014, lat= -33.43301075)#CNA
# punto <- data.frame(lon=-70.58813772, lat= -33.51610874)#FLD
# punto <- data.frame(lon=-70.52346222, lat= -33.37639222)#CDE
# punto <- data.frame(lon=-70.7503877, lat= -33.43798487	)#PDH
# punto <- data.frame(lon=-70.59475058, lat= -33.59134682	)#PTA
punto <- data.frame(lon=-70.74822755, lat= -33.36576262)#QUI
# Extraer la elevación en el punto
elevacion <- extract(dem, punto)

# Mostrar la elevación
print(elevacion)

########################################################################
####################################################################
##################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # 

## 2 - Recortar mosaico  

crs_project <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "
# 
# ## Shape
shape <- readShapePoly("D:/Josefina/Proyectos/ProyectoChile/shape/data_referencia/santiago_4326.shp",
                       proj4string = CRS(crs_project))
# 
# Cargar el archivo raster
dem <- raster("D:/Josefina/Proyectos/ProyectoChile/DEM/tiff/S34W071.tif")

# # Recortar
data_recorte <- crop(dem, shape)  #recorto imagen para Valencia

# 
# # Resampling >> Uso imagen MODIS MCD19A2 como modelo para crear raster
# MCD19A2  <- raster("stack/month/AOD_mes_01_max.tif")
ext = extent(-70.89626564382297,-70.54544288411844,-33.61652852593954,-33.33591657301113)#

raster_template <- raster(nrows = 239, ncols = 158,
                          crs = crs_project ,
                          ext = extent(shape))  # toma las extensiones
#raster_template <- raster(nrows = 86, ncols = 87,   crs = crs_project, ext =ext)


data_resampling <- raster::resample(data_recorte, raster_template)


# Guardar
writeRaster(data_resampling , 
            "D:/Josefina/Proyectos/ProyectoChile/DEM/tiff/DEM_resampled2.tif", 
            format = "GTiff",
            overwrite = TRUE)
