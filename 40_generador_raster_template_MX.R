
library(rgdal)      # Para leer archivos HDF
library(raster)     # Para trabajar con rasters
library(sp)         # Para manipulación espacial
library(sf)         # Para guardar en formato shapefile

# 1. Leer el archivo raster HDF
ndvi_raster <- "D:/Josefina/Proyectos/ProyectoChile/MX/dataset/01_NDVI/MOD13A3.A2015091.h08v07.061.2021323202627.hdf"
ndvi_raster <- "D:/Josefina/Proyectos/ProyectoChile/BA/dataset/01_NDVI/MOD13A3.A2019001.h13v12.061.2020286171958.hdf"

setwd("D:/Josefina/Proyectos/ProyectoChile/BA/dataset/01_NDVI/")
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

#MX
# 2. Crear el área de interés (AOI) con las coordenadas dadas
# coords <- matrix(c(-99.41324, 19.08584,  # Abajo-Izquierda
#                    -98.74565, 19.08733,  # Abajo-Derecha
#                    -98.75181, 19.83251,  # Arriba-Derecha
#                    -99.41598, 19.83186,  # Arriba-Izquierda
#                    -99.41324, 19.08584), # Cerrar el polígono
#                  ncol = 2, byrow = TRUE)
# asi aparece en rl google hacer mapa 20.24459, -99.87742
##### ----BA

coords <- matrix(c(-59.58923,-35.53499,  # Abajo-Izquierda
                   -57.51038,-35.53496,  # Abajo-Derecha
                   -57.56517,-33.92568,  # Arriba-Derecha
                   -59.62774,-33.95756,  # Arriba-Izquierda
                   -59.58923,-35.53499), # Cerrar el polígono
                 ncol = 2, byrow = TRUE)

# coords <- matrix(c(-35.16411, -59.10379,  # Abajo-Izquierda L
#                    -35.19109, -57.75798,  # Abajo-Derecha L
#                    -34.35641, -57.82939,  # Arriba-Derecha L
#                    -34.33146, -59.18895,  # Arriba-Izquierda L
#                    -35.16411, -59.10379), # Cerrar el polígono
#                  ncol = 2, byrow = TRUE)

aoi <- SpatialPolygons(list(Polygons(list(Polygon(coords)), "1")), 
                       proj4string = CRS(proj4string(nvdi_factor)))  # Usar la proyección del raster NDVI


# 3. Recortar el raster NDVI al área de interés
ndvi_crop <- crop(nvdi_factor, aoi)       # Recorta al extent del AOI
#ndvi_masked <- mask(ndvi_crop, aoi)  # Aplica el AOI como máscara



# 5. Guardar el raster de la grilla como archivo
# writeRaster(ndvi_crop, "D:/Josefina/Proyectos/ProyectoChile/MX/dataset/rasterTemplate/rasterTemplate_3.tif", format = "GTiff", overwrite = TRUE)
writeRaster(ndvi_crop, "D:/Josefina/Proyectos/ProyectoChile/BA/dataset/rasterTemplate/rasterTemplate.tif", format = "GTiff", overwrite = TRUE)

