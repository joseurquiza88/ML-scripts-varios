# Topograf?a del terreno
# Los datos de elevaci?n del terreno del ?rea de la Comunidad Valenciana fueron descargados del
# sitio web del Consorcio de Informaci?n Espacial del CGIAR [Jarvis et al., 2008] con una
# resoluci?n de 3 segundos de arco (90 m en el ecuador) y cobertura global
# (http://srtm.csi.cgiar.org/). 

# https://www.earthdata.nasa.gov/sensors/srtm
# 
# https://search.earthdata.nasa.gov/search/granules?p=C2763266360-LPCLOUD&pg[0][v]=f&pg[0][gsk]=-start_date&q=SRTM&sb[0]=-70.89258%2C-33.67673%2C-70.44434%2C-33.27248&tl=1724144331.039!3!!&lat=-33.40430741352033&long=-70.56298828125&zoom=7


library(terra)
estacion <- "BA"
# Cargar el archivo .hgt como un objeto SpatRaster
# dem <- rast("D:/Josefina/Proyectos/ProyectoChile/DEM/S34W071.hgt")
# dem <- rast("D:/Josefina/Proyectos/ProyectoChile/Talca/dataset/DEM/S36W072.hgt")
# dem <- rast("D:/Josefina/Proyectos/ProyectoChile/SP/dataset/03_DEM/S24W047.hgt")
# dem <- rast("D:/Josefina/Proyectos/ProyectoChile/MX/dataset/03_DEM/N19W099.hgt")
dem <- rast("D:/Josefina/Proyectos/ProyectoChile/BA/dataset/03_DEM/S36W059.hgt")
# Mostrar informaci?n b?sica del raster
print(dem)

# Visualizar el DEM
plot(dem)

## Guardar el DEM como GeoTIFF
#writeRaster(dem, "D:/Josefina/Proyectos/ProyectoChile/DEM/tiff/S34W071.tif",  overwrite = TRUE)
#writeRaster(dem, "D:/Josefina/Proyectos/ProyectoChile/Talca/dataset/DEM/S36W072.tif",  overwrite = TRUE)
#writeRaster(dem, "D:/Josefina/Proyectos/ProyectoChile/SP/proceed/03_DEM/S24W047.tif",  overwrite = TRUE)
writeRaster(dem, "D:/Josefina/Proyectos/ProyectoChile/BA/proceed/03_DEM/S36W059.tif",  overwrite = TRUE)

##################################################################
###############################################################
#Extraer datos de elevacion del dem
library(terra)
estacion<- "BA"
# Cargar el archivo raster
dem <- rast(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/proceed/03_DEM/S35W059.tif",sep=""))
data_estacciones <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/dataset/estaciones/sitios_",estacion,".csv",sep=""))
data_estacciones <- data_estacciones[data_estacciones$Considerado == "SI",]
#data_estacciones <- data_estacciones[22:24,]
puntos <- data_estacciones
#puntos$estacion <- puntos$Nombre.sitio 

valores_raster <- extract(dem, puntos[, c("long", "lat")])

puntos_con_valores <- puntos %>%
  mutate(valor_raster = valores_raster)
puntos_con_valores$estacion <- puntos_con_valores$Nombre

df <- data.frame (estacion=puntos_con_valores$estacion, ID = puntos_con_valores$ID,valor = puntos_con_valores$valor_raster[2])
names(df) <- c("estacion", "ID","valor" )
View(df)
write.csv(df, paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/proceed/03_DEM/",estacion,"_DEM_S35W059.csv",sep=""))



#####################################################

estacion<- "BA"
# Cargar el archivo raster
dem <- raster(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/proceed/03_DEM/S35W059.tif",sep=""))
ndvi_raster <- raster(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/dataset/rasterTemplate/raster_template.tif",sep=""))
crs_project <- "+proj=longlat +datum=WGS84"
dir_era_guardado <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/dataset_ejemplo/Prediccion_2022/",sep="")

dem_proj <- projectRaster(dem,crs = crs_project)
resampled_dem <- raster::resample(dem_proj, ndvi_raster,method = "bilinear")
cropped_dem <- crop(resampled_dem, extent(ndvi_raster))
writeRaster(cropped_dem, paste(dir_era_guardado,"tiff/03_DEM/DEM_raster",sep=""), format="GTiff", overwrite=TRUE)

