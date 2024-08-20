# Productos MERRA - 2
# https://disc.gsfc.nasa.gov/datasets?keywords=tavg1_2d_aer_Nx&page=1&temporalResolution=1%20hour
# https://disc.gsfc.nasa.gov/datasets/M2T1NXAER_5.12.4/summary?keywords=tavg1_2d_aer_Nx

#Como descargar MERRA-2
# https://medium.com/@xhl272703370/tutorial-on-how-to-download-multiple-earthdata-urls-78c96df4c1c7
# wget --load-cookies ./.urs_cookies --save-cookies ./.urs_cookies --keep-session-cookies --user=josefina88 --ask-password --content-disposition -i subset_M2T1NXAER_5.12.4_20240820_105646_.txt
#######################################################################
########################################################################
# Extraccion de datos de MERRA-2
rm(list=ls())

punto <- cbind(lon = -70.659566, lat = -33.465694)  # Ejemplo con coordenadas de longitud y latitud
estacion <- "OHG"
#Variables: DMSSMASS,DMSSMASS, DUSMASS, OCSMASS, SO2SMASS, SO4SMASS, y SSSMASS
setwd("D:/Josefina/Proyectos/ProyectoChile/dataset/MERRA-2")
ext = extent(-70.89626564382297,-70.54544288411844,-33.61652852593954,-33.33591657301113)#
#raster_template <- raster(nrows = 86, ncols = 87,   crs = crs_project, ext =ext)
crs_project <- "+proj=longlat +datum=WGS84"  # Ejemplo de CRS, ajusta según sea necesario

# Calcular la resolución del píxel en grados
# Asumiendo que las coordenadas están en grados, por ejemplo, si 1 grado = 111 km
km_per_degree <- 111  # Aproximadamente 111 km por grado
pixel_size_km <- 1  # Tamaño de píxel deseado en km

# Calcular la resolución en grados
resolution_degrees <- pixel_size_km / km_per_degree

# Calcular el número de filas y columnas basados en la resolución
nrows <- (ext@ymax - ext@ymin) / resolution_degrees
ncols <- (ext@xmax - ext@xmin) / resolution_degrees

# Redondear a números enteros para filas y columnas
nrows <- ceiling(nrows)
ncols <- ceiling(ncols)

# Crear el raster con la extensión y resolución adecuada
raster_template <- raster(nrows = nrows, ncols = ncols, crs = crs_project, ext = ext)


dire <- "D:/Josefina/Proyectos/ProyectoChile/dataset/MERRA-2"
setwd(dire)
id <- list.files(path = getwd(),
                 pattern = "*.nc",
                 full.names = FALSE)

archivo_nc<- "1MERRA2_400.tavg1_2d_aer_Nx.20150102.SUB.nc"
# sds <- raster::stack(archivo_nc) 

nameVar <- c("BCSMASS","DMSSMASS", "DUSMASS","DUSMASS25", "OCSMASS", "SO2SMASS", "SO4SMASS", "SSSMASS","SSSMASS25")

df_rbind_2 <- data.frame()
for (i in 1:length(id)){
  print(i)
  #print(paste("Esto es i = ", i, sep= ""))
  archivo_nc = id[i]
  df_rbind <- data.frame()
  for (num_sds in 1:length(nameVar)){
    name_sds<- nameVar[num_sds] #substring(sds[num_sds],31)
    MIRRAraster <- raster(archivo_nc,varname=name_sds)
    unit<- MIRRAraster@data@unit
    date <- substr(archivo_nc,28,35)
    crs_project <- "+proj=longlat +datum=WGS84"
    MIRRAraster2 <- projectRaster(MIRRAraster,
                                  crs = crs_project,
                                  method = "bilinear")
    rst_resampling <- raster::resample(MIRRAraster2, raster_template)
  # Definir las coordenadas del punto
    valor <- extract(rst_resampling, punto)
  
  df <- data.frame (date= date,variable = name_sds, unidad=unit, value=valor,estacion=estacion)
  
  df_rbind <- rbind(df_rbind,df)
  }
  df_rbind_2<- rbind(df_rbind_2,df_rbind)
}
View(df_rbind_2)
#######################################################################
########################################################################
# Generar raster / tif para hacer el mapaa


writeRaster(rst_resampling, "rst_resampling.tif", format="GTiff", overwrite=TRUE)
