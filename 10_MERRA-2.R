# https://medium.com/@xhl272703370/tutorial-on-how-to-download-multiple-earthdata-urls-78c96df4c1c7
# wget --load-cookies ./.urs_cookies --save-cookies ./.urs_cookies --keep-session-cookies --user=josefina88 --ask-password --content-disposition -i subset_M2T1NXAER_5.12.4_20240820_105646_.txt
#######################################################################
########################################################################
# Extraccion de datos de MERRA-2
rm(list=ls())#

#coords <- data.frame(x = -70.659566, y = -33.465694) #01 ohg  # Ejemplo con coordenadas de longitud y latitud
#coords <- data.frame(x = -70.66517052, y = -33.54606688) #02 BSQ
#coords <- data.frame(x = -70.7503877, y = -33.43798487) #03 PDH
#coords <- data.frame(x = -70.732100142, y = -33.43301075) #04 CNA
#coords <- data.frame(x = -70.59475058, y = -33.59134682	) #05 PTA
#coords <- data.frame(x = -70.732100142, y = -33.43301075) #06 FLD
coords <- data.frame(x = -70.52346222, y = -33.37639222) #07 CDE
#coords <- data.frame(x = -70.74822755, y = -33.36576262) #08 QUI
estacion <- "CDE"
num_estacion <- "07"

crs_project <- "+proj=longlat +datum=WGS84"
punto <- SpatialPoints(coords, proj4string = CRS(crs_project))



#Variables: DMSSMASS,DMSSMASS, DUSMASS, OCSMASS, SO2SMASS, SO4SMASS, y SSSMASS
year <- 2015
dire <- paste("D:/Josefina/Proyectos/ProyectoChile/dataset/MERRA-2/",year, "/01-12-",year,sep="")
setwd(dire)
id <- list.files(path = getwd(),
                 pattern = "*.nc",
                 full.names = FALSE)

for (i in 1:1){
  # ext = extent(-70.89626564382297,-70.54544288411844,-33.61652852593954,-33.33591657301113)
  ext = extent(-70.89626564382297,-70.49122311005938,-33.61652852593954,-33.33013269447868 )##
  #raster_template <- raster(nrows = 86, ncols = 87,   crs = crs_project, ext =ext)
  # Ejemplo de CRS, ajusta según sea necesario
  
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
  
  
  # archivo_nc<- "1MERRA2_400.tavg1_2d_aer_Nx.20150102.SUB.nc"
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
      valor <- raster::extract(rst_resampling, punto)
      
      df <- data.frame (date= date,variable = name_sds, unidad=unit, value=valor,estacion=estacion)
      
      df_rbind <- rbind(df_rbind,df)
    }
    df_rbind_2<- rbind(df_rbind_2,df_rbind)
  }
}
#View(df_rbind_2)

# Generamos dataframe por separado y arreglamos los nombres
for (i in 1:1){
  BCSMASS <- df_rbind_2 [df_rbind_2$variable == "BCSMASS",]
  BCSMASS <- data.frame(date=BCSMASS$date,unidad_BCSMASS=BCSMASS$unidad,BCSMASS=BCSMASS$value)
  
  
  DMSSMASS <- df_rbind_2 [df_rbind_2$variable == "DMSSMASS",]
  DMSSMASS <- data.frame(date=DMSSMASS$date,unidad_DMSSMASS=DMSSMASS$unidad,
                         DMSSMASS=DMSSMASS$value)
  
  DUSMASS <- df_rbind_2 [df_rbind_2$variable == "DUSMASS",]
  DUSMASS <- data.frame(date=DUSMASS$date,unidad_DUSMASS=DUSMASS$unidad,
                        DUSMASS=DUSMASS$value)
  
  DUSMASS25 <- df_rbind_2 [df_rbind_2$variable == "DUSMASS25",]
  DUSMASS25 <- data.frame(date=DUSMASS25$date,unidad_DUSMASS25=DUSMASS25$unidad,
                          DUSMASS25=DUSMASS25$value)
  
  OCSMASS <- df_rbind_2 [df_rbind_2$variable == "OCSMASS",]
  OCSMASS <- data.frame(date=OCSMASS$date,unidad_OCSMASS=OCSMASS$unidad,
                        OCSMASS=OCSMASS$value)
  
  SO2SMASS <- df_rbind_2 [df_rbind_2$variable == "SO2SMASS",]
  SO2SMASS <- data.frame(date=SO2SMASS$date,unidad_SO2SMASS=SO2SMASS$unidad,
                         SO2SMASS=SO2SMASS$value)
  
  SO4SMASS <- df_rbind_2 [df_rbind_2$variable == "SO4SMASS",]
  SO4SMASS <- data.frame(date=SO4SMASS$date,unidad_SO4SMASS=SO4SMASS$unidad,
                         SO4SMASS=SO4SMASS$value)
  
  SSSMASS <- df_rbind_2 [df_rbind_2$variable == "SSSMASS",]
  SSSMASS <- data.frame(date=SSSMASS$date,unidad_SSSMASS=SSSMASS$unidad,
                        SSSMASS=SSSMASS$value)
  
  SSSMASS25 <- df_rbind_2 [df_rbind_2$variable == "SSSMASS25",]
  SSSMASS25 <- data.frame(date=SSSMASS25$date,unidad_SSSMASS25=SSSMASS25$unidad,
                          SSSMASS25=SSSMASS25$value)
  
  # Lista de dataframes
  dataframes <- list(BCSMASS,DMSSMASS,DUSMASS,DUSMASS25,OCSMASS,
                     SO2SMASS,SO4SMASS,SSSMASS,SSSMASS25)
  
  # Merge de todos los dataframes en la lista usando la columna 'date'
  merged_df <- Reduce(function(x, y) merge(x, y, by = "date", all = TRUE), dataframes)
  
  
  merged_df$estacion <- estacion
}
view(merged_df)

write.csv(merged_df,paste("D:/Josefina/Proyectos/ProyectoChile/proceed/MERRA-2/",year,"/",num_estacion,"_",estacion,"_MERRA-2_",year,".csv",sep=""))
S#######################################################################
########################################################################
# Generar raster / tif para hacer el mapaa


writeRaster(rst_resampling, "rst_resampling.tif", format="GTiff", overwrite=TRUE)
