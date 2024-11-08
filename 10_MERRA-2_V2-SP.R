# https://medium.com/@xhl272703370/tutorial-on-how-to-download-multiple-earthdata-urls-78c96df4c1c7
# wget --load-cookies ./.urs_cookies --save-cookies ./.urs_cookies --keep-session-cookies --user=josefina88 --ask-password --content-disposition -i subset_M2T1NXAER_5.12.4_20240820_105646_.txt
#######################################################################
########################################################################
# Extraccion de datos de MERRA-2
rm(list=ls())#
year <- 2024
for (j in 1:1){
  data_estacciones <- read.csv("D:/Josefina/Proyectos/ProyectoChile/SP/dataset/sitios.csv")
  data_estacciones <- data_estacciones[data_estacciones$Considerado == "SI",]
  puntos <- data_estacciones
  puntos$estacion <- puntos$Nombre.sitio 
  
  crs_project <- "+proj=longlat +datum=WGS84"
  #punto <- SpatialPoints(coords, proj4string = CRS(crs_project))
  
  #Variables: DMSSMASS,DMSSMASS, DUSMASS, OCSMASS, SO2SMASS, SO4SMASS, y SSSMASS
  
  dire <- paste("D:/Josefina/Proyectos/ProyectoChile/SP/dataset/04_MERRA-2_Dia/",year, sep="")
  setwd(dire)
  id <- list.files(path = getwd(),
                   pattern = "*.nc",
                   full.names = FALSE)
  raster_template <- raster("D:/Josefina/Proyectos/ProyectoChile/SP/dataset/rasterTemplate/raster_template.tif")
  for (i in 1:1){
  
    
    
    # archivo_nc<- "1MERRA2_400.tavg1_2d_aer_Nx.20150102.SUB.nc"
    # sds <- raster::stack(archivo_nc) 
    # "DMSSMASS",
    nameVar <- c("BCSMASS", "DUSMASS","DUSMASS25", "OCSMASS", "SO2SMASS", "SO4SMASS", "SSSMASS","SSSMASS25")
    
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
        valores_raster <- extract(rst_resampling, puntos[, c("long", "lat")])
        
        # Unir los valores del raster al dataframe original
        puntos_con_valores <- puntos %>%
          mutate(valor_raster = valores_raster)
  
        df <- data.frame (date= date,variable = name_sds, unidad=unit, value=puntos_con_valores$valor_raster,estacion=puntos_con_valores$estacion, ID = puntos_con_valores$ID)
        
        df_rbind <- rbind(df_rbind,df)
      }
      df_rbind_2<- rbind(df_rbind_2,df_rbind)
    }
  }
  #View(df_rbind_2)
  
  # Generamos dataframe por separado y arreglamos los nombres
  for (i in 1:1){
    BCSMASS <- df_rbind_2 [df_rbind_2$variable == "BCSMASS",]
    BCSMASS <- data.frame(date=BCSMASS$date,unidad_BCSMASS=BCSMASS$unidad,estacion = BCSMASS$estacion, ID = BCSMASS$ID,BCSMASS=BCSMASS$value)
    
    
    # DMSSMASS <- df_rbind_2 [df_rbind_2$variable == "DMSSMASS",]
    # DMSSMASS <- data.frame(date=DMSSMASS$date,unidad_DMSSMASS=DMSSMASS$unidad,
    #                        DMSSMASS=DMSSMASS$value)
    
    DUSMASS <- df_rbind_2 [df_rbind_2$variable == "DUSMASS",]
    DUSMASS <- data.frame(date=DUSMASS$date,unidad_DUSMASS=DUSMASS$unidad,
                          estacion  = DUSMASS$estacion,  ID= DUSMASS$ID, 
                          DUSMASS=DUSMASS$value)
    
    DUSMASS25 <- df_rbind_2 [df_rbind_2$variable == "DUSMASS25",]
    DUSMASS25 <- data.frame(date=DUSMASS25$date,unidad_DUSMASS25=DUSMASS25$unidad,
                            estacion = DUSMASS25$estacion, ID = DUSMASS25$ID,
                            DUSMASS25=DUSMASS25$value)
    
    OCSMASS <- df_rbind_2 [df_rbind_2$variable == "OCSMASS",]
    OCSMASS <- data.frame(date=OCSMASS$date,unidad_OCSMASS=OCSMASS$unidad,
                          estacion = OCSMASS$estacion, ID = OCSMASS$ID,
                          OCSMASS=OCSMASS$value)
    
    SO2SMASS <- df_rbind_2 [df_rbind_2$variable == "SO2SMASS",]
    SO2SMASS <- data.frame(date=SO2SMASS$date,unidad_SO2SMASS=SO2SMASS$unidad,
                           estacion = SO2SMASS$estacion, ID = SO2SMASS$ID,
                           SO2SMASS=SO2SMASS$value)
    
    SO4SMASS <- df_rbind_2 [df_rbind_2$variable == "SO4SMASS",]
    SO4SMASS <- data.frame(date=SO4SMASS$date,unidad_SO4SMASS=SO4SMASS$unidad,
                           estacion = SO4SMASS$estacion, ID = SO4SMASS$ID,
                           SO4SMASS=SO4SMASS$value)
    
    SSSMASS <- df_rbind_2 [df_rbind_2$variable == "SSSMASS",]
    SSSMASS <- data.frame(date=SSSMASS$date,unidad_SSSMASS=SSSMASS$unidad,
                          estacion = SSSMASS$estacion, ID = SSSMASS$ID,
                          SSSMASS=SSSMASS$value)
    
    SSSMASS25 <- df_rbind_2 [df_rbind_2$variable == "SSSMASS25",]
    SSSMASS25 <- data.frame(date=SSSMASS25$date,unidad_SSSMASS25=SSSMASS25$unidad,
                            estacion = SSSMASS25$estacion, ID = SSSMASS25$ID,
                            SSSMASS25=SSSMASS25$value)
    
    # Lista de dataframes
    dataframes <- list(BCSMASS,DUSMASS,DUSMASS25,OCSMASS,
                       SO2SMASS,SO4SMASS,SSSMASS,SSSMASS25) #DMSSMASS
    
    # Merge de todos los dataframes en la lista usando la columna 'date'
    merged_df <- Reduce(function(x, y) merge(x, y, by = c("ID","date"), all = TRUE), dataframes)
    
    
    
  }
  
  
  write.csv(merged_df,paste("D:/Josefina/Proyectos/ProyectoChile/SP/proceed/04_MERRA-2_Dia/MERRA-2_Dia_",year,".csv",sep=""))
}
View(merged_df)
######################################################################
########################################################################
# Generar raster / tif para hacer el mapaa


writeRaster(rst_resampling, "rst_resampling.tif", format="GTiff", overwrite=TRUE)
