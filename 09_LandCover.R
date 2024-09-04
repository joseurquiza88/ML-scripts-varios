## Land Cover
# Producto satelital MCD12Q1
# Resolucion 500 m- anual

# Ejemplo con una imagen
rm(list=ls())

dire <- paste("D:/Josefina/Proyectos/ProyectoChile/dataset/LandCover/data" ,sep="/")


#D:/Josefina/Proyectos/MODIS/MOD/dataset
id <- dir(dire, pattern = ".hdf")

setwd(dire)
#file <- id[77]
for (i in 1:1){
  #aeronet <- data.frame(-70.659566, -33.465694) #OHG
  #aeronet <- data.frame(-70.66517052,-33.54606688) #BSQ
  #aeronet <- data.frame(-70.73210014,-33.43301075) #CNA
  
  #aeronet <- data.frame(-70.58813772,-33.51610874) # FLD
  aeronet <- data.frame(-70.52346222,-33.37639222	) #CDE
  
  #aeronet <- data.frame(-70.7503877,-33.43798487	) #PDH
  #aeronet <- data.frame(-70.59475058,-33.59134682	) #PTA
  # aeronet <- data.frame(-70.74822755,-33.36576262) #QUI
  
  
  names(aeronet) <- c("Longitud", "Latitud")
  coordinates(aeronet) <- ~Longitud+Latitud
  proj4string(aeronet) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  custom.buffer <- function(p, r) {        #
    stopifnot(length(p) == 1)
    cust <- sprintf("+proj=aeqd +lat_0=%s +lon_0=%s +x_0=0 +y_0=0",    
                    p@coords[[2]], p@coords[[1]])
    projected <- spTransform(p, CRS(cust))                          
    buffered <- gBuffer(projected, width=r, byid=TRUE)              
    spTransform(buffered, p@proj4string)                            
  }
  buffered_1000 <- custom.buffer(aeronet, 1000)#este valor es el radio
  buffered_250 <- custom.buffer(aeronet, 250)#este valor es el radio
  buffered_3000 <- custom.buffer(aeronet, 3000) # 3 km
  buffered_1500 <- custom.buffer(aeronet, 1500) # 3 km
  # buffered_5000 <- custom.buffer(aeronet, 5000) #5 km
  # buffered_15000 <- custom.buffer(aeronet, 15000) #15 km
  # buffered_30000 <- custom.buffer(aeronet, 25000)
}

LandCover_function <- function(file, latlong.range = NULL, border.shp = NULL) {
  landCover_tot <- data.frame()
  f_LC_Type1_tot <- data.frame()
  #info <- gdalinfo(file)
  sds <- get_subdatasets(file)
  #subdataset_metadata <- attr(info,"mdata")
  
  year <- substr(file,10,13)
  tile <- substr(file,18,23)
  
  #Image_Optical_Depth_Land_And_Ocean"
  gdal_translate(sds[1], dst_dataset = paste0('LC_Type1', basename(file), '.tiff'))#, b = nUHnd) # mask is UHnd number
  LC_Type1<- raster(paste0('LC_Type1', basename(file), '.tiff'))
  f_LC_Type1_wgs84 <- projectRaster(LC_Type1,
                                    crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  # Guardar el raster procesado
  #writeRaster(f_LC_Type1_wgs84, "f_LC_Type1_wgs84.tif", format="GTiff", overwrite=TRUE)
  
  f_LC_Type1 <- raster::as.data.frame(f_LC_Type1_wgs84, xy = T)
  
  gdal_translate(sds[2], dst_dataset = paste0('LC_Type2', basename(file), '.tiff'))#, b = nUHnd) # mask is UHnd number
  LC_Type2<- raster(paste0('LC_Type2', basename(file), '.tiff'))
  f_LC_Type2_wgs84 <- projectRaster(LC_Type2,crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  f_LC_Type2 <- raster::as.data.frame(f_LC_Type2_wgs84, xy = T)
  
  gdal_translate(sds[3], dst_dataset = paste0('LC_Type3', basename(file), '.tiff'))#, b = nUHnd) # mask is UHnd number
  LC_Type3<- raster(paste0('LC_Type3', basename(file), '.tiff'))
  f_LC_Type3_wgs84 <- projectRaster(LC_Type3,crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  f_LC_Type3 <- raster::as.data.frame(f_LC_Type3_wgs84, xy = T)
  
  gdal_translate(sds[4], dst_dataset = paste0('LC_Type4', basename(file), '.tiff'))#, b = nUHnd) # mask is UHnd number
  LC_Type4<- raster(paste0('LC_Type4', basename(file), '.tiff'))
  f_LC_Type4_wgs84 <- projectRaster(LC_Type4,crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  f_LC_Type4 <- raster::as.data.frame(f_LC_Type4_wgs84, xy = T)  
  
  gdal_translate(sds[5], dst_dataset = paste0('LC_Type5', basename(file), '.tiff'))#, b = nUHnd) # mask is UHnd number
  LC_Type5<- raster(paste0('LC_Type5', basename(file), '.tiff'))
  f_LC_Type5_wgs84 <- projectRaster(LC_Type5,crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  f_LC_Type5 <- raster::as.data.frame(f_LC_Type5_wgs84, xy = T)
  
  colnames(f_LC_Type1) <- c("x", "y", "LandCover_IGBP")
  colnames(f_LC_Type2) <- c("x", "y", "LandCover_UMD")
  colnames(f_LC_Type3) <- c("x", "y", "LandCover_LAI")
  colnames(f_LC_Type4) <- c("x", "y", "LandCover_BGC")
  colnames(f_LC_Type5) <- c("x", "y", "LandCover_PFT")
  
  f_LC_Types <- data.frame(y = f_LC_Type1$y, x = f_LC_Type1$x,
                           LandCover_IGBP = f_LC_Type1$LandCover_IGBP,
                           LandCover_UMD = f_LC_Type2$LandCover_UMD,
                           LandCover_LAI = f_LC_Type3$LandCover_LAI,
                           LandCover_BGC = f_LC_Type4$LandCover_BGC,
                           LandCover_PFT = f_LC_Type5$LandCover_PFT)
  
  
  f_LC_Types$LandCover_IGBP[f_LC_Types$LandCover_IGBP == 255] <- NA
  f_LC_Types$LandCover_UMD[f_LC_Types$LandCover_UMD == 255] <- NA
  f_LC_Types$LandCover_LAI[f_LC_Types$LandCover_LAI == 255] <- NA
  f_LC_Types$LandCover_BGC[f_LC_Types$LandCover_BGC == 255] <- NA
  f_LC_Types$LandCover_PFT[f_LC_Types$LandCover_PFT == 255] <- NA
  
  
  file.remove(dir('./', paste0('LC_Type1', basename(file), '*')))
  file.remove(dir('./', paste0('LC_Type2', basename(file), '*')))
  file.remove(dir('./', paste0('LC_Type3', basename(file), '*')))
  file.remove(dir('./', paste0('LC_Type4', basename(file), '*')))
  file.remove(dir('./', paste0('LC_Type5', basename(file), '*')))
  
  
  
  # --- Cut the border --- #
  if (!is.null(latlong.range)) { # Using lat/long
    # Check if latlong.range is legal
    if (latlong.range[1] >= -180 & latlong.range[1] <= 180 & latlong.range[2] >= -180 & latlong.range[2] <= 180 &
        latlong.range[3] >= -90 & latlong.range[3] <= 90 & latlong.range[4] >= -90 & latlong.range[4] <= 90 &
        latlong.range[1] <= latlong.range[2] & latlong.range[3] <= latlong.range[4]) {
      #corto en el buffer
      
      
      f_LC_Types_sub <- subset(data_raster, y >= latlong.range[3] &
                                 y <= latlong.range[4] &
                                 x >= latlong.range[1] &
                                 x <= latlong.range[2])
      
      
    } else {
      stop('Illegal lat/long ranges. Please check the value of `latlong.range`. It should be `c(lon.min, lon.max, lat.min, lat.max)`')
    }
    
  }
  
  
  #Si hay dato de coordenadas
  if (nrow(f_LC_Types_sub) > 0) { #si hay filas
    # --- Add Time Stamp --- #
    f_LC_Types_tot<- data.frame(year = year,tile=tile,
                                LandCover_IGBP =  round(mean(f_LC_Types_sub$LandCover_IGBP,na.rm=TRUE),1),
                                LandCover_UMD =  round(mean(f_LC_Types_sub$LandCover_UMD,na.rm=TRUE),1),
                                LandCover_LAI =  round(mean(f_LC_Types_sub$LandCover_LAI,na.rm=TRUE),1),
                                LandCover_BGC =  round(mean(f_LC_Types_sub$LandCover_BGC,na.rm=TRUE),1),
                                LandCover_PFT =  round(mean(f_LC_Types_sub$LandCover_PFT,na.rm=TRUE),1))
    f_LC_Types_tot<- data.frame(year = year,tile=tile,
                                x=f_LC_Types_sub$x,y=f_LC_Types_sub$y,
                                LandCover_IGBP =  f_LC_Types_sub$LandCover_IGBP,
                                LandCover_UMD =  f_LC_Types_sub$LandCover_UMD,
                                LandCover_LAI =  f_LC_Types_sub$LandCover_LAI,
                                LandCover_BGC =  f_LC_Types_sub$LandCover_BGC,
                                LandCover_PFT =  f_LC_Types_sub$LandCover_PFT)
    
  }else {
    
    #Si no hay dato de coordenadas COMO HACER PARA QUE ME PONGA NULL
    f_LC_Types_tot<- data.frame(year = year[1],tile=tile[1],
                                x=NA,y=NA,
                                LandCover_IGBP =  NA,
                                LandCover_UMD =  NA,
                                LandCover_LAI =  NA,
                                LandCover_BGC =  NA,
                                LandCover_PFT =  NA)
    
  }
  landCover_tot <- rbind(landCover_tot,f_LC_Types_tot)
  return(landCover_tot)
}




################             RUN THE FUNCTION             ################

#Interest buffer
buf_250 <- buffered_250
hdf_df_250 <- data.frame()

for (i in 1:length(id)){
  print(i)
  print(Sys.time())
  df_250 <- LandCover_function(file = id[i], latlong.range = c(buf_250@bbox[1],buf_250@bbox[3],buf_250@bbox[2],buf_250@bbox[4]))
  hdf_df_250  <- rbind( hdf_df_250 ,df_250)
  
}
hdf_df_250_2 <- hdf_df_250[complete.cases(hdf_df_250),]
view(hdf_df_250_2)
write.csv(hdf_df_250_2,"D:/Josefina/Proyectos/ProyectoChile/proceed/LandCover/07_CDE_LandCover.csv")


####################################################################
#####################################################################
#####################################################################

file <- id[1]
sds <- get_subdatasets(file)
gdal_translate(sds[1], dst_dataset = paste0('LC_Type1', basename(file), '.tiff'))#, b = nUHnd) # mask is UHnd number
LC_Type1<- raster(paste0('LC_Type1', basename(file), '.tiff'))
f_LC_Type1_wgs84 <- projectRaster(LC_Type1,
                                  crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


# Verificar la nueva proyecci?n y resoluci?n
print(f_LC_Type1_wgs84)
plot(f_LC_Type1_wgs84)


## 2 - Recortar mosaico  

crs_project <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "
# 
# ## Shape
shape <- readShapePoly("D:/Josefina/Proyectos/ProyectoChile/shape/data_referencia/santiago_4326.shp",
                       proj4string = CRS(crs_project))
# 


# # Recortar
data_recorte <- crop(f_LC_Type1_wgs84, shape)  #recorto imagen para Valencia
plot(data_recorte)
print(data_recorte)
# 
# # Resampling >> Uso imagen MODIS MCD19A2 como modelo para crear raster
# MCD19A2  <- raster("stack/month/AOD_mes_01_max.tif")
ext = extent(-70.89626564382297,-70.54544288411844,-33.61652852593954,-33.33591657301113)#

raster_template <- raster(nrows = 239, ncols = 158,
                          crs = crs_project ,
                          ext = extent(shape))  # toma las extensiones

data_resampling <- raster::resample(data_recorte, raster_template)

plot(data_resampling)
print(data_resampling)
# Guardar
writeRaster(data_resampling , 
            "D:/Josefina/Proyectos/ProyectoChile/dataset/LandCover/data/tiff/LandCover_resampled.tif", 
            format = "GTiff",
            overwrite = TRUE)
