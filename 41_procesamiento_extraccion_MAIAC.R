
# https://github.com/solrepresa/AQ-Valencia/blob/1732eab4542b64bb9429d7a89e9a4611b52acd12/src/AC_15.R#L7
rm(list=ls())

### Objetivo: Trabajar con NDVI de MOD13A3

crs_project = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
raster_template <- raster("D:/Josefina/Proyectos/ProyectoChile/SP/dataset/rasterTemplate/raster_template.tif")


data_estacciones <- read.csv("D:/Josefina/Proyectos/ProyectoChile/SP/dataset/sitios.csv")
data_estacciones <- data_estacciones[data_estacciones$Considerado == "SI",]
puntos <- data_estacciones
puntos$estacion <- puntos$Nombre.sitio 



dir_raster = "D:/Josefina/Proyectos/ProyectoChile/SP/dataset/00_MAIAC/"
setwd(dir_raster)

id <- dir(dir_raster, pattern = ".hdf")
df_rbind_tot <- data.frame()
#i<-1
#file.name <- id[i]
  
  #  Function to obtain info AOD 470 and 550 MAIAC
  #readMCD19A2 <- function(file.name, latlong.range = NULL) {
for (i in 1:length(id)){
    print(c("i",i))
    # Open the HDF file and get the data sets
    file.name <- id[i]
    sds <- get_subdatasets(file.name)
    
    # Get orbit information
    info <- GDALinfo(file.name,returnScaleOffset=FALSE)
    subdataset_metadata <- attr(info,"mdata")
    orbitas<-(subdataset_metadata)[59]
    orbit <- gsub(pattern = 'Orbit_time_stamp=', replacement = '', x = orbitas) 
    # Seperate the string array by spaces
    orbit <- unlist(strsplit(orbit, split = ' ')) 
    # Remove NA
    sub.idx <- which(nchar(orbit) != 0)
    orbit <- orbit[sub.idx]
    
    #For each orbit we obtain its information
    maiac.lst <- list()
    maiac.df.tot <- data.frame()
    date <- substr(file.name,10,16)
    df_rbind <- data.frame()
    #print(length(orbit))
    for (nband in 1 : length(orbit)) {
      print(c("orbit", nband))
      # Convert the dataset of interest to a raster (.tiff format)
      # Optical_Depth_047
      gdal_translate(sds[1], dst_dataset = paste0('tmp047', basename(file.name), '.tiff'), b = nband) # mask is band number
      r.047 <- raster(paste0('tmp047', basename(file.name), '.tiff'))
      
      # Optical_Depth_055
      gdal_translate(sds[2], dst_dataset = paste0('tmp055', basename(file.name), '.tiff'), b = nband)
      r.055 <- raster(paste0('tmp055', basename(file.name), '.tiff'))
      
      # AOD_QA
      gdal_translate(sds[6], dst_dataset = paste0('tmpqa', basename(file.name), '.tiff'), b = nband)
      r.qa <- raster(paste0('tmpqa', basename(file.name), '.tiff'))
      
     #  Projection transformation 
      SINU <- as.character(r.047@crs)
      
      #proj4string(r.qa ) <- CRS(r.qa )
      r.qa  <- projectRaster(r.qa ,crs = crs_project)
      r.047  <- projectRaster(r.047 ,crs = crs_project)
      r.055  <- projectRaster(r.055 ,crs = crs_project)
      #### OJO CON ESTO VER SI HACE FALTA EL FACTOR DE ESCALA O NO!!!!!!!!!!!!!!!
      r.047_factor <- r.047*0.001   #factor de escala
      r.055_factor <- r.055*0.001   #factor de escala
      
      # 1) Aplicar mascara de calidad (QA= 0000) a imagenes MODIS
      r.qa [  r.qa ] <- as.integer(substring(intToBin( r.qa [ r.qa ]), 4, 7)) #nos quedamos con los bits 8-11
      r.qa [  r.qa  != 0] <- NA
      #Aplicar m?scara
      r.055_factor_mask <- mask(r.055_factor ,  r.qa )
      r.047_factor_mask <- mask(r.047_factor ,  r.qa )
      
      r.055_resampling <- raster::resample(r.055_factor_mask , raster_template)
      r.047_resampling <- raster::resample(r.047_factor_mask , raster_template)
      
      
      
      
      
      valores_raster_055 <- extract(r.055_resampling, puntos[, c("long", "lat")])
      valores_raster_047 <- extract(r.047_resampling, puntos[, c("long", "lat")])
      puntos_con_valores <- puntos %>%
        mutate(valores_raster_055 = valores_raster_055) %>%
        mutate(valores_raster_047 = valores_raster_047)
      
      
      df <- data.frame (date= date,nband = nband, value_055=puntos_con_valores$valores_raster_055,
                        value_047=puntos_con_valores$valores_raster_047, estacion=puntos_con_valores$estacion, 
                        ID = puntos_con_valores$ID)
      

      #Delete temporary tiff files
      file.remove(dir('./', paste0('tmp')))
      df_rbind <- rbind(df_rbind,df)
      
    }
    df_rbind_media <- df_rbind %>%
      group_by(estacion, ID) %>%
      summarize(aod_550 = mean(value_055, na.rm = TRUE),
                aod_470 = mean(value_047, na.rm = TRUE))
    df_rbind_media$date <- date
    df_rbind_tot <- rbind(df_rbind_tot,df_rbind_media)
  }
      