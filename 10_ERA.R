# Procesamiento de era en el pixel de la estacion de monitoreo EMCI_DS-PM25
nameVar = "t2m"
process_era5 <- function (coordenadas_sitio,sitio,path){
  #hora_llegada <-  as.POSIXct(strptime(hora_interes  ,format = "%Y-%m-%d %H:%M:%S"))
  #hora <- paste(hour(hora_llegada),":00:00",sep="")
  
  crs_project = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  era.df<- data.frame()
  # Shape para cortar imagen ERA
  #shape <- readOGR("D:/Josefina/Proyectos/ERA/grilla/grilla.shp")
  # shape <- readOGR("D:/Josefina/Proyectos/ERA/grilla/poligono_BA2.shp")
  #Poligono de la estacion OHigging
  # shape <- read_sf("D:/Josefina/Proyectos/ERA/grilla/poligono_CH.shp")
  ####shape <- read_sf("D:/Josefina/Proyectos/ProyectoChile/shape/Poligono_estaciones/poligono_BSQ.shp")
  #abajo-izq, arriba derec
  
  
  # ext = extent(-58.9360651377158,-58.0779281124066,-34.9124999968662,-34.1958333302637)#EMCI_DS-PM25
  #CH
  # -33.61652852593954, -70.89626564382297, -33.33591657301113, -70.54544288411844
  # ext = extent(-70.89626564382297,-70.54544288411844,-33.61652852593954,-33.33591657301113)#
  ext = extent(-70.89626564382297,-70.49122311005938,-33.61652852593954,-33.33013269447868 )##
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
  dire <- path
  setwd(dire)
  id <- list.files(path = getwd(),
                   pattern = "*.nc",
                   full.names = FALSE)
  
  for (i in 1:length(id)){
    
    #print(paste("Esto es i = ", i, sep= ""))
    file.name = id[i]
    # Get the data sets
    # sds <- get_subdatasets(file.name)
    #In .varName(nc, varname, warn = warn) : varname used is: t2m
    #If that is not correct, you can set it to one of: t2m, d2m, sp, u10, v10, blh, tp
    sds <- raster::stack(file.name) 
    nameVar <- c("t2m", "d2m", "sp", "u10", "v10", "blh", "tp")
    for (num_sds in 1:length(nameVar)){
      # Get orbit information
      name_sds<- nameVar[num_sds] #substring(sds[num_sds],31)
      MIRRAraster <- raster(file.name,varname=name_sds)
      num_bands <- nbands(MIRRAraster)
      # --- For each orbit --- #
      
      era.df <- data.frame()
      #print(Sys.time())
      #24 hs
      for (nband in 1:num_bands) {
        if(nband %%100==0){
          print(print(paste("Esto es nband = ", nband, sep= "")))
        }
        
        MIRRAraster <- raster(file.name,varname=name_sds,b=nband)
        #print(Sys.time())# print(sds[1])    
        unidad <-  MIRRAraster@data@unit
        t <- MIRRAraster@z[[1]]
        tiempo <- substr(t,1,10)
        #timestamp <- lubridate::as_datetime(c(t*60*60), origin = "1900-01-01")
        
        # 2) Reproyectar
        # bilinear vs cubic vs ngb:
        #   
        #   bilinear es una opción común para datos continuos y produce resultados suaves.
        # cubic es más complejo y puede proporcionar una mejor calidad en algunos casos, pero es más computacionalmente intensivo.
        # ngb es ideal para datos categóricos o cuando se necesita una reproyección rápida sin cambios en los valores.
        MIRRAraster2 <- projectRaster(MIRRAraster,
                                      crs = crs_project,
                                      method = "bilinear")
        rst_resampling <- raster::resample(MIRRAraster2, raster_template)
        #rst_resampling <- raster::resample(data_recorte, raster_template)
        # writeRaster(rst_resampling, filename="mi_raster.tif", format="GTiff", overwrite=TRUE)
        # 4) Recortar
        # Crear un objeto SpatialPoints con las coordenadas del punto
        punto_coords <- matrix(coordenadas_sitio, ncol = 2, byrow = TRUE) #OHG
        #punto_coords <- matrix(c(-70.66517052,-33.54606688), ncol = 2, byrow = TRUE) #BSQ
        punto_sp <- SpatialPoints(punto_coords, proj4string = CRS("+proj=longlat +datum=WGS84"))
        
        # Extraer el valor del raster en el punto especificado
        valor <- extract(rst_resampling, punto_sp)
        #print(valor)
        
        # data_recorte <- crop(rst_resampling, shape)  #recorto imagen para CABA + LP
        # writeRaster(data_recorte, filename="mi_raster2.tif", format="GTiff", overwrite=TRUE)
        #data_recorte <- crop(MIRRAraster2, shape)  #recorto imagen para CABA + LP
        
        # df<- raster::as.data.frame(data_recorte, xy = T)
        df<- raster::as.data.frame(valor, xy = T)
        df$tiempo <- t
        df$unidad <- unidad
        df$variable <-name_sds
        df$date <- substr(df$tiempo,1,10)
        df$hora <- substr(df$tiempo,12,19)
        # names(df) <- c("x","y","valor","fecha","unidad","nombre_var","date","hora") 
        names(df) <- c("valor","fecha","unidad","nombre_var","date","hora") 
        era.df <- rbind(era.df,df)
        # names(era.df) <-c("x","y","valor","fecha","unidad","nombre_var","date","hora") 
        names(era.df) <-c("valor","fecha","unidad","nombre_var","date","hora") 
        rm(MIRRAraster, rst_resampling)  #data_recorte
      }
      #era.df_subset <- era.df [era.df$hora == hora,]
      #horario <- substr(hora,1,2)
      #nombre <-paste("./proceed/",id[i],"_",name_sds,"_",horario,".csv",sep = "")
      nombre <-paste("./",sitio,"/",id[i],"_",name_sds,".csv",sep = "")
      
      write.csv(era.df ,nombre)
      
      
    }
    
    
  }   
  
  
}
# Path es donde se encuentran los datos
# Se guardan dentro de esta misma carpeta pero dentro de la carpeta proceed
#path='D:/Josefina/Proyectos/ProyectoChile/dataset/meteoSatelital/2016/01-2016/'
# coordenadas = c(-70.659566, -33.465694) #OHG
#coordenadas = c(-70.66517052,-33.54606688) #BSQ
# coordenadas = c(-70.73210014 ,-33.43301075) #CNA
coordenadas = c( -70.52346222, -33.37639222) #07 CDE

estacion <- "proceed_CDE_01-06-2015"
path='D:/Josefina/Proyectos/ProyectoChile/dataset/meteoSatelital/2015/01-06-2015/'
prueba2 <- process_era5 (coordenadas_sitio= coordenadas,sitio=estacion, path='D:/Josefina/Proyectos/ProyectoChile/dataset/meteoSatelital/2016/11-2016/')
prueba2 <- process_era5 (coordenadas_sitio= coordenadas,sitio=estacion, path='D:/Josefina/Proyectos/ProyectoChile/dataset/meteoSatelital/2016/08-2016/')
prueba2 <- process_era5 (coordenadas_sitio= coordenadas,sitio=estacion, path='D:/Josefina/Proyectos/ProyectoChile/dataset/meteoSatelital/2016/09-2016/')
