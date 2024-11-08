# Procesamiento de era en el pixel de la estacion de monitoreo EMCI_DS-PM25
nameVar = "t2m"
#process_era5 <- function (coordenadas_sitio,sitio,path){
  year <- 2017
  data_estacciones <- read.csv("D:/Josefina/Proyectos/ProyectoChile/SP/dataset/sitios.csv")
  data_estacciones <- data_estacciones[data_estacciones$Considerado == "SI",]
  puntos <- data_estacciones
  puntos$estacion <- puntos$Nombre.sitio 

  crs_project <- "+proj=longlat +datum=WGS84"

  #dire <- paste("D:/Josefina/Proyectos/ProyectoChile/SP/dataset/meteoSatelital/",year, "/01-03-2017",sep="")
  dire <- paste("D:/Josefina/Proyectos/ProyectoChile/SP/dataset/meteoSatelital/",year,sep="")
  
  setwd(dire)

  raster_template <- raster("D:/Josefina/Proyectos/ProyectoChile/SP/dataset/rasterTemplate/raster_template.tif")
  era.df<- data.frame()
  id <- list.files(path = getwd(),
                   pattern = "*.nc",
                   full.names = FALSE)
  
  
  for (i in 1:length(id)){
    
    #print(paste("Esto es i = ", i, sep= ""))
    file.name = id[i]
    print(file.name)
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
      date <- substr(file.name,1,10)
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
        timestamp <- as.POSIXct(t, origin = "1970-01-01", tz = "UTC")
        
        hora <- substr(timestamp,12,19)
        
        
        MIRRAraster2 <- projectRaster(MIRRAraster,
                                      crs = crs_project,
                                      method = "bilinear")
        rst_resampling <- raster::resample(MIRRAraster2, raster_template)
        valores_raster <- extract(rst_resampling, puntos[, c("long", "lat")])
        
        # Unir los valores del raster al dataframe original
        puntos_con_valores <- puntos %>%
          mutate(valor_raster = valores_raster)
        
        df <- data.frame (date= date, fecha = timestamp, hora = hora, nombre_var = name_sds, unidad=unidad, valor=puntos_con_valores$valor_raster,estacion=puntos_con_valores$estacion, ID = puntos_con_valores$ID)

        era.df <- rbind(era.df,df)

        rm(MIRRAraster, rst_resampling)  #data_recorte
      }

      nombre <-paste("D:/Josefina/Proyectos/ProyectoChile/SP/proceed/05_ERA5/",year,"/",id[i],"_",name_sds,".csv",sep = "")
      
      write.csv(era.df ,nombre)
      
      
    }
    
    
  }   

    
  

