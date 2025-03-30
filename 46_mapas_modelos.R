
# Este codigo es el mismo que 22_codigo_MES
###########################################################################
###########################################################################
#Usar version de r 2.3.4
###########################################################################
# -----------------------   Todas las variables ---------------------------
###########################################################################
#rm(list=ls())
df_rbind <- data.frame()
# Sponiendo que mi_dataframe es el objeto que no quieres eliminar

for (l in 1:1){
rm(list = setdiff(ls(), "df_rbind"))
estacion <- "CH"
year<- 2024
numRaster <- "01"
#modelo <- "06-RF_cv_M1-090125-E_MX.RData"
#modelo <- "02-RF_cv_M4-270125-BA.RData"
#modelo <- "02-RF_cv_M1-251124_SP.RData"
# modelo <- "07-RF-ESP_cv_M6-110225-SP.RData"
# modelo <- "07-RF-ESP_cv_M6-110225-MD.RData"
#modelo <- "prueba_02-RF_cv_M1-120225-CH.RData"
# modelo <- "03-XGB_cv_M1-041024_CH.RData"
# modelo <- "07-RF_esp_cv_M6-180225-CH.RData"
# modelo <- "02-RF_cv_M1-070225-SP.RData
# modelo <- "07-RF_ESP_cv_M6-180225-MX.RData"
#modelo <- "02-RF_cv_M1-070225-MX.RData"
#modelo <- "02-RF_cv_M1-080125_MD.RData"
# modelo <- "02-XGB_M1-260225_SP.RData"
modelo <- "02-XGB_M1-260225_MX.RData"
#nombre_salida <- "02-RF_cv_M1-251124_SP_AOD-MERRA"
# nombre_salida <- "07-RF-ESP_cv_M6-110225-MD_MERRA"
nombre_salida <- "02-RF_cv_M1-120225-CH"
# nombre_salida <- "03-XGB_cv_M1-041024_CH"
# nombre_salida <- "07-RF_esp_cv_M6-180225-CH"
# nombre_salida <- "02-RF_cv_M1-070225-SP"
# nombre_salida <- "07-RF_ESP_cv_M6-180225-MX"
# nombre_salida <- "02-RF_cv_M1-070225-MX"
# nombre_salida <- "02-XGB_M1-260225_SP"
nombre_salida <- "02-XGB_M1-260225_MX"

setwd(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/dataset_ejemplo/Prediccion_",year,"/tiff/",sep=""))
dir_salida <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/salidas/SalidasDiarias/",nombre_salida,"/",sep="")
# Fechas de inter?1
fechaInicio <- as.Date("25-12-2024", format = "%d-%m-%Y")
fechaFin <- as.Date("31-12-2024", format = "%d-%m-%Y")
###
fechaNDVI<- as.Date("01-12-2024", format = "%d-%m-%Y")
lista_fecha <- data.frame(date=seq.Date(fechaInicio, fechaFin, by = "day"))
dir_modelos <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/modelo/",sep="")
load(paste(dir_modelos,modelo,sep=""))
}


#for (i in 1:1){
lista_fecha$date[j]
j<-1

for (j in 1:nrow(lista_fecha)) {
  
  print(j)
  fechaInteres <- as.Date(lista_fecha$date[j], format = "%d-%m-%Y")
  # fechaInteres <- as.Date("08-03-2024", format = "%d-%m-%Y")
  ################# -----     00 MAIAC     -----
  # 2024001-MAIAC_raster.tif
  # Convertir a d?a juliano respecto al 1 de enero del mismo a?o
  dayJulian <- as.numeric(fechaInteres - as.Date(paste0(format(fechaInteres, "%Y"), "-01-01"))) + 1
  yearInteres <- year(fechaInteres)
  if(nchar(dayJulian)==1){
    sep = "00"
  }
  if (nchar(dayJulian)==2) {
    sep = "0"
  }
  
  if (nchar(dayJulian)==3) {
    sep = ""
  }
  maiacDate <- paste(yearInteres,sep,dayJulian,sep = "")
  MAIAC_raster <- raster(paste("00_MAIAC/00_MAIAC_IDW/IDW-",maiacDate,"-MAIAC_raster_",numRaster,".tif",sep=""))
  #MAIAC_raster <- raster(paste("00_MAIAC/00_MAIAC_IDW/IDW-",maiacDate,"-MAIAC_raster.tif",sep=""))
  #MAIAC_raster <- raster(paste("00_MAIAC/",maiacDate,"-MAIAC_raster.tif",sep=""))
  #num_na_MAIAC <- sum(is.na(MAIAC_raster[]))
  #print(c("MAIAC",num_na_MAIAC))
  
  ################### -----     NDVI     -----
  # Como es modis tambien la fecha es juliana pero ojo es un dato mensual
  # fechaNDVI<- as.Date("01-01-2024", format = "%d-%m-%Y")
  
  #fechaNDVI<- as.Date("01-04-2024", format = "%d-%m-%Y")
  yearNdvi <- year(fechaNDVI)
  dayJulianNDVI <- as.numeric(fechaNDVI - as.Date(paste0(format(fechaNDVI, "%Y"), "-01-01"))) + 1
  
  if(nchar(dayJulianNDVI)==1){
    sep = "00"
  }
  if (nchar(dayJulianNDVI)==2) {
    sep = "0"
  }
  
  if (nchar(dayJulianNDVI)==3) {
    sep = ""
  }
  NDVIDate <- paste(yearInteres,sep,dayJulianNDVI,sep = "")
  
  NDVI_raster <- raster(paste("01_NDVI/",NDVIDate,"-NDVI_raster.tif",sep=""))
  #plot(NDVI_raster)
  #num_na_NDVI <- sum(is.na(NDVI_raster[]))
  #print(c("NDVI",num_na_NDVI))
  ################# -----     LandCover     -----
  #Este dato es anual por lo que tambien tenemos que setear la fecha dierente
  # Pero es MODIS = Dia juliano
  # MCD12Q1.A2024001.h12v12.061.2022169161028.hdf
  
  
  # yearLandCover <- year(fechaNDVI)
  # LandCoverDate <- paste(yearLandCover,"001",sep = "")
  #LandCover_raster <- raster(paste("02_LandCover/2015001-LandCover_raster.tif",sep=""))
  # LandCover_NA <- sum(is.na(LandCover_raster[]))
  # print(c("landCover",LandCover_NA))
  #plot(LandCover_raster)
  
  ################# -----     DEM     -----
  
  DEM_raster <- raster("03_DEM/DEM_raster.tif")
  #plot(DEM_raster)
  #DEM_raster_NA <- sum(is.na(DEM_raster[]))
  #print(c("DEM",DEM_raster_NA))
  ################# -----     MERRA-2     -----
  fechaInteres_MERRA <- gsub("-", "", fechaInteres)
  # MERRA-2 AOD
  #MAIAC_AOD_MERRA <- raster(paste("07_MERRA-2_Dia_AOD/",fechaInteres_MERRA,"-AOD-MERRA_raster.tif",sep=""))
  
  ## BCSMASS
  #BCSMASS_raster <- raster(paste("04_MERRA-2/",fechaInteres_MERRA,"-BCSMASS_raster.tif",sep=""))
  #BCSMASS_raster <- raster(paste("04_MERRA-2_dia/",fechaInteres_MERRA,"-BCSMASS_raster_",numRaster,".tif",sep=""))
  BCSMASS_raster <- raster(paste("04_MERRA-2_dia/",fechaInteres_MERRA,"-BCSMASS_raster.tif",sep=""))
  
  #BCSMASS_raster_NA <- sum(is.na(BCSMASS_raster[]))
  #print(c("BCSMASS",BCSMASS_raster_NA))
  #plot(BCSMASS_raster)
  
  ## DMSSMASS
  # DMSSMASS_raster <- raster(paste("04_MERRA-2/",fechaInteres_MERRA,"-DMSSMASS_raster.tif",sep=""))
  #DMSSMASS_raster <- raster(paste("04_MERRA-2_Dia/",fechaInteres_MERRA,"-DMSSMASS_raster.tif",sep=""))
  #DMSSMASS_raster_NA <- sum(is.na(DMSSMASS_raster[]))
  #print(c("DMSSMASS",DMSSMASS_raster_NA))
  #plot(DMSSMASS_raster)
  
  ## DUSMASS
  #DUSMASS_raster <- raster(paste("04_MERRA-2/",fechaInteres_MERRA,"-DUSMASS_raster.tif",sep=""))
  #DUSMASS_raster <- raster(paste("04_MERRA-2_DIA/",fechaInteres_MERRA,"-DUSMASS_raster_",numRaster,".tif",sep=""))
  
  DUSMASS_raster <- raster(paste("04_MERRA-2_DIA/",fechaInteres_MERRA,"-DUSMASS_raster.tif",sep=""))
  #DUSMASS_raster_NA <- sum(is.na(DUSMASS_raster[]))
  #print(c("DUSMASS",DUSMASS_raster_NA))
  #plot(DUSMASS_raster)
  
  ## DUSMASS25
  #DUSMASS25_raster <- raster(paste("04_MERRA-2/",fechaInteres_MERRA,"-DUSMASS25_raster.tif",sep=""))
  #DUSMASS25_raster <- raster(paste("04_MERRA-2_dia/",fechaInteres_MERRA,"-DUSMASS25_raster_",numRaster,".tif",sep=""))
  DUSMASS25_raster <- raster(paste("04_MERRA-2_dia/",fechaInteres_MERRA,"-DUSMASS25_raster.tif",sep=""))
  
  #DUSMASS25_raster_NA <- sum(is.na(DUSMASS25_raster[]))
  #print(c("DUSMASS25",DUSMASS25_raster_NA))
  #plot(DUSMASS25_raster)
  
  ## OCSMASS
  #OCSMASS_raster <- raster(paste("04_MERRA-2/",fechaInteres_MERRA,"-OCSMASS_raster.tif",sep=""))
  #OCSMASS_raster <- raster(paste("04_MERRA-2_dia/",fechaInteres_MERRA,"-OCSMASS_raster_",numRaster,".tif",sep=""))
  OCSMASS_raster <- raster(paste("04_MERRA-2_dia/",fechaInteres_MERRA,"-OCSMASS_raster.tif",sep=""))
  
  #OCSMASS_raster_NA <- sum(is.na(OCSMASS_raster[]))
  #print(c("OCSMASS",OCSMASS_raster_NA))
  #plot(OCSMASS_raster)
  
  ## SO2SMASS
  # SO2SMASS_raster <- raster(paste("04_MERRA-2/",fechaInteres_MERRA,"-SO2SMASS_raster.tif",sep=""))
  #SO2SMASS_raster <- raster(paste("04_MERRA-2_dia/",fechaInteres_MERRA,"-SO2SMASS_raster_",numRaster,".tif",sep=""))
  SO2SMASS_raster <- raster(paste("04_MERRA-2_dia/",fechaInteres_MERRA,"-SO2SMASS_raster.tif",sep=""))
  
  #SO2SMASS_raster_NA <- sum(is.na(SO2SMASS_raster[]))
  #print(c("SO2SMASS",SO2SMASS_raster_NA))
  #plot(SO2SMASS_raster)
  
  ## SO4SMASS
  #SO4SMASS_raster <- raster(paste("04_MERRA-2/",fechaInteres_MERRA,"-SO4SMASS_raster.tif",sep=""))
  #SO4SMASS_raster <- raster(paste("04_MERRA-2_dia/",fechaInteres_MERRA,"-SO4SMASS_raster_",numRaster,".tif",sep=""))
  SO4SMASS_raster <- raster(paste("04_MERRA-2_dia/",fechaInteres_MERRA,"-SO4SMASS_raster.tif",sep=""))
  
  #SO4SMASS_raster_NA <- sum(is.na(SO4SMASS_raster[]))
  #print(c("SO4SMASS",SO4SMASS_raster_NA))
  #plot(SO4SMASS_raster)
  
  ## SSSMASS
  #SSSMASS_raster <- raster(paste("04_MERRA-2/",fechaInteres_MERRA,"-SSSMASS_raster.tif",sep=""))
  #SSSMASS_raster <- raster(paste("04_MERRA-2_dia/",fechaInteres_MERRA,"-SSSMASS_raster_",numRaster,".tif",sep=""))
  SSSMASS_raster <- raster(paste("04_MERRA-2_dia/",fechaInteres_MERRA,"-SSSMASS_raster.tif",sep=""))
  #SSSMASS_raster_NA <- sum(is.na(SSSMASS_raster[]))
  #print(c("SSSMASS",SSSMASS_raster_NA))
  #plot(SSSMASS_raster)
  
  ## SSSMASS25
  #SSSMASS25_raster <- raster(paste("04_MERRA-2/",fechaInteres_MERRA,"-SSSMASS25_raster.tif",sep=""))
  #SSSMASS25_raster <- raster(paste("04_MERRA-2_dia/",fechaInteres_MERRA,"-SSSMASS25_raster_",numRaster,".tif",sep=""))
  SSSMASS25_raster <- raster(paste("04_MERRA-2_dia/",fechaInteres_MERRA,"-SSSMASS25_raster.tif",sep=""))
  
  #SSSMASS25_raster_NA <- sum(is.na(SSSMASS25_raster[]))
  #print(c("SSSMASS25",SSSMASS25_raster_NA))
  #plot(SSSMASS25_raster)
  
  ################# -----     ERA5     -----
  ## BLH     -----
  #BLH_raster <- raster(paste("05_ERA5/",fechaInteres,"-BLH_raster_",numRaster,".tif",sep=""))
  BLH_raster <- raster(paste("05_ERA5/",fechaInteres,"-BLH_raster.tif",sep=""))
  
  
  #plot(BLH_raster)
  #BLH_raster_raster_NA <- sum(is.na(BLH_raster[]))
  #print(c("BLH",BLH_raster_raster_NA))
  ## D2M
  #D2M_raster <- raster(paste("05_ERA5/",fechaInteres,"-D2M_raster_",numRaster,".tif",sep=""))
  
  D2M_raster <- raster(paste("05_ERA5/",fechaInteres,"-D2M_raster.tif",sep=""))
  #plot(D2M_raster)
  #D2M_raster_NA <- sum(is.na(D2M_raster[]))
  #print(c("D2M",D2M_raster_NA))
  ## T2M
  #T2M_raster <- raster(paste("05_ERA5/",fechaInteres,"-T2M_raster_",numRaster,".tif",sep=""))
  
  T2M_raster <- raster(paste("05_ERA5/",fechaInteres,"-T2M_raster.tif",sep=""))
  #plot(T2M_raster)
  #T2M_raster_NA <- sum(is.na(T2M_raster[]))
  #print(c("T2M",T2M_raster_NA))
  
  ## TP
  #TP_raster <- raster(paste("05_ERA5/",fechaInteres,"-TP_raster_",numRaster,".tif",sep=""))
TP_raster <- raster(paste("05_ERA5/",fechaInteres,"-TP_raster.tif",sep=""))
  
  #plot(TP_raster)
  #TP_raster_NA <- sum(is.na(TP_raster[]))
  #print(c("TP",TP_raster_NA))
  ## SP
  SP_raster <- raster(paste("05_ERA5/",fechaInteres,"-SP_raster.tif",sep=""))
  #SP_raster <- raster(paste("05_ERA5/",fechaInteres,"-SP_raster_",numRaster,".tif",sep=""))
  #plot(SP_raster)
  #SP_raster_NA <- sum(is.na(SP_raster[]))
  #print(c("SP",SP_raster_NA))
  ## V10
  V10_raster <- raster(paste("05_ERA5/",fechaInteres,"-V10_raster.tif",sep=""))
  
  #V10_raster <- raster(paste("05_ERA5/",fechaInteres,"-V10_raster_",numRaster,".tif",sep=""))
  #plot(V10_raster)
  #V10_raster_NA <- sum(is.na(V10_raster[]))
  #print(c("V10",V10_raster_NA))
  ## U10
  #U10_raster <- raster(paste("05_ERA5/",fechaInteres,"-U10_raster_",numRaster,".tif",sep=""))
  U10_raster <- raster(paste("05_ERA5/",fechaInteres,"-U10_raster.tif",sep=""))
  
  #plot(U10_raster)
  #U10_raster_NA <- sum(is.na(U10_raster[]))
  #print(c("U10",U10_raster_NA))
  ################# -----     DayWeek     -----
  ################# -----     ERA5     -----
  ## BLH     -----
  dayWeek_raster <- raster(paste("06_weekDay/",fechaInteres,"-weekDay_raster.tif",sep=""))
  #plot(dayWeek_raster)
  #dayWeek_raster_NA <- sum(is.na(dayWeek_raster[]))
  #print(c("dayWeek",dayWeek_raster_NA))
  ##### STACK
  
  r_stack <- stack(MAIAC_raster,NDVI_raster,#LandCover_raster,
                   BCSMASS_raster ,DUSMASS_raster,DUSMASS25_raster,
                   OCSMASS_raster,SO2SMASS_raster, SO4SMASS_raster,
                   SSSMASS_raster ,SSSMASS25_raster,BLH_raster,
                   SP_raster, D2M_raster, T2M_raster,V10_raster,
                   U10_raster,TP_raster,DEM_raster,dayWeek_raster)
  # 
  # r_stack <- stack(MAIAC_AOD_MERRA,NDVI_raster,#,#LandCover_raster
  #                  BCSMASS_raster ,DUSMASS_raster,DUSMASS25_raster,
  #                  OCSMASS_raster,SO2SMASS_raster, SO4SMASS_raster,
  #                  SSSMASS_raster ,SSSMASS25_raster,BLH_raster,
  #                  SP_raster, D2M_raster, T2M_raster,V10_raster,
  #                  U10_raster,TP_raster,DEM_raster,dayWeek_raster)
  # 
    #plot(r_stack)
  
  r_stack_df <- as.data.frame(r_stack, na.rm = TRUE)
  #names(r_stack_df)
  
  # names(r_stack_df) <- c( "AOD_055" ,"ndvi","LandCover","BCSMASS",
  #                         "DUSMASS","DUSMASS25",
  #                         "OCSMASS","SO2SMASS",
  #                         "SO4SMASS","SSSMASS",
  #                         "SSSMASS25","blh_mean" ,
  #                         "sp_mean","d2m_mean",
  #                         "t2m_mean","v10_mean",
  #                         "u10_mean" ,"tp_mean" , 
  #                         "DEM","dayWeek")
  
  names(r_stack_df) <- c( "AOD_055" ,"ndvi", #"LandCover",
                          "BCSMASS_dia","DUSMASS_dia",
                          "DUSMASS25_dia","OCSMASS_dia",
                          "SO2SMASS_dia","SO4SMASS_dia",
                          "SSSMASS_dia","SSSMASS25_dia",
                          "blh_mean" ,"sp_mean",
                          "d2m_mean","t2m_mean",
                          "v10_mean","u10_mean" ,
                          "tp_mean" , "DEM","dayWeek")
  ###############################################################
  ##################################################################
  ##############################################################
  ####
  
  # Aplicar el modelo
  # predictions <- predict(rf_cv_model, newdata = r_stack_df)

  # Para XGB
  X_test <- r_stack_df[ , c("AOD_055", "ndvi" ,"BCSMASS_dia", "DUSMASS_dia", "DUSMASS25_dia",
                         "OCSMASS_dia", "SO2SMASS_dia", "SO4SMASS_dia", "SSSMASS_dia",
                        "SSSMASS25_dia", "blh_mean", "sp_mean", "d2m_mean",
                       "t2m_mean", "v10_mean", "u10_mean", "tp_mean", "DEM","dayWeek")]
  # 
  # 
  dtest <- as.matrix(X_test)
  # # dtest <- xgb.DMatrix(data = as.matrix(X_test))
  #predictions <- predict(xgb_tuned, newdata = dtest)
  predictions <- predict(xgb_model, newdata = dtest)
  # dtest <- as.matrix(X_test)
  # dtrain <- as.matrix(X)
  
  
  # Crear un raster vac?o con la misma extensi?n y resoluci?n que el stack
  pred_raster <- raster(r_stack)

  # Asignar las predicciones al raster
  pred_raster[] <- NA  # Inicia con valores NA
  
  # Reinsertar las predicciones en las celdas correspondientes
  pred_raster[!is.na(values(r_stack[[1]]))] <- predictions
  #getwd()
  
  
  name_salida <- paste(dir_salida,"PM-",fechaInteres,"_",nombre_salida,".tif",sep="")
  writeRaster(pred_raster, filename = name_salida, format = "GTiff", overwrite = TRUE)
  
  # ##############################################################
  # # # Definir coordenadas (por ejemplo, latitud y longitud)
  # #for (x in 1:1){
  #   # Dataframe con coordenadas y nombres de estaciones
  #   puntos <- data.frame(
  #     lon = c(-70.659566, -70.66517052, -70.73210014, -70.58813772, -70.52346222, -70.7503877, -70.59475058, -70.74822755,-70.70497123797023,-70.71950144777533,-70.65122931421972),
  #     lat = c(-33.465694, -33.54606688, -33.43301075, -33.51610874, -33.37639222, -33.43798487, -33.59134682, -33.36576262,-33.4952767527216,-33.4928995961536,-33.422304539963896 ),
  #     estacion = c("OHG", "BSQ", "CNA", "FLD", "CDE", "PDH", "PTA", "QUI","CER-II","CER-I","IND")
  #   )
  # 
  #   # Extraer los valores del raster en las coordenadas especificadas
  #   valores_raster <- extract(pred_raster, puntos[, c("lon", "lat")])
  # 
  #   # Unir los valores del raster al dataframe original
  #   puntos_con_valores <- puntos %>%
  #     mutate(valor_raster = valores_raster)
  # 
  #   # Mostrar el dataframe resultante
  #   # print(puntos_con_valores)
  #   puntos_con_valores$date <- fechaInteres
  # 
  #   df_rbind <- rbind(df_rbind,puntos_con_valores)
}




# #loView(df_rbind)
# 
# df_rbind2 <- df_rbind[df_rbind$estacion == "CNA",]
# #write.csv(df_rbind, paste(dir_salida,"salida_03-XGB_cv_M1-041024_prediccion_2015.csv",sep=""))
# #manualmente lo unimos con las estaciones sinca
# 
# 
# ##############################################################################
# ##otra forma de extraer los datos
# 
# dir <- "D:/Josefina/Proyectos/ProyectoChile/modelos/Salidas/Salida_03-XGB_cv_M1-041024/"
# setwd(dir)
# id <- list.files(path = dir,
#                  pattern = "*.tif",
#                  full.names = FALSE)
# crs_project <- "+proj=longlat +datum=WGS84"
# df_rbind <- data.frame()
# #for (x in 1:1){
# # Dataframe con coordenadas y nombres de estaciones
# puntos <- data.frame(
#   lon = c(-70.659566, -70.66517052, -70.73210014, -70.58813772, -70.52346222, -70.7503877, -70.59475058, -70.74822755,-70.70497123797023,-70.71950144777533,-70.65122931421972,-70.72379958800373),
#   lat = c(-33.465694, -33.54606688, -33.43301075, -33.51610874, -33.37639222, -33.43798487, -33.59134682, -33.36576262,-33.4952767527216,-33.4928995961536,-33.422304539963896,-33.34963630804128 ),
#   estacion = c("OHG", "BSQ", "CNA", "FLD", "CDE", "PDH", "PTA", "QUI-I","CER-II","CER-I","IND", "QUI")
# )
# for (i in 1:length(id)){
#   print(i)
#   pred_raster <- raster(id[i])
#   
#   #plot(pred_raster)
#   # Extraer los valores del raster en las coordenadas especificadas
#   valores_raster <- extract(pred_raster, puntos[, c("lon", "lat")])
#   
#   # Unir los valores del raster al dataframe original
#   puntos_con_valores <- puntos %>%
#     mutate(valor_raster = valores_raster)
#   
#   fechaInteres <- as.Date(substr(id[i],4,13), format = "%Y-%m-%d")# Mostrar el dataframe resultante
#   # print(puntos_con_valores)
#   puntos_con_valores$date <- fechaInteres
#   
#   df_rbind <- rbind(df_rbind,puntos_con_valores)
# }
# 
# dir_salida <- "D:/Josefina/Proyectos/ProyectoChile/modelos/Salidas/Salida_03-XGB_cv_M1-041024/"
# 
# 
# write.csv(df_rbind, paste(dir_salida,"Salida_03-XGB_cv_M1-041024.csv",sep=""))
# 
# #############################################################################
# 
# 
# 
# 
# Interestyear = 2024
# # data_estaciones_2024 <- read.csv("D:/Josefina/Proyectos/ProyectoChile/dataset/estaciones/diarios/PM25_2024_tot_validados.csv")
# # data_estaciones_2024$date <- as.POSIXct(as.character(data_estaciones_2024$date), format = "%y%m%d")
# data_estaciones <- read.csv("D:/Josefina/Proyectos/ProyectoChile/dataset/estaciones/diarios/PM25_tot.csv",colClasses = c("FECHA..YYMMDD."  = "character"))
# data_estaciones$date <- as.POSIXct(as.character(data_estaciones$FECHA..YYMMDD.), format = "%y%m%d")
# #data_estaciones <- data_estaciones[year(data_estaciones$date) == Interestyear,]
# data_estaciones <- data_estaciones[complete.cases(data_estaciones$Registros.completos),]
# 
# length(unique(data_estaciones$estacion))
# data_estaciones <- data_estaciones[data_estaciones$estacion == "CER-I" |
#                                      data_estaciones$estacion == "CER-II" |
#                                      data_estaciones$estacion == "IND" |
#                                      data_estaciones$estacion == "QUI-I" ,]
# 
# 
# # prediccion_2024 <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/Prediccion_01-2024/Salida/Salida_02-XGB_cv_M4-300924/salida_02-XGB_cv_M4-300924_01-2024.csv")
# # prediccion_2024 <- prediccion_2024[complete.cases(prediccion_2024$valor_raster),]
# # prediccion_2024$date <- as.POSIXct(as.character(prediccion_2024$date), format = "%d/%m/%Y")
# #prediccion_modelo <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/Prediccion_2015/Salida/Salida_03-XGB_cv_M1-041024/salida_03-XGB_cv_M1-041024_prediccion_2015.csv")
# #prediccion_modelo <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/Prediccion_2015/Salida/Salida_02-XGB_cv_M1-300924/Salida_02-XGB_cv_M1-300924.csv")
# prediccion_modelo <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/Salidas/SalidasDiarias/Salida_03-XGB_cv_M1-041024/Salida_03-XGB_cv_M1-041024.csv")
# 
# prediccion_modelo <- prediccion_modelo[complete.cases(prediccion_modelo$valor_raster),]
# prediccion_modelo$date <- as.POSIXct(as.character(prediccion_modelo$date), format = "%Y-%m-%d")
# 
# 
# # Ejemplo: Hacer un merge entre puntos y valores_raster basado en dos columnas
# data_merge <- prediccion_modelo %>%
#   left_join(data_estaciones, by = c("date", "estacion")) #%>%  # Merge basado en dos columnas
# #mutate(valor_raster_2 = preliminares)  # Crear una nueva columna con los valores del raster
# #write.csv(data_merge,"D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/Prediccion_01-2024/Salida/Salida_02-XGB_cv_M4-300924/salida_02-XGB_cv_M4-300924_01-2024_MERGE.csv")
# 
# 
# # data_pm <- data_merge[complete.cases(data_merge$Registros.validados),]
# data_pm <- data_merge[complete.cases(data_merge$Registros.completos),]
# data_pm <- data_pm[data_pm$valor_raster>0,]
# data_pm <- data_pm[complete.cases(data_pm$estacion),]
# # Crear scatter plot con l?nea de regresi?n
# #modelo <- lm(X02_RF.CV.M1.090924 ~ valor_sinca, data = data_pm)
# # modelo <- lm(valor_raster ~ Registros.validados, data = data_pm)
# modelo <- lm(valor_raster ~ Registros.completos, data = data_pm)
# # Calcular R?
# r2 <- summary(modelo)$r.squared
# 
# # Calcular RMSE (Root Mean Squared Error)
# #rmse <- sqrt(mean((data_pm$X02_RF.CV.M1.090924 - predict(modelo))^2))
# rmse <- sqrt(mean((data_pm$valor_raster - predict(modelo))^2))
# 
# # Calcular Bias
# # bias <- mean(data_pm$X02_RF.CV.M1.090924 - data_pm$valor_sinca)
# bias <- mean(data_pm$valor_raster - data_pm$Registros.completos)
# 
# # pearson <- cor(data_pm$valor_sinca, data_pm$X02_RF.CV.M1.090924, method = "pearson")
# pearson <- cor(data_pm$Registros.completos, data_pm$valor_raster, method = "pearson")
# # Crear scatter plot con l?nea de regresi?n
# #ggplot(data_pm, aes(x = valor_sinca, y = X02_RF.CV.M1.090924)) +
# plot <- ggplot(data_pm, aes(x = Registros.completos , y = valor_raster)) +
#   geom_point(color = "#99d8c9", alpha = 0.8, size = 2) +  # Puntos del scatter plot
#   geom_smooth(method = "lm", color = "red", se = FALSE) +  # Línea de regresión lineal
#   labs(x = "SINCA", y = "Predicción", title = "Concentraciones SINCA (n=9) vs Predicción 2024") +
#   theme_classic() +
#   
#   scale_x_continuous(limits = c(0, 160), breaks = seq(0, 160, by = 40)) +  # Ticks en el eje X
#   scale_y_continuous(limits = c(0, 160), breaks = seq(0, 160, by = 40)) +  # Ticks en el eje Y
#   
#   # Añadir texto para R^2
#   geom_text(aes(x = 5, y = 120),
#             label = expression(R^2 == 0.71),  # Coloca aquí el valor real de R^2
#             size = 4, color = "Black") +
#   
#   # Añadir texto para coeficiente de correlación Pearson (r)
#   geom_text(aes(x = 4.2, y = 110), 
#             label = paste("r = ", round(pearson, 2)), 
#             size = 4, color = "Black") +
#   
#   # Añadir texto para RMSE
#   geom_text(aes(x = 7.4, y = 100), 
#             label = paste("RMSE = ", round(rmse, 2)), 
#             size = 4, color = "Black") +
#   
#   # Añadir texto para Bias
#   geom_text(aes(x = 6.6, y = 90), 
#             label = paste("Bias = ", round(bias, 2)), 
#             size = 4, color = "Black") +
#   
#   # Aumentar el tamaño de los ticks en los ejes X e Y
#   theme(
#     axis.text.x = element_text(size = 12),  # Tamaño de los ticks en el eje X
#     axis.text.y = element_text(size = 12)   # Tamaño de los ticks en el eje Y
#   )
# 
# plot
# ggsave("D:/Josefina/Proyectos/ProyectoChile/plots/SeriesTemporales/Salida_03-XGB_cv_M1-041024_regresion_Tot2024.png", plot = plot, width = 10, height = 6, dpi = 300)
# 
# data_pm$Error_RF.CV.M1 <- data_pm$valor_sinca - data_pm$RF.CV.M1
# 
# # Graficar
# ggplot(data_pm, aes(x = RF.CV.M1, y = Error_RF.CV.M1)) +
#   geom_point(color = "#99d8c9", alpha = 0.7, size = 2) +
#   geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
#   labs(x = "Valor Predicho", y = "Error", title = "Error de Predicci?n") +
#   theme_classic()
