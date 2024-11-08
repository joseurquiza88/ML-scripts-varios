

library(raster)

# Definir el directorio donde estÃ¡n tus archivos raster
rm(list = ls())
#dir_salida <- "D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/Prediccion_01-2024/Salida/Salida_02-XGB_cv_M4-300924/"
month <- c("01","02","03","04","05","06","07","08","09","10","11","12")
variable <- "BCSMASS"
variable <- "DMSSMASS"
variable <- "DUSMASS" # VERIFICAR
variable <- "DUSMASS25"
variable <- "OCSMASS"
variable <- "SO2SMASS"
variable <- "SO4SMASS"
variable <- "SSSMASS" ##3 NO SE HIZO
variable <- "SSSMASS25"

rm(list = ls())
#dir_salida <- "D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/Prediccion_01-2024/Salida/Salida_02-XGB_cv_M4-300924/"
month <- c("01","02","03","04","05","06","07","08","09","10","11","12")
variable <- "BLH"
variable <- "D2M"
variable <- "SP"
variable <- "T2M"
variable <- "TP"
variable <- "U10"
variable <- "V10"
year <- "2015"

for (i in 1:length(month)){
  print(i)
  #dir_salida <- "D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/Prediccion_2015/tiff/04_MERRA-2_Dia/"
  dir_salida <- "D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/Prediccion_2015/tiff/05_ERA5/"
  
  setwd(dir_salida)
  
  # Lista de archivos raster (ajusta la extensiÃ³n segÃºn sea necesario)
  #lista_raster <- list.files(pattern = "*.tif") # Cambia la extensiÃ³n si es necesario
  lista_raster <- list.files(pattern = paste(year,"-",month[i],".*?",variable,".*\\.tif$",sep=""))

  
  lista_raster_recorte <- lista_raster
  #lista_raster_recorte
  len <- length(lista_raster_recorte)
  print(c(month[i],len))
  # Cargar los rasters en un RasterStack
  raster_stack <- stack(lista_raster_recorte)
  
  # Calcular el promedio mensual y desviacion estandar
  promedio_mensual <- calc(raster_stack, fun = mean, na.rm = TRUE)
  #sd_mensual <- calc(raster_stack, fun = sd, na.rm = TRUE)
  # Calcular el coeficiente de variacion
  #coef_Var <- (sd_mensual / promedio_mensual) * 100
  #plot(promedio_mensual)
  #modelo <- substr(lista_raster[1],15,33)
  # Guardar el resultado en un nuevo archivo raster
  #dir_salida <- "D:/Josefina/Proyectos/ProyectoChile/modelos/Salidas/SalidasMensuales/Salida_03-XGB_cv_M1-041024/"
  #dir_salida <- "D:/Josefina/Proyectos/ProyectoChile/modelos/Salidas/SalidasMensuales/MERRA-2_Dia/"
  dir_salida <- "D:/Josefina/Proyectos/ProyectoChile/modelos/Salidas/SalidasMensuales/ERA5/"
  
  writeRaster(promedio_mensual, filename = paste(dir_salida,variable,"_mensual_",month[i],"-",year,".tif",sep=""), format = "GTiff", overwrite = TRUE)
}
