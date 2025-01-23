

library(raster)

# Definir el directorio donde están tus archivos raster
rm(list = ls())
#dir_salida <- "D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/Prediccion_01-2024/Salida/Salida_02-XGB_cv_M4-300924/"
month <- c("01","02","03","04","05","06","07","08","09","10","11","12")

i<-1
# modelo <- "02-RF_cv_M1-261224_MX"
modelo <- "03-XGB_cv_M1-041024_CH"
estacion <- "CH"
for (i in 1:length(month)){
  print(i)
  #dir_salida <- paste("D:/Josefina/Proyectos/ProyectoChile/modelos/Salidas/SalidasDiarias/Salida_03-XGB_cv_M1-041024/",year,"/",month[i],sep="")
  dir_salida <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/salidas/SalidasDiarias/",modelo,"/",month[i],sep="")
  setwd(dir_salida)
  
  # Lista de archivos raster (ajusta la extensión según sea necesario)
  lista_raster <- list.files(pattern = "*.tif") # Cambia la extensión si es necesario
  lista_raster_recorte <- lista_raster
  #lista_raster_recorte
  len <- length(lista_raster_recorte)
  print(c(month[i],len))
  # Cargar los rasters en un RasterStack
  raster_stack <- stack(lista_raster_recorte)
  
  # Calcular el promedio mensual y desviacion estandar
  promedio_mensual <- calc(raster_stack, fun = mean, na.rm = TRUE)

  modelo_2 <- substr(lista_raster[1],14,34)
  # Guardar el resultado en un nuevo archivo raster
  #dir_salida <- "D:/Josefina/Proyectos/ProyectoChile/modelos/Salidas/SalidasMensuales/Salida_03-XGB_cv_M1-041024/"
  #dir_salida <- "D:/Josefina/Proyectos/ProyectoChile/modelos/Salidas/SalidasMensuales/Salida_03-XGB_cv_M1-041024/Coef_Var/"
  dir_salida_tiff <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/salidas/SalidasMensualAnual/",modelo,"/",sep="")
  
  writeRaster(promedio_mensual, filename = paste(dir_salida_tiff,"mensual_",month[i],"-",modelo_2,".tif",sep=""), format = "GTiff", overwrite = TRUE)
}
