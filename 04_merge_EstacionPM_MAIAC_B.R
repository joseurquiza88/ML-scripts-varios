# MERGE PERO A PARTIR DE QUE DESCARGAMOS LOS DATOS DIARIOS DE INCA
#Y NO HORARIOS COMO EL OTRO SCRIPT

estacion <- "QUI"
num_estacion <- "08"
ventana <- "1250"
ventana <- "1000"
for (i in 1:1){
  # datos estacion PM
  data_PM <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/estaciones_media_dia_V02/",num_estacion,"-",estacion,"_PM25.csv",sep=""))
  data_PM$date  <- strptime(data_PM$fecha, format = "%Y-%m-%d")
  data_PM$date <- as.Date(data_PM$date)
  
  # datos MAIAC
  data_sat <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/maiac_media_dia/",ventana,"/",num_estacion,"-",ventana,"-",estacion,"_dailyMean.csv",sep=""))
  data_sat$date  <- strptime(data_sat$date, format = "%Y-%m-%d")
  data_sat$date <- as.Date(data_sat$date)
  
  #Merge
  
  merge_tot <- merge(x = data_sat, y = data_PM, by = "date") # Equivalente
  merge_tot_subt<- data.frame(date= merge_tot$date,
                              AOD_470 = merge_tot$AOD_047,
                              AOD_550 = merge_tot$AOD_055,
                              PM25 =merge_tot$valor)
  # Datos pm - maiac completo
  merge_tot_subt_complete <- merge_tot_subt[complete.cases(merge_tot_subt),]
  #Guardamos
  name <- paste("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/merge_pm_maiac/diario_V02/",ventana,"/",num_estacion,"-",estacion,"_merge_PM25-MAIAC_dailyMean.csv",sep="")
  print(length(merge_tot_subt_complete$AOD_550))
  write.csv(merge_tot_subt_complete,name)
}

name <- read.csv("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/merge_pm_maiac/diario_V02/1000/09-TOT_merge_PM25-MAIAC_dailyMean.csv")
name$date  <- strptime(name$date, format = "%d/%m/%Y")
write.csv(name,"D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/merge_pm_maiac/diario_V02/1000/09-TOT_merge_PM25-MAIAC_dailyMean.csv")

