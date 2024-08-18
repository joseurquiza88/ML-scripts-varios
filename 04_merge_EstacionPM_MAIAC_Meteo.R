# MERGE DE DATOS METEO con PM y AOD
rm(list=ls())
estacion <- "CDE"
num_estacion <- "07"
ventana <- "1250"
# ventana <- "1000"
for (i in 1:1){
  # datos estacion PM - AOD

  data_PM <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/merge_pm_maiac/diario_V02/",ventana,"/",num_estacion,"-",estacion,"_merge_PM25-MAIAC_dailyMean.csv",sep=""))
  data_PM$date  <- strptime(data_PM$date, format = "%Y-%m-%d")
  data_PM$date <- as.Date(data_PM$date)
  
  # datos METEO temp
  data_temperatura <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/meteo/temperatura/",num_estacion,"-",estacion,"_temperatura.csv",sep=""))
  data_temperatura$date  <- strptime(data_temperatura$fecha, format = "%Y-%m-%d")
  data_temperatura$date <- as.Date(data_temperatura$date)
  
  # datos METEO humedad
  data_humedad <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/meteo/humedad/",num_estacion,"-",estacion,"_humedad.csv",sep=""))
  data_humedad$date  <- strptime(data_humedad$fecha, format = "%Y-%m-%d")
  data_humedad$date <- as.Date(data_humedad$date)
  
  # datos METEO DIR VIENTO
  data_dirViento <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/meteo/dirViento/",num_estacion,"-",estacion,"_dirViento.csv",sep=""))
  data_dirViento$date  <- strptime(data_dirViento$fecha, format = "%Y-%m-%d")
  data_dirViento$date <- as.Date(data_dirViento$date)
  
  
  # datos METEO DIR VIENTO
  data_velViento <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/meteo/velViento/",num_estacion,"-",estacion,"_velViento.csv",sep=""))
  data_velViento$date  <- strptime(data_velViento$fecha, format = "%Y-%m-%d")
  data_velViento$date <- as.Date(data_velViento$date)
  #Merge
  
  merge_tot <- merge(x = data_PM, y = data_temperatura, by = "date") # Equivalente
  merge_tot_2 <- merge(x = merge_tot, y = data_humedad, by = "date") # Equivalente
  names(merge_tot_2) <-  c("date", "X","AOD_470","AOD_550","PM25","y", "fecha",
                           "temperatura", "estacion.x", "year.x", "variable.x", "z",
                           "fecha.z", "humedad","estacion.z", "year.z","variable.z")
  merge_tot_2<- data.frame(date = merge_tot_2$date,AOD_470 = merge_tot_2$AOD_470,
                            AOD_550 = merge_tot_2$AOD_550, PM25=  merge_tot_2$PM25, 
                            temperatura = merge_tot_2$temperatura,humedad= merge_tot_2$humedad, 
                            estacion = merge_tot_2$estacion.x,  year=merge_tot_2$year.x)
  merge_tot_3 <- merge(x = merge_tot_2, y = data_dirViento, by = "date") # Equivalente

  merge_tot_3<- data.frame(date = merge_tot_3$date,AOD_470 = merge_tot_3$AOD_470,
                           AOD_550 = merge_tot_3$AOD_550, PM25=  merge_tot_3$PM25, 
                           temperatura = merge_tot_3$temperatura,humedad= merge_tot_3$humedad, 
                           dirViento = merge_tot_3$valor, estacion = merge_tot_3$estacion.x,  year=merge_tot_3$year.x)
  
  merge_tot_4 <- merge(x = merge_tot_3, y = data_velViento, by = "date") # Equivalente
  merge_tot_subt<- data.frame(date = merge_tot_4$date,AOD_470 = merge_tot_4$AOD_470,
                           AOD_550 = merge_tot_4$AOD_550, PM25=  merge_tot_4$PM25, 
                           temperatura = merge_tot_4$temperatura,humedad= merge_tot_4$humedad, 
                           dirViento = merge_tot_4$valor,velViento = merge_tot_4$valor,
                           estacion = merge_tot_4$estacion.x,  year=merge_tot_4$year.x)
  

  # Datos pm - maiac completo
  merge_tot_subt_complete <- merge_tot_subt[complete.cases(merge_tot_subt),]
  #Guardamos
  name <- paste("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/merge_pm-maiac-meteo/",ventana,"/",num_estacion,"-",estacion,"_merge_PM25-MAIAC-Meteo.csv",sep="")
  print(length(merge_tot_subt_complete$AOD_550))
  write.csv(merge_tot_subt_complete,name)
}
View(merge_tot_subt_complete)
name <- read.csv("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/merge_pm_maiac/diario_V02/1000/09-TOT_merge_PM25-MAIAC_dailyMean.csv")
name$date  <- strptime(name$date, format = "%d/%m/%Y")
write.csv(name,"D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/merge_pm_maiac/diario_V02/1000/09-TOT_merge_PM25-MAIAC_dailyMean.csv")

