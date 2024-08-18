# El objetivo es generar medias diarias de cada una de las estacioenes
# Para luego hacer el merge con las estaciones de PM

#01 generamos el codigo paso por paso
estacion <- "FLD"
ventana <- "1000"
daily_mean <- function(estacion,ventana){
  #01 Leemos archivo csv
  data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/dataset/MAIAC/",estacion,"/prueba_",ventana,"_C61_",estacion,".csv",sep=""))
  data$date_format  <- strptime(data$date, format = "%Y%j")
  data %>%
    group_by(date_format) %>%  
    group_split() ->combinate_data
  rbind_combinate <- data.frame()
  for (i in 1:length(combinate_data)){
    df <- data.frame( date = combinate_data[[i]][["date_format"]][1],
                      AOD_047 = mean(combinate_data[[i]][["AOD_047"]],na.rm=T),
                      AOD_055 = mean(combinate_data[[i]][["AOD_055"]],na.rm=T))               
    
  rbind_combinate <- rbind(rbind_combinate,df)
  }
  name <- paste("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/maiac_media_dia/",ventana,"/",num_estacion,"-",ventana,"-",estacion,"_dailyMean.csv",sep="")
  write.csv(rbind_combinate,name)
}
num_estacion <- "08"
estacion <- "QUI"
ventana <- "1000"
ventana <- "1250"
daily_mean(estacion,ventana)
