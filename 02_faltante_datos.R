# corroborar que esten todos lo dias y todos los años

estacion <- "QUI"
#01 Leemos archivo csv
data <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/dataset/MAIAC/",estacion,"/prueba_1250_C61_",estacion,".csv",sep=""))
data$date_format  <- strptime(data$date, format = "%Y%j")


for (i in 1:1){
  #data <- data[complete.cases(data$AOD), ]
  
  # Me quedo solo con la columna dia y genero un dataframe
  fecha_sat <- data.frame(fecha=data$date_format, num=1)
  
  #Elimino los nans, y solo me quedo con los que si tienen fecha
  fecha_sat <- fecha_sat[complete.cases(fecha_sat$fecha), ]
  
  
  #Convierto el dato en tipo date
  fecha_sat$fecha <- strptime(fecha_sat$fecha, format="%Y-%m-%d") 
  fecha_sat%>%
    dplyr::group_by(fecha) %>%  
    dplyr::group_split() -> dat_agrupado
  df_rbind <- data.frame()
  for (x in 1:length(dat_agrupado)){
    df <- data.frame(fecha = dat_agrupado[[x]][["fecha"]][1],
                     num = dat_agrupado[[x]][["num"]][1] )
    df_rbind <- rbind(df_rbind,df)
  }
  fecha_sat<-df_rbind 
  fecha_sat$fecha <- strptime(fecha_sat$fecha, format="%Y-%m-%d") 
  
  # Crear vector de fechas desde 01-12-2015 al 31-12-2022
  fecha_total <- data.frame(fechas=seq(as.Date("2015-01-01"), as.Date("2024-05-31"), by="day"))
  #Convierto el dato en tipo date
  fecha_total$fecha <- strptime(fecha_total$fechas, format="%Y-%m-%d") 
  #Uno los dos dataframe por la columna fecha, 
  #En los dos df se tienen que llamar igual
  df_completo <- merge(fecha_total,fecha_sat,  by = "fecha", all.x = TRUE)  # O usando dplyr::left_join(df_fechas, df_original, by = "fecha")
  #Modifico los nombres de las columnas para mejor entendimiento
  names(df_completo) <- c("fecha_total","fecha_sat","fecha_existente")
  #Me quedo con los que tienen valor nulo en la columa num que corresponde a los
  #datos faltante
  fechas_faltantes <- df_completo[is.na(df_completo$fecha_existente), ]
  
}

