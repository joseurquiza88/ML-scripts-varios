 
data_horaria <- read.csv("D:/Josefina/Proyectos/ProyectoChile/dataset/estaciones/QUI.csv")
#data_horaria <- read.csv("D:/Josefina/Proyectos/ProyectoChile/dataset/estaciones/OHG.csv")

for(i in 1:length(data_horaria$HORA..HHMM.)){
  print(i)
  if(data_horaria$HORA..HHMM.[i]== 0){
    data_horaria$hora[i] <- "0000"
  } 
  else if( data_horaria$HORA..HHMM.[i]>0  & data_horaria$HORA..HHMM.[i]<900){
    data_horaria$hora[i] <- paste("0",data_horaria$HORA..HHMM.[i],sep="")
  }
  else if(data_horaria$HORA..HHMM.[i]>=1000){
    data_horaria$hora[i] <- data_horaria$HORA..HHMM.[i]
  }
  
}

for(i in 1:length(data_horaria$FECHA..YYMMDD.)){
  print(i)
  if(data_horaria$FECHA..YYMMDD.[i]<= 999){
    data_horaria$fecha[i] <- paste("000",data_horaria$FECHA..YYMMDD.[i],sep="")
  } 
  else if(data_horaria$FECHA..YYMMDD.[i]>999  & data_horaria$FECHA..YYMMDD.[i]<=9999){
    
    data_horaria$fecha[i] <- paste("00",data_horaria$FECHA..YYMMDD.[i],sep="")
  }
  else if(data_horaria$FECHA..YYMMDD.[i]>9999  & data_horaria$FECHA..YYMMDD.[i]<=99999){
    
    data_horaria$fecha[i] <- paste("0",data_horaria$FECHA..YYMMDD.[i],sep="")
  } 
  else if(data_horaria$FECHA..YYMMDD.[i]>99999){
    data_horaria$fecha[i] <- data_horaria$FECHA..YYMMDD.[i]
  }
}


data_horaria$site <- "QUI"
data_horaria$fecha_convertida <- paste(data_horaria$fecha,data_horaria$hora,sep=" ")
data_horaria$fecha_convertida <- strptime(data_horaria$fecha_convertida, "%y%m%d %H%M")#, format = "%Y-%m-%d %H:%M")
data_horaria$year <- year(data_horaria$fecha_convertida)
data_horaria_sub <- data.frame(fecha = data_horaria$fecha_convertida,
                             valor = data_horaria$Registros.validados,
                             estacion =data_horaria$site,
                             year = data_horaria$year,
                             variable = "PM25")


write.csv(data_horaria_sub,"D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/estaciones/08_QUI-PM25.csv")
##################################################################################
##################################################################################
#METEREOLOGIA
data_meteo <- read.csv("D:/Josefina/Proyectos/ProyectoChile/dataset/meteo/humedad/humedad_BSQ.csv")
#data_meteo <- read.csv("D:/Josefina/Proyectos/ProyectoChile/dataset/meteo/temperatura/temperatura_BSQ.csv")

for(i in 1:length(data_meteo$HORA..HHMM.)){
  print(i)
  if(data_meteo$HORA..HHMM.[i]== 0){
    data_meteo$hora[i] <- "0000"
  } 
  else if( data_meteo$HORA..HHMM.[i]>0  & data_meteo$HORA..HHMM.[i]<900){
    data_meteo$hora[i] <- paste("0",data_meteo$HORA..HHMM.[i],sep="")
  }
  else if(data_meteo$HORA..HHMM.[i]>=1000){
    data_meteo$hora[i] <- data_meteo$HORA..HHMM.[i]
  }
  
}

for(i in 1:length(data_meteo$FECHA..YYMMDD.)){
  print(i)
  if(data_meteo$FECHA..YYMMDD.[i]< 100101){
    data_meteo$fecha[i] <- paste("0",data_meteo$FECHA..YYMMDD.[i],sep="")
  } 
  else if(data_meteo$FECHA..YYMMDD.[i]>=100101){
    data_meteo$fecha[i] <- data_meteo$FECHA..YYMMDD.[i]
  }
}

data_meteo$fecha_convertida <- paste(data_meteo$fecha,data_meteo$hora,sep=" ")
data_meteo$fecha_convertida <- strptime(data_meteo$fecha_convertida, "%y%m%d %H%M")#, format = "%Y-%m-%d %H:%M")
data_meteo$year <- year(data_meteo$fecha_convertida)
data_meteo_sub <- data.frame(fecha = data_meteo$fecha_convertida,
                               valor = data_meteo$VAR,
                               estacion = "BSQ",
                               year = data_meteo$year,
                               variable = "TEMP")

write.csv(data_meteo_sub,"D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/meteo/humedad/01_BSQ-HR.csv")
write.csv(data_meteo_sub,"D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/meteo/temperatura/01_BSQ-TMP.csv")

####################################################################
####################################################################
####################################################################
#MEDIAS DIARIAS ESTACIONES PM
estacion <- "QUI"
num_estacion <- "08"

data_horaria <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/estaciones/",num_estacion,"_",estacion,"-PM25.csv",sep=""))
data_horaria$date  <- strptime(data_horaria$fecha, format = "%Y-%m-%d %H:%M:%S")
data_horaria$date <- as.Date(data_horaria$date)
data_horaria %>%
  group_by(date) %>%  
  group_split() ->combinate_data
rbind_combinate <- data.frame()
for (i in 1:length(combinate_data)){
  df <- data.frame( date = combinate_data[[i]][["date"]][1],
                    pm25= mean(combinate_data[[i]][["valor"]],na.rm=T))               
  
  rbind_combinate <- rbind(rbind_combinate,df)
}
name <- paste("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/estaciones_media_dia/",num_estacion,"-",estacion,"_PM25.csv",sep="")
write.csv(rbind_combinate,name)
}