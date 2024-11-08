#MEDIAS DIARIAS ESTACIONES PM
# PERO NO A PARTIR DE DATOS HORARIOS SINO LA DSCARGA FE SINCA SE HIZO DE DATOS 
#DIARIOAS DIRECTAMENTE
estacion <- "QUI"
num_estacion <- "08"

data<- (paste("D:/Josefina/Proyectos/ProyectoChile/dataset/estaciones/diarios/",estacion,"_diario.csv",sep=""))
datos <- read.csv(data, colClasses = c("character", "numeric", "numeric", "numeric", "numeric","character"))
datos$date  <- strptime(datos$FECHA..YYMMDD., format = "%y%m%d")
df <- data.frame(fecha = datos$date,
                               valor = datos$Registros.validados,
                               valor_completos = datos$Registros.completos,
                               estacion = datos$estacion,
                               year = year(datos$date),
                               variable = "PM25")             
  
View(df)
name <- paste("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/PM25/",num_estacion,"-",estacion,"_PM25.csv",sep="")
write.csv(df,name)

############################################################################################################
#Datos meteo
rm(list=ls())
estacion <- "CDE"
num_estacion <- "0E"
var <- "dirViento"
for (i in 1:1){
  data<- (paste("D:/Josefina/Proyectos/ProyectoChile/dataset/meteo/",var,"/",var,"_",estacion,".csv",sep=""))
  datos <- read.csv(data, colClasses = c("character","character", "numeric"))
  datos$date  <- strptime(datos$FECHA..YYMMDD., format = "%y%m%d")
  df <- data.frame(fecha = datos$date,
                   valor = datos$VAR,
                   estacion = estacion,
                   year = year(datos$date),
                   variable = var)             
  
  df %>%
    dplyr::group_by(fecha) %>%  
    dplyr::group_split() -> df_diario
  
  df_rbind <- data.frame()
  for (p in 1:length(df_diario)){
    
    data_diario <- data.frame(fecha = df_diario[[p]][["fecha"]][1],
                              valor = mean(df_diario[[p]][["valor"]],na.rm = T), 
                              estacion = df_diario[[p]][["estacion"]][1],
                              year =  df_diario[[p]][["year"]][1],
                              variable = df_diario[[p]][["variable"]][1])
    df_rbind <- rbind(df_rbind ,data_diario)
    
  }
  
  name <- paste("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/meteo/",var,"/",num_estacion,"-",estacion,"_",var,".csv",sep="")
  
  write.csv(df_rbind,name)
  
}
View(df_rbind)

