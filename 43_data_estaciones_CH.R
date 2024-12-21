
## DATA SINCA CH
estacion <- "CH"

data_dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/dataset/estaciones/diarios",sep="")
setwd(data_dir)
lista_estaciones <- list.files(pattern = "*.csv")
j<-12
df_rbind <- data.frame()
for (j in 1:length(lista_estaciones)){
  print(j)
  data <-read.csv(lista_estaciones[j], colClasses = c("character","character", "numeric",
                                           "numeric","numeric","character",
                                           "numeric","numeric"))
  print(nrow(data))
  df_rbind <- rbind(df_rbind,data)
  
  
}
df_rbind<-df_rbind[!is.na(df_rbind$ID),]
df_rbind$date  <- strptime(df_rbind$FECHA..YYMMDD., format = "%y%m%d")

df_rbind_subt <- data.frame( fecha = df_rbind$FECHA..YYMMDD.,date=df_rbind$date, ID= df_rbind$ID,estacion = df_rbind$estacion,
                             Registros.validados=df_rbind$Registros.validados, Registros.preliminares=df_rbind$Registros.preliminares,
                             Registros.no.validados= df_rbind$Registros.no.validados,
                             Registros.completos=df_rbind$Registros.completos)
write.csv(df_rbind_subt,paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/proceed/PM25/CH_PM25.csv",sep=""))

          