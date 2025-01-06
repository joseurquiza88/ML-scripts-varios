
direccion <- "D:/Josefina/Proyectos/ProyectoChile/MD/dataset/estaciones/"
estacion <- "ID 94 V02"
setwd(paste(direccion,estacion, sep =""))
id <- list.files(path = getwd(),
                 pattern = "*.csv",
                 full.names = FALSE)
i <-1
df_rbind <- data.frame()
nombres <- names(read.csv(id[1], na.strings = "-9999"))
for (i in 1:length(id)) {
    print(i)
    data <- read.csv(id[i], na.strings = -9999)
    data_names <- names(data)
    
  if (identical(nombres, data_names)){
      df_rbind <- rbind(df_rbind, data)
    } else {
      stop(paste("Error: Los nombres no coinciden en el archivo", id[i]))
    }
}

###### ----- CALIDAD DATOS -------
##3https://siata.gov.co/descarga_siata/index.php/info/aire/
# Valor Flag Calidad del dato
# 1 Dato v´alido
# -1 Dato v´alido por el operado anterior
# 1.8 – 2.5 Dato dudoso
# 2.6 – 3.9 Dato malo
# >= 4.0 Dato faltante
# Dato -9999 y calidad (flag) 1 Equipo fuera de operaci´on

df_rbind_calidad <- df_rbind[df_rbind$calidad_pm25<2.6,]
df_rbind_calidad <- df_rbind_calidad[df_rbind_calidad$pm25!= -9999,]
# df_rbind_calidad$dateH <- as.POSIXct( strptime (df_rbind_calidad$X, format = "%Y-%m-%d %H:%M:%S"))
df_rbind_calidad$dateH <- as.POSIXct( strptime (df_rbind_calidad$Fecha_Hora, format = "%Y-%m-%d %H:%M:%S"))
df_rbind_calidad$date <- as.Date(df_rbind_calidad$dateH)
names(df_rbind_calidad)
df <- df_rbind_calidad %>%
  group_by(date) %>%
  summarise( 
    # Primer valor de unidad
    mean = mean(pm25, na.rm = TRUE),               # Media
    min = min(pm25, na.rm = TRUE),                 # M?nimo
    max = max(pm25, na.rm = TRUE),                 # M?ximo
    sd = sd(pm25, na.rm = TRUE)
    # Desviaci?n est?ndar
    #,                         # Conteo de valores no NA
    #mean_subt = mean(data_subt$valor[data_subt$estacion == unique(estacion)], na.rm = TRUE) # Media para horas espec?ficas
  )
View(df)
df$ID <- df_rbind_calidad$codigoSerial[1]
names <- paste(direccion,"/mediasDiarias/mediaDiaria_",estacion,".csv",sep="")
write.csv(df,names)

##########################################################
##########################################################
direccion <- "D:/Josefina/Proyectos/ProyectoChile/MD/dataset/estaciones/mediasDiarias/"

setwd(direccion)
id <- list.files(path = getwd(),
                 pattern = "*.csv",
                 full.names = FALSE)
i <-1
df_rbind <- data.frame()

for (i in 1:length(id)) {
  print(i)
  data <- read.csv(id[i])
  data_names <- names(data)
  df_rbind <- rbind(df_rbind, data)

}
