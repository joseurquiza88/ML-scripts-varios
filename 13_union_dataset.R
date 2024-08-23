

#Objetivo unir todos dataset, hacer un merge por dia
# Directorio
setwd("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/")

# Generar la secuencia de fechas desde el 01-01-2015 hasta el 31-07-2024
date <- seq.Date(from = as.Date("2015-01-01"), to = as.Date("2024-07-31"), by = "day")

# Crear el dataframe con la columna 'date'
df_date <- data.frame(date = date)
df_date$date <- strptime(df_date$date, format = "%Y-%m-%d")
#01 Estaciones PM 25
data_pm <- read.csv("./estaciones_media_dia_V02/01-BSQ_PM25.csv")
data_pm$date  <- strptime(data_pm$fecha, format = "%Y-%m-%d")

data_pm <- data.frame(date=data_pm$date, estacion=data_pm$estacion, PM25 = data_pm$valor)
data_pm$date  <- strptime(data_pm$date, format = "%Y-%m-%d")
#02 AOD
data_aod <- read.csv("./maiac_media_dia/1000/01-1000-BSQ_dailyMean.csv")
data_aod$date  <- strptime(data_aod$date, format = "%Y-%m-%d")
names(data_aod)
data_aod <- data.frame(date=data_aod$date, AOD_055=data_aod$AOD_055)
data_aod$date  <- strptime(data_aod$date, format = "%Y-%m-%d")

##################
#03 NDVI
data_NDVI<- read.csv("./NDVI/02_BSQ_NDVI.csv")
data_NDVI <- data.frame(date=data_NDVI$date, ndvi=data_NDVI$ndvi)
data_NDVI$date  <- strptime(data_NDVI$date, format = "%Y%j")
names(data_NDVI)

# Función para expandir los valores mensuales a todos los días del mes
expandir_mensual <- function(df_mensual) {
  do.call("rbind", lapply(1:nrow(df_mensual), function(i) {
    start_date <- df_mensual$date[i]
    end_date <- seq(start_date, by = "month", length = 2)[2] - 1
    data.frame(date = seq(start_date, end_date, by = "day"), value = df_mensual$ndvi[i])
  }))
}
df_mensual<-data_NDVI
# Expandir el dataframe mensual
data_NDVI_dia <- expandir_mensual(data_NDVI)
data_NDVI_dia$date<- strptime(data_NDVI_dia$date, format = "%Y-%m-%d")
names(data_NDVI_dia) <- c("date", "ndvi")

##################
#04 LandCover


##############################
#Merge
data_merged <- merge(df_date, data_pm, by = "date", all.x = TRUE)
data_merged <- merge(data_merged, data_aod, by = "date", all.x = TRUE)
data_merged <- merge(data_merged, data_NDVI_dia, by = "date", all.x = TRUE)

