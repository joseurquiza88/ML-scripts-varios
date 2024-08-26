
rm(list=ls())
#Objetivo unir todos dataset, hacer un merge por dia
# Directorio
setwd("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/")

# Generar la secuencia de fechas desde el 01-01-2015 hasta el 31-07-2024
date <- seq.Date(from = as.Date("2015-01-01"), to = as.Date("2024-07-31"), by = "day")

# Crear el dataframe con la columna 'date'
df_date <- data.frame(date = date)
df_date$date <- strptime(df_date$date, format = "%Y-%m-%d")
estacion <- "PTA"
num_estacion <- "06"
##################
#01 Estaciones PM 25
data_pm <- read.csv(paste("./estaciones_media_dia_V02/",num_estacion,"-",estacion,"_PM25.csv",sep=""))
data_pm <- data.frame(date=data_pm$fecha, estacion=data_pm$estacion, PM25 = data_pm$valor)
data_pm$date  <- strptime(data_pm$date, format = "%Y-%m-%d")

##################
### --- 02 AOD
data_aod <- read.csv(paste("./maiac_media_dia/1000/",num_estacion,"-1000-",estacion,"_dailyMean.csv",sep=""))
data_aod <- data.frame(date=data_aod$date, AOD_055=data_aod$AOD_055)
data_aod$date  <- strptime(data_aod$date, format = "%Y-%m-%d")

##################
### --- 03 NDVI
data_NDVI<- read.csv(paste("./NDVI/",num_estacion,"_",estacion,"_NDVI.csv",sep=""))
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
# Expandir el dataframe mensual
data_NDVI_dia <- expandir_mensual(data_NDVI)
data_NDVI_dia$date<- strptime(data_NDVI_dia$date, format = "%Y-%m-%d")
names(data_NDVI_dia) <- c("date", "ndvi")

##################
### --- 04 LandCover 
data_LandCover<- read.csv(paste("./LandCover/",num_estacion,"_",estacion,"_LandCover.csv",sep=""))
data_LandCover$date <- strptime(data_LandCover$year, format = "%Y")

expandir_anual <- function(df_anual) {
  start_date <- as.Date("2015-01-01")
  end_date <- as.Date("2024-08-26")
  all_dates <- seq(start_date, end_date, by = "day")
  
  # Calcular el número de días totales
  total_days <- length(all_dates)
  
  # Ajustar el número de repeticiones para que coincida con la cantidad total de días
  repeat_count <- ceiling(total_days / nrow(df_anual))
  expanded_values <- rep(df_anual$LandCover_IGBP, each = repeat_count)
  
  # Asegurarse de que los vectores tengan la misma longitud
  expanded_values <- expanded_values[1:total_days]
  
  # Crear un dataframe con la secuencia de fechas y los valores expandidos
  data.frame(date = all_dates, value = expanded_values)
}

# Expandir el dataframe mensual
data_LandCover_dia <- expandir_anual(data_LandCover)
names(data_LandCover_dia) <- c("date", "LandCover")
data_LandCover_dia$date <- strptime(data_LandCover_dia$date, format = "%Y-%m-%d")

##################
### --- 05 DEM
data_DEM<- read.csv("./DEM/elevacion_DEM.csv")
data_DEM_estacion <- data_DEM[data_DEM$Estacion == estacion,][[2]]

##################
### --- 06 MERRA-2
data_MERRA<- read.csv(paste("./MERRA-2/",num_estacion,"_",estacion, "_MERRA.csv",sep=""))
data_MERRA <- data.frame(date = data_MERRA$date, BCSMASS = data_MERRA$BCSMASS, 
                        DMSSMASS = data_MERRA$DMSSMASS, DUSMASS = data_MERRA$DUSMASS, 
                        DUSMASS25 = data_MERRA$DUSMASS25, OCSMASS = data_MERRA$OCSMASS,
                        SO2SMASS = data_MERRA$SO2SMASS,SO4SMASS = data_MERRA$SO4SMASS, 
                        SSSMASS = data_MERRA$SSSMASS,SSSMASS25= data_MERRA$SSSMASS25)

data_MERRA$date <- strptime(data_MERRA$date, format = "%Y%m%d")


##################
### --- 07 ERA-2
data_ERA <- read.csv(paste("./ERA/",num_estacion,"_",estacion, "_ERA.csv",sep=""))
data_ERA <- subset(data_ERA, select = -c(X,estacion))
data_ERA$date <- strptime(data_ERA$date, format = "%Y-%m-%d")

##############################

# Lista de dataframes que quieres unir
dataframes <- list(df_date, data_pm, data_aod, data_NDVI_dia, data_LandCover_dia, data_MERRA, data_ERA)

# Usar Reduce para hacer merge secuencial
data_merged <- Reduce(function(x, y) merge(x, y, by = "date", all.x = TRUE), dataframes)

# Añadir la columna DEM
data_merged$DEM <- data_DEM_estacion

data_merged$estacion <- estacion

###############################################################
# Guardar archivo
getwd()
name <- paste("./merge_tot/",num_estacion,"_",estacion,"_merge_tot.csv",sep="")
write.csv(data_merged,name)
