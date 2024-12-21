
rm(list=ls())
#Objetivo unir todos dataset, hacer un merge por dia
# Directorio
setwd("D:/Josefina/Proyectos/ProyectoChile/SP/proceed/")

# Generar la secuencia de fechas desde el 01-01-2015 hasta el 31-07-2024
date <- seq.Date(from = as.Date("2015-01-01"), to = as.Date("2024-10-31"), by = "day")

# Crear el dataframe con la columna 'date'
df_date <- data.frame(date = date)
df_date$date <- strptime(df_date$date, format = "%Y-%m-%d")
#unique(data_pm$estacion)

df_date_1 <- df_date
df_date_1$ID <- 1
df_date_2 <- df_date
df_date_2$ID <- 2
df_date_3 <- df_date
df_date_3$ID <- 3
df_date_4 <- df_date
df_date_4$ID <- 4
df_date_5 <- df_date
df_date_5$ID <- 5
df_date_6 <- df_date
df_date_6$ID <- 6
df_date_7 <- df_date
df_date_7$ID <- 7
df_date_8 <- df_date
df_date_8$ID <- 8
df_date_9 <- df_date
df_date_9$ID <- 9
df_date_10 <- df_date
df_date_10$ID <- 10
df_date_11 <- df_date
df_date_11$ID <- 11
df_date_12 <- df_date
df_date_12$ID <- 12
df_date_13 <- df_date
df_date_13$ID <- 13
df_date_14 <- df_date
df_date_14$ID <- 14
df_date_15<- df_date
df_date_15$ID <- 15
df_date_16 <- df_date
df_date_16$ID <- 16

df_date_17 <- df_date
df_date_17$ID <- 17
df_date_18<- df_date
df_date_18$ID <- 18
df_date_19 <- df_date
df_date_19$ID <- 19
df_date_20 <- df_date
df_date_20$ID <- 20
df_date_21 <- df_date
df_date_21$ID <- 21

df_date_22 <- df_date
df_date_22$ID <- 22

df_date_23 <- df_date
df_date_23$ID <- 23

df_date_24 <- df_date
df_date_24$ID <- 24

df_date_rbind <- rbind (df_date_1,df_date_2,df_date_3,df_date_4,df_date_5,df_date_6,
                        df_date_7, df_date_8,df_date_9, df_date_10,df_date_11, df_date_12,
                        df_date_13, df_date_14,df_date_15,df_date_16,df_date_17,df_date_18,
                        df_date_19,df_date_20,df_date_21,df_date_22,df_date_23,df_date_24)


for (i in 1:1){
  ##################
  #01 Estaciones PM 25
  data_pm <- read.csv("./06_estaciones/SP_estaciones.csv")
  data_pm <- data.frame(date=data_pm$date, estacion=data_pm$estacion, ID = data_pm$ID, PM25 = data_pm$mean)
  data_pm$date  <- strptime(data_pm$date, format = "%Y-%m-%d")
  
  ##################
  ### --- 02 AOD
  data_aod <- read.csv(paste("./00_MAIAC/SP_MAIAC.csv",sep=""))
  data_aod <- data.frame(date=data_aod$date,estacion=data_aod$estacion, ID = data_aod$ID, AOD_055=data_aod$aod_550)
  data_aod$date  <- strptime(data_aod$date, format = "%Y%j")
  
  ##################
  ### --- 03 NDVI
  data_NDVI<- read.csv("./01_NDVI/SP_NDVI.csv")
  data_NDVI <- data.frame(date=data_NDVI$date,estacion=data_NDVI$estacion, ID = data_NDVI$ID, ndvi=data_NDVI$ndvi)
  data_NDVI$date  <- strptime(data_NDVI$date, format = "%Y%j")
  names(data_NDVI)
  

  # Función para expandir los valores mensuales a todos los días del mes por estación
  expandir_mensual <- function(df_mensual) {
    do.call("rbind", lapply(1:nrow(df_mensual), function(i) {
      start_date <- df_mensual$date[i]
      end_date <- seq(start_date, by = "month", length = 2)[2] - 1
      data.frame(
        date = seq(start_date, end_date, by = "day"),   # Expande a todos los días del mes
        ndvi = df_mensual$ndvi[i],                     # Valor NDVI correspondiente
        estacion = df_mensual$estacion[i],
        ID = df_mensual$ID[i]# Mantiene el nombre de la estación
      )
    }))
  }
  # Expandir el dataframe mensual
  data_NDVI_dia <- expandir_mensual(data_NDVI)
  
  ##################
  ### --- 04 LandCover 
  data_LandCover<- read.csv("./02_LandCover/SP_LandCover.csv")
  data_LandCover$date <- strptime(data_LandCover$year, format = "%Y")
  data_LandCover <- data_LandCover[data_LandCover$tile !="h14v11",]

  df_anual <- data_LandCover
  
  expandir_anual <- function(df_anual) {
    # Definir el rango de fechas diarias entre 2015-01-01 y 2024-10-31
    start_date <- as.Date("2015-01-01")
    end_date <- as.Date("2024-10-31")
    all_dates <- seq(start_date, end_date, by = "day")
    
    # Expansión por cada estación
    df_expandido <- do.call("rbind", lapply(split(df_anual, df_anual$estacion), function(df_estacion) {
      # Expandir para cada año disponible en la estación
      expanded_data <- do.call("rbind", lapply(unique(df_estacion$year), function(yr) {
        # Filtrar el valor de Type1 para el año específico
        annual_value <- df_estacion$Type1[df_estacion$year == yr]
        
        # Crear las fechas diarias solo para el año actual
        year_dates <- all_dates[format(all_dates, "%Y") == as.character(yr)]
        
        # Crear un dataframe con las fechas diarias para ese año, el valor anual, ID y estación
        data.frame(
          date = year_dates,
          landCover = rep(annual_value, length(year_dates)),  # Repetir el valor anual para cada día del año
          ID = df_estacion$ID[1],
          estacion = df_estacion$estacion[1]
        )
      }))
      
      return(expanded_data)
    }))
    
    return(df_expandido)
  }
  
  
  
  data_LandCover_dia <- expandir_anual(df_anual)
  data_LandCover_dia$date <- strptime(data_LandCover_dia$date, format = "%Y%j")
  
  #rownames(data_LandCover_dia) <- NULL
  
  # # Expandir el dataframe mensual
  # data_LandCover_dia <- expandir_anual(data_LandCover)
  # names(data_LandCover_dia) <- c("date", "LandCover")
  # data_LandCover_dia$date <- strptime(data_LandCover_dia$date, format = "%Y-%m-%d")
  
  ##################
  ### --- 05 DEM
  data_DEM<- read.csv("./03_DEM/SP_DEM.csv")
  # data_DEM_estacion <- data_DEM[data_DEM$Estacion == estacion,][[2]]
  
  df_anual_dem<- data_DEM
  
  expandir_anual_dem <- function(df_anual) {
    # Definir el rango de fechas diarias entre 2015-01-01 y 2024-10-31
    start_date <- as.Date("2015-01-01")
    end_date <- as.Date("2024-10-31")
    all_dates <- seq(start_date, end_date, by = "day")
    
    # Expansión por cada estación
    df_expandido <- do.call("rbind", lapply(split(df_anual, df_anual$ID), function(df_estacion) {
      # Obtener el valor de la estación actual (único, ya que es constante)
      valor_estacion <- unique(df_estacion$valor)
      
      # Crear un dataframe con las fechas diarias para el rango completo, el valor constante, y el ID
      data.frame(
        date = all_dates,
        valor = rep(valor_estacion, length(all_dates)),  # Repetir el valor anual para cada día del rango
        ID = df_estacion$ID[1]  # Mantener el ID de la estación
      )
    }))
    
    return(df_expandido)
  }
  
  dem_expand <- expandir_anual_dem(df_anual_dem)
  #dem_exapended_1 <- dem_exapended[dem_exapended$ID==2,]
  
  dem_expand$date <- strptime(dem_expand$date, format = "%Y-%m-%d")
  names(dem_expand) <- c("date", "DEM", "ID")
  ##################
  ### --- 06 MERRA-2
  data_MERRA<- read.csv("./04_MERRA-2_Dia/SP_MERRA-2_Dia.csv")
  data_MERRA <- data.frame(date = data_MERRA$date, ID = data_MERRA$ID, BCSMASS = data_MERRA$BCSMASS, 
                          #DMSSMASS = data_MERRA$DMSSMASS, 
                          DUSMASS = data_MERRA$DUSMASS, 
                          DUSMASS25 = data_MERRA$DUSMASS25, OCSMASS = data_MERRA$OCSMASS,
                          SO2SMASS = data_MERRA$SO2SMASS,SO4SMASS = data_MERRA$SO4SMASS, 
                          SSSMASS = data_MERRA$SSSMASS,SSSMASS25= data_MERRA$SSSMASS25)
  
  data_MERRA$date <- strptime(data_MERRA$date, format = "%Y%m%d")
  
  
  ##################
  ### --- 07 ERA-2
  data_ERA <- read.csv(paste("./05_ERA5/SP_ERA5.csv",sep=""))
  #data_ERA2 <- subset(data_ERA, select = -c(X,estacion))
  data_ERA$date <- strptime(data_ERA$date, format = "%Y-%m-%d")
  


  
  ##############################

  # Lista de dataframes que quieres unir ####data_LandCover_dia
  dataframes <- list(df_date_rbind , data_pm, data_aod, data_NDVI_dia, data_MERRA, data_ERA,dem_expand)
  
  # Usar Reduce para hacer merge secuencial
  data_merged <- Reduce(function(x, y) merge(x, y, by = c("ID","date"), all.x = TRUE), dataframes)
  data_merged_subt <- data.frame(ID = data_merged$ID, date=data_merged$date, estacion = data_merged$estacion.y,
                                 PM25 = data_merged$PM25,AOD_055= data_merged$AOD_055, ndvi= data_merged$ndvi,
                                 
                                 #landCover= data_merged$landCover, 
                                 BCSMASS= data_merged$BCSMASS, DUSMASS =data_merged$DUSMASS,
                                 DUSMASS25=  data_merged$DUSMASS25, OCSMASS = data_merged$OCSMASS, SO2SMASS =data_merged$SO2SMASS,
                                 SO4SMASS = data_merged$SO4SMASS, SSSMASS= data_merged$SSSMASS,
                                 SSSMASS25 = data_merged$SSSMASS25,
                                 blh_mean = data_merged$blh_mean,blh_min = data_merged$blh_min,blh_max= data_merged$blh_max,
                                 blh_sd= data_merged$blh_sd, blh_mean_subt = data_merged$blh_mean_subt, 
                                 
                                 sp_mean = data_merged$sp_mean, sp_min = data_merged$sp_min,
                                 sp_max =data_merged$sp_max, sp_sd = data_merged$sp_sd, sp_mean_subt=data_merged$sp_mean_subt,
                                 
                                 
                                 d2m_mean = data_merged$d2m_mean, d2m_min=data_merged$d2m_min,
                                 d2m_max= data_merged$d2m_max, d2m_sd = data_merged$d2m_sd, d2m_mean_subt =data_merged$d2m_mean_subt,
                                 
                                 t2m_mean=data_merged$t2m_mean,t2m_min = data_merged$t2m_min,
                                 t2m_max= data_merged$t2m_max, t2m_sd = data_merged$t2m_sd, t2m_mean_subt = data_merged$t2m_mean_subt,
                                 
                                 
                                 v10_mean= data_merged$v10_mean, v10_min= data_merged$v10_min,
                                 v10_max = data_merged$v10_max, v10_sd= data_merged$v10_sd, v10_mean_subt= data_merged$v10_mean_subt,
                                 
                                 u10_mean=  data_merged$u10_mean, u10_min =data_merged$u10_min,
                                 u10_max= data_merged$u10_max, data_merged$u10_sd, data_merged$u10_mean_subt,
                                 
                                 tp_mean = data_merged$tp_mean, tp_min= data_merged$tp_min,
                                 tp_max = data_merged$tp_max, tp_sd = data_merged$tp_sd, tp_mean_subt = data_merged$tp_mean_subt,
                                 DEM = data_merged$DEM
                                 
                       
                                 )

}
###############################################################
View(data_merged)
# Guardar archivo
# getwd()
data_merged_subt
name <- paste("./merge_tot/SP_merge_tot.csv",sep="")
write.csv(data_merged_subt,"./merge_tot/SP_merge_tot.csv")
data_merged_subt_complete <- data_merged_subt[complete.cases(data_merged_subt),]
write.csv(data_merged_subt_complete,"./merge_tot/SP_merge_comp.csv")


