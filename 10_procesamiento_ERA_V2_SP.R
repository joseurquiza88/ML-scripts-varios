########################################################################
#media diaria
#Manualmente los separamos despues del codigo 02_procesamiento
rm(list=ls())
# dire <- "DJosefina/Proyectos/ProyectoChile/dataset/meteoSatelital/2015/02-2015/proceed/v10"
tipo <- "blh"
year <- 2015
dire <- paste("D:/Josefina/Proyectos/ProyectoChile/SP/proceed/05_ERA5/",year,"/",tipo,sep="")


#blh, d2m, sp, t2m, tp,v10, u10

setwd(dire)
id <- list.files(path = getwd(), pattern = "*.csv", full.names = FALSE)
df_rbind <- data.frame()
for (i in 1:length(id)){
  print(i)
  data <- read.csv(id[i])
  # Cargar el paquete dplyr
  library(dplyr)
  
  # Filtrar el data frame para las horas especificadas
  data_subt <- data %>%
    filter(hora %in% c("12:00:00", "13:00:00", "14:00:00", "15:00:00", "16:00:00", "17:00:00"))
  
  # Agrupar por la columna 'estacion' y calcular las estadísticas
  df <- data %>%
    group_by(estacion) %>%
    summarise(
      date = as.Date(fecha)[1],                       # Primer valor de fecha
      nombre_var = nombre_var[1],                     # Primer valor de nombre_var
      unidad = unidad[1],                             # Primer valor de unidad
      mean = mean(valor, na.rm = TRUE),               # Media
      min = min(valor, na.rm = TRUE),                 # Mínimo
      max = max(valor, na.rm = TRUE),                 # Máximo
      sd = sd(valor, na.rm = TRUE),                   # Desviación estándar
      n = sum(!is.na(valor)),                         # Conteo de valores no NA
      mean_subt = mean(data_subt$valor[data_subt$estacion == unique(estacion)], na.rm = TRUE) # Media para horas específicas
    )
  
  df_rbind <- rbind(df_rbind ,df) 
}

write.csv(df_rbind,paste("D:/Josefina/Proyectos/ProyectoChile/SP/proceed/05_ERA5/tot/",tipo,"_",year,"_dia.csv",sep=""))
######
#nimos dataset merge por dia
setwd("D:/Josefina/Proyectos/ProyectoChile/dataset/meteoSatelital/")
setwd("D:/Josefina/Proyectos/ProyectoChile/SP/proceed/05_ERA5/tot/")
rm(list=ls())
estacion <- "PTA"
num_estacion <- "07"


for (x in 1:1){
  data_blh <- read.csv("blh_dia.csv")
  data_blh <- data.frame( x = data_blh$X, date = data_blh$date, estacion = data_blh$estacion, nombre_var = data_blh$nombre_var,
                          unidad_blh = data_blh$unidad,
                          mean_blh = data_blh$mean, min_blh = data_blh$min, max_blh = data_blh$max,
                          sd_blh = data_blh$sd, n_blh = data_blh$n,mean_subt_blh= data_blh$mean_subt)
  # names(data_blh)<- c("X","date","nombre_var",paste("unidad_",data_blh$nombre_var[1],sep=""),"mean_blh",
  #                     "min_blh","max_blh","sd_blh","n", "mean_subt_blh")
  
  ##
  data_d2m <- read.csv("d2m_dia.csv")
  data_d2m <- data.frame( x = data_d2m$X, date = data_d2m$date, estacion = data_d2m$estacion,nombre_var = data_d2m$nombre_var,
                          unidad_d2m = data_d2m$unidad,
                          mean_d2m = data_d2m$mean, min_d2m = data_d2m$min, max_d2m = data_d2m$max,
                          sd_d2m = data_d2m$sd, n_d2m = data_d2m$n,mean_subt_d2m= data_d2m$mean_subt)
  
  ##
  data_sp <- read.csv("sp_dia.csv")
  data_sp <- data.frame( x = data_sp$X, date = data_sp$date, estacion = data_sp$estacion,nombre_var = data_sp$nombre_var,
                          unidad_sp = data_sp$unidad,
                          mean_sp = data_sp$mean, min_sp = data_sp$min, max_sp = data_sp$max,
                          sd_sp = data_sp$sd, n_sp = data_sp$n,mean_subt_sp= data_sp$mean_subt)
  
  
  ##
  data_t2m <- read.csv("t2m_dia.csv")
  data_t2m <- data.frame( x = data_t2m$X, date = data_t2m$date, estacion = data_t2m$estacion,nombre_var = data_t2m$nombre_var,
                          unidad_t2m = data_t2m$unidad,
                          mean_t2m = data_t2m$mean, min_t2m = data_t2m$min, max_t2m = data_t2m$max,
                          sd_t2m = data_t2m$sd, n_t2m = data_t2m$n,mean_subt_t2m= data_t2m$mean_subt)
  
  
  ##
  data_tp <- read.csv("tp_dia.csv")
  data_tp <- data.frame( x = data_tp$X, date = data_tp$date, estacion = data_tp$estacion,nombre_var = data_tp$nombre_var,
                          unidad_tp = data_tp$unidad,
                          mean_tp = data_tp$mean, min_tp = data_tp$min, max_tp = data_tp$max,
                          sd_tp = data_tp$sd, n_tp = data_tp$n,mean_subt_tp= data_tp$mean_subt)
  
  
  ##
  data_u10 <- read.csv("u10_dia.csv")
  data_u10 <- data.frame( x = data_u10$X, date = data_u10$date,estacion = data_u10$estacion, nombre_var = data_u10$nombre_var,
                          unidad_u10 = data_u10$unidad,
                          mean_u10 = data_u10$mean, min_u10 = data_u10$min, max_u10 = data_u10$max,
                          sd_u10 = data_u10$sd, n_u10 = data_u10$n,mean_subt_u10= data_u10$mean_subt)
  
  
  ##
  data_v10 <- read.csv("v10_dia.csv")
  data_v10 <- data.frame( x = data_v10$X, date = data_v10$date, estacion = data_v10$estacion,nombre_var = data_v10$nombre_var,
                          unidad_v10 = data_v10$unidad,
                          mean_v10 = data_v10$mean, min_v10 = data_v10$min, max_v10 = data_v10$max,
                          sd_v10 = data_v10$sd, n_v10 = data_v10$n,mean_subt_v10= data_v10$mean_subt)
  
  
  
  merged_df <- data_blh %>%
    #left_join(data_d2m, by = "date") %>%
    #left_join(data_sp, by = c("estacion","date") )%>%
    left_join(data_t2m, by = c("estacion","date")) #%>%
    #left_join(data_tp, by = c("estacion","date")) %>%
    #left_join(data_u10,by = c("estacion","date")) %>%
    #left_join(data_v10,by = c("estacion","date"))
  
  
  merged_df_subt <- data.frame(date=merged_df$date, blh_mean=merged_df$mean_blh,
                               blh_min=merged_df$min_blh,blh_max=merged_df$max_blh,
                               blh_sd=merged_df$sd_blh, blh_mean_subt=merged_df$mean_subt_blh,
                               
                               sp_mean=merged_df$mean_sp,
                               sp_min=merged_df$min_sp,sp_max=merged_df$max_sp,
                               sp_sd=merged_df$sd_sp, sp_mean_subt=merged_df$mean_subt_sp,

                               d2m_mean=merged_df$mean_d2m,
                               d2m_min=merged_df$min_d2m,d2m_max=merged_df$max_d2m,
                               d2m_sd=merged_df$sd_d2m, d2m_mean_subt=merged_df$mean_subt_d2m,
                               # 
                               t2m_mean=merged_df$mean_t2m,
                               t2m_min=merged_df$min_t2m,t2m_max=merged_df$max_t2m,
                               t2m_sd=merged_df$sd_t2m, t2m_mean_subt=merged_df$mean_subt_t2m,
                               
                               v10_mean=merged_df$mean_v10,
                               v10_min=merged_df$min_v10,v10_max=merged_df$max_v10,
                               v10_sd=merged_df$sd_v10, v10_mean_subt=merged_df$mean_subt_v10,

                               u10_mean=merged_df$mean_u10,
                               u10_min=merged_df$min_u10,u10_max=merged_df$max_u10,
                               u10_sd=merged_df$sd_u10, u10_mean_subt=merged_df$mean_subt_u10,

                               tp_mean=merged_df$mean_tp,
                               tp_min=merged_df$min_tp,tp_max=merged_df$max_tp,
                               tp_sd=merged_df$sd_tp, tp_mean_subt=merged_df$mean_subt_tp,
                               estacion = merged_df$estacion)
  
  
  write.csv(merged_df_subt,paste("D:/Josefina/Proyectos/ProyectoChile/dataset/meteoSatelital/",num_estacion,"_",estacion,"_data_meteo_sat_dia.csv",sep=""))
}

######
#unimos dataset de todas las estaciones juntas
setwd("D:/Josefina/Proyectos/ProyectoChile/dataset/meteoSatelital/")
dir <- "D:/Josefina/Proyectos/ProyectoChile/dataset/meteoSatelital/"
fs <- list.files(path = dir, 
                 pattern = ".csv",
                 full.names = FALSE)
data_rbind <- data.frame()        
for (i in 1:length(fs)){
  print(i)
  data <- read.csv(fs[i])
  data_rbind <- rbind(data_rbind,data)
  
} 
getwd()
write.csv(data_rbind,"08_tot_data_meteo_sat_dia.csv")


