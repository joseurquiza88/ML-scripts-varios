########################################################################
#media diaria
#Manualmente los separamos despues del codigo 02_procesamiento
rm(list=ls())
# dire <- "DJosefina/Proyectos/ProyectoChile/dataset/meteoSatelital/2015/02-2015/proceed/v10"
tipo <- "u10"
estacion <- "PTA"
num_estacion <- "07"
dire <- paste("D:/Josefina/Proyectos/ProyectoChile/dataset/meteoSatelital/",num_estacion,"_",estacion,"/",tipo, sep="")
#blh, d2m, sp, t2m, tp,v10, u10

setwd(dire)
id <- list.files(path = getwd(), pattern = "*.csv", full.names = FALSE)
df_rbind <- data.frame()
for (i in 1:length(id)){
  print(i)
  data <- read.csv(id[i])
  # data_subt <-data[data$hora=="13:00:00" | data$hora=="14:00:00"| data$hora=="15:00:00" |
  #                    data$hora=="18:00:00" |data$hora=="19:00:00",]
  data_subt <-data[data$hora=="12:00:00" | data$hora=="13:00:00"| data$hora=="12:00:00" |
                     data$hora=="15:00:00" |data$hora=="16:00:00" |data$hora=="17:00:00",]
  
  
  df <- data.frame(date = as.Date(data$fecha)[1],
                   nombre_var = data$nombre_var[1],
                   unidad = data$unidad[1],
                   mean = mean(data$valor,na.rm = T),
                   min = min(data$valor,na.rm = T),
                   max = max(data$valor,na.rm = T),
                   sd =  sd (data$valor,na.rm = T),
                   n = length(data$valor),
                   mean_subt = mean(data_subt$valor,na.rm = T))
  df_rbind <- rbind(df_rbind ,df) 
}

write.csv(df_rbind,paste("D:/Josefina/Proyectos/ProyectoChile/dataset/meteoSatelital/",num_estacion,"_",estacion,"/",num_estacion,"_",estacion,"_",tipo,"_dia.csv",sep=""))
######
#nimos dataset merge por dia
setwd("D:/Josefina/Proyectos/ProyectoChile/dataset/meteoSatelital/")
rm(list=ls())
estacion <- "PTA"
num_estacion <- "07"
for (x in 1:1){
  data_blh <- read.csv(paste(num_estacion,"_",estacion,"/",num_estacion,"_",estacion,"_blh_dia.csv",sep=""))
  names(data_blh)<- c("X","date","nombre_var",paste("unidad_",data_blh$nombre_var[1],sep=""),"mean_blh",
                      "min_blh","max_blh","sd_blh","n", "mean_subt_blh")
  
  ##
  
  data_d2m <- read.csv(paste(num_estacion,"_",estacion,"/",num_estacion,"_",estacion,"_d2m_dia.csv",sep=""))
  
  names(data_d2m)<- c("X","date","nombre_var",paste("unidad_",data_d2m$nombre_var[1],sep=""),"mean_d2m",
                      "min_d2m","max_d2m","sd_d2m","n", "mean_subt_d2m")
  
  ##
  
  data_sp <- read.csv(paste(num_estacion,"_",estacion,"/",num_estacion,"_",estacion,"_sp_dia.csv",sep=""))
  names(data_sp)<- c("X","date","nombre_var",paste("unidad_",data_sp$nombre_var[1],sep=""),
                     "mean_sp","min_sp","max_sp","sd_sp","n", "mean_subt_sp")
  
  
  ##
  
  data_t2m <- read.csv(paste(num_estacion,"_",estacion,"/",num_estacion,"_",estacion,"_t2m_dia.csv",sep=""))
  
  names(data_t2m)<- c("X","date","nombre_var",paste("unidad_",data_t2m$nombre_var[1],sep=""),
                      "mean_t2m","min_t2m","max_t2m","sd_t2m","n", "mean_subt_t2m")
  
  
  ##
  data_tp <- read.csv(paste(num_estacion,"_",estacion,"/",num_estacion,"_",estacion,"_tp_dia.csv",sep=""))
  names(data_tp)<- c("X","date","nombre_var",paste("unidad_",data_tp$nombre_var[1],sep=""),
                     "mean_tp","min_tp","max_tp","sd_tp","n", "mean_subt_tp")
  
  
  ##
  
  data_u10 <- read.csv(paste(num_estacion,"_",estacion,"/",num_estacion,"_",estacion,"_u10_dia.csv",sep=""))
  names(data_u10)<- c("X","date","nombre_var",paste("unidad_",data_u10$nombre_var[1],sep=""),
                      "mean_u10","min_u10 ","max_u10 ","sd_u10 ","n", "mean_subt_u10 ")
  
  
  ##
  data_v10 <- read.csv(paste(num_estacion,"_",estacion,"/",num_estacion,"_",estacion,"_v10_dia.csv",sep=""))
  names(data_v10)<- c("X","date","nombre_var",paste("unidad_",data_v10$nombre_var[1],sep=""),
                      "mean_v10","min_v10 ","max_v10 ","sd_v10 ","n", "mean_subt_v10 ")
  
  
  
  merged_df <- data_blh %>%
    left_join(data_d2m, by = "date") %>%
    left_join(data_sp, by = "date") %>%
    left_join(data_t2m, by = "date") %>%
    left_join(data_tp, by = "date")%>%
    left_join(data_u10, by = "date")%>%
    left_join(data_v10, by = "date")
  merged_df_subt <- data.frame(date=merged_df$date, blh_mean=merged_df$mean_blh,
                               blh_min=merged_df$min_blh,blh_max=merged_df$max_blh,
                               blh_sd=merged_df$sd_blh, blh_mean_subt=merged_df$mean_subt_blh,
                               
                               sp_mean=merged_df$mean_sp,
                               sp_min=merged_df$min_sp,sp_max=merged_df$max_sp,
                               sp_sd=merged_df$sd_sp, sp_mean_subt=merged_df$mean_subt_sp,
                               
                               d2m_mean=merged_df$mean_d2m,
                               d2m_min=merged_df$min_d2m,d2m_max=merged_df$max_d2m,
                               d2m_sd=merged_df$sd_d2m, d2m_mean_subt=merged_df$mean_subt_d2m,
                               
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
                               estacion = estacion)
  
  
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


