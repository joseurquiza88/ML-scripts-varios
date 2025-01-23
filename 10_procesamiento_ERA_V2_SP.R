########################################################################
#media diaria
#Manualmente los separamos despues del codigo 02_procesamiento
rm(list=ls())
# dire <- "DJosefina/Proyectos/ProyectoChile/dataset/meteoSatelital/2015/02-2015/proceed/v10"
tipo <- "tp"
estacion<- "MD"
year <- 2020
j<-1
for (j in 1:length(tipo)){
  print("--------------------")
  print(tipo[j])
  print("--------------------")
  # dire <- paste("D:/Josefina/Proyectos/ProyectoChile/SP/proceed/05_ERA5/",year,"/",tipo,sep="")
  #dire <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/proceed/05_ERA5/V02/",year,"/",tipo,sep="")
  dire <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/proceed/05_ERA5/",year,"/",tipo[j],sep="")
  
  
  #blh, d2m, sp, t2m, tp,v10, u10
  
  setwd(dire)
  id <- list.files(path = getwd(), pattern = "*.csv", full.names = FALSE)
  df_rbind <- data.frame()
  i<-1
  for (i in 1:length(id)){
    print(i)
    data <- read.csv(id[i])
    ID <- data$ID[1]
  
    
    data$date <- as.Date(data$fecha, format = "%Y-%m-%d")
    #data$date <- as.Date(data$date, format = "%Y-%m-%d")
    # Filtrar el data frame para las horas especificadas
    data_subt <- data %>%
      filter(hora %in% c("12:00:00", "13:00:00", "14:00:00", "15:00:00", "16:00:00", "17:00:00"))
    
    # Agrupar por la columna 'estacion' y calcular las estadísticas
    df <- data %>%
      group_by(estacion,ID,date) %>%
      summarise(
        #ID = ID,
        date = date[1],#as.Date(fecha)[1],                       # Primer valor de fecha
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
  #View(df_rbind)
  nombre <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/proceed/05_ERA5/tot/",tipo[j],"/",tipo[j],"_",year,"_dia.csv",sep="")
  #write.csv(df_rbind,paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/proceed/05_ERA5/V02/tot/",tipo,"/",tipo,"_",year,"_dia.csv",sep=""))
  write.csv(df_rbind,nombre)
}
nrow(df_rbind)
View(df_rbind)

################
rm(list=ls())
estacion <- "MD"
tipo <- "u10"
dire <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/proceed/05_ERA5/tot/",tipo,sep="")
setwd(dire)
id <- list.files(path = getwd(), pattern = "*.csv", full.names = FALSE)
df_rbind <- data.frame()
for(x in 1:length(id)){
  print(x)
  data <- read.csv(id[x])
  print(nrow(data))
  df_rbind <- rbind (df_rbind,data)
}
write.csv(df_rbind, paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/proceed/05_ERA5/tot/",tipo,"_MX_dia.csv",sep=""))
######
#nimos dataset merge por dia
rm(list=ls())
estacion <- "MD"
dire <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/proceed/05_ERA5/tot/",sep="")

setwd(dire)


for (x in 1:1){
  data_blh <- read.csv(paste("blh_",estacion,"_dia.csv",sep=""))
  data_blh <- data.frame(date = data_blh$date, estacion = data_blh$estacion, ID = data_blh$ID,nombre_var = data_blh$nombre_var,
                          unidad_blh = data_blh$unidad,
                          mean_blh = data_blh$mean, min_blh = data_blh$min, max_blh = data_blh$max,
                          sd_blh = data_blh$sd, n_blh = data_blh$n)#,mean_subt_blh= data_blh$mean_subt)
  data_blh$date <- as.Date(data_blh$date, format = "%Y-%m-%d")#"%d/%m/%Y")
  data_blh <- data_blh[data_blh$ID != 0,]
  
  ##
  data_d2m <- read.csv(paste("d2m_",estacion,"_dia.csv",sep=""))
  data_d2m <- data.frame( date = data_d2m$date, estacion = data_d2m$estacion,ID = data_d2m$ID,nombre_var = data_d2m$nombre_var,
                          unidad_d2m = data_d2m$unidad,
                          mean_d2m = data_d2m$mean, min_d2m = data_d2m$min, max_d2m = data_d2m$max,
                          sd_d2m = data_d2m$sd, n_d2m = data_d2m$n)#,mean_subt_d2m= data_d2m$mean_subt)
  data_d2m$date <- as.Date(data_d2m$date, format = "%Y-%m-%d")#"%d/%m/%Y")
  data_d2m <- data_d2m[data_d2m$ID != 0,]
  ##
  data_sp <- read.csv(paste("sp_",estacion,"_dia.csv",sep=""))
  data_sp <- data.frame( date = data_sp$date, estacion = data_sp$estacion,ID = data_sp$ID,nombre_var = data_sp$nombre_var,
                          unidad_sp = data_sp$unidad,
                          mean_sp = data_sp$mean, min_sp = data_sp$min, max_sp = data_sp$max,
                          sd_sp = data_sp$sd, n_sp = data_sp$n)#,mean_subt_sp= data_sp$mean_subt)
  data_sp$date <- as.Date(data_sp$date, format = "%Y-%m-%d")#"%d/%m/%Y")
  data_sp <- data_sp[data_sp$ID != 0,]
  ##
  data_t2m <- read.csv(paste("t2m_",estacion,"_dia.csv",sep=""))
  data_t2m <- data.frame( date = data_t2m$date, estacion = data_t2m$estacion,ID = data_t2m$ID,nombre_var = data_t2m$nombre_var,
                          unidad_t2m = data_t2m$unidad,
                          mean_t2m = data_t2m$mean, min_t2m = data_t2m$min, max_t2m = data_t2m$max,
                          sd_t2m = data_t2m$sd, n_t2m = data_t2m$n)#,mean_subt_t2m= data_t2m$mean_subt)
  data_t2m$date <- as.Date(data_t2m$date, format = "%Y-%m-%d")#"%d/%m/%Y")
  data_t2m <- data_t2m[data_t2m$ID != 0,]
  ####################### VER!!!!!!
  data_tp <- read.csv(paste("tp_",estacion,"_dia.csv",sep=""))
  data_tp <- data.frame( date = data_tp$date, estacion = data_tp$estacion,ID = data_tp$ID,nombre_var = data_tp$nombre_var,
                          unidad_tp = data_tp$unidad,
                          mean_tp = data_tp$mean, min_tp = data_tp$min, max_tp = data_tp$max,
                          sd_tp = data_tp$sd, n_tp = data_tp$n)#,mean_subt_tp= data_tp$mean_subt)
  data_tp$date <- as.Date(data_tp$date, format = "%Y-%m-%d")#"%d/%m/%Y")
  data_tp <- data_tp[data_tp$ID != 0,]
  
  ##
  data_u10 <- read.csv(paste("u10_",estacion,"_dia.csv",sep=""))
  data_u10 <- data.frame( date = data_u10$date,estacion = data_u10$estacion, ID = data_u10$ID,nombre_var = data_u10$nombre_var,
                          unidad_u10 = data_u10$unidad,
                          mean_u10 = data_u10$mean, min_u10 = data_u10$min, max_u10 = data_u10$max,
                          sd_u10 = data_u10$sd, n_u10 = data_u10$n)#,mean_subt_u10= data_u10$mean_subt)
  data_u10$date <- as.Date(data_u10$date, format ="%Y-%m-%d")# "%Y-%m-%d")#"%d/%m/%Y")
  data_u10 <- data_u10[data_u10$ID != 0,]
  #class(data_u10$date)
  ##
  data_v10 <- read.csv(paste("v10_",estacion,"_dia.csv",sep=""))
  data_v10 <- data.frame( date = data_v10$date, estacion_v10 = data_v10$estacion,ID = data_v10$ID,nombre_var = data_v10$nombre_var,
                          unidad_v10 = data_v10$unidad,
                          mean_v10 = data_v10$mean, min_v10 = data_v10$min, max_v10 = data_v10$max,
                          sd_v10 = data_v10$sd, n_v10 = data_v10$n)#,mean_subt_v10= data_v10$mean_subt)

  data_v10$date <- as.Date(data_v10$date, format = "%Y-%m-%d")#"%Y-%m-%d")#"%d/%m/%Y")
  data_v10 <- data_v10[data_v10$ID != 0,]
  #length(unique(data_v10$ID))
  #merged_df <- merge(data_blh, data_d2m, by = c("date", "estacion"), all.y = FALSE, all.x = FALSE)
  #merged_df <- merged_df[complete.cases(merged_df),]
  merged_df <- data_blh %>%
    left_join(data_d2m, by = c("ID","date")) %>%
    left_join(data_sp, by = c("ID","date") )%>%
    left_join(data_t2m, by = c("ID","date")) %>%
    left_join(data_tp, by = c("ID","date")) %>%
    left_join(data_u10,by = c("ID","date")) %>%
    left_join(data_v10,by = c("ID","date"))
  
  ### Nos falto el ID
  # data_estacciones <- read.csv("D:/Josefina/Proyectos/ProyectoChile/SP/dataset/sitios.csv")
  # data_estacciones <- data_estacciones[data_estacciones$Considerado == "SI",]
  # puntos <- data_estacciones
  # puntos$estacion <- puntos$Nombre.sitio 
  # 
  # merged_df2 <- merged_df %>%
  #   left_join(puntos, by = c("estacion"))
    
  #write.csv(merged_df,"prueba.csv")
  merged_df_subt <- data.frame(date=merged_df$date, blh_mean=merged_df$mean_blh,
                               blh_min=merged_df$min_blh,blh_max=merged_df$max_blh,
                               blh_sd=merged_df$sd_blh,#, blh_mean_subt=merged_df$mean_subt_blh),
                               
                               sp_mean=merged_df$mean_sp,
                               sp_min=merged_df$min_sp,sp_max=merged_df$max_sp,
                               sp_sd=merged_df$sd_sp,# sp_mean_subt=merged_df$mean_subt_sp,

                               d2m_mean=merged_df$mean_d2m,
                               d2m_min=merged_df$min_d2m,d2m_max=merged_df$max_d2m,
                               d2m_sd=merged_df$sd_d2m, #d2m_mean_subt=merged_df$mean_subt_d2m,
                               # 
                               t2m_mean=merged_df$mean_t2m,
                               t2m_min=merged_df$min_t2m,t2m_max=merged_df$max_t2m,
                               t2m_sd=merged_df$sd_t2m, #t2m_mean_subt=merged_df$mean_subt_t2m,
                               
                               v10_mean=merged_df$mean_v10,
                               v10_min=merged_df$min_v10,v10_max=merged_df$max_v10,
                               v10_sd=merged_df$sd_v10,# v10_mean_subt=merged_df$mean_subt_v10,

                               u10_mean=merged_df$mean_u10,
                               u10_min=merged_df$min_u10,u10_max=merged_df$max_u10,
                               u10_sd=merged_df$sd_u10,# u10_mean_subt=merged_df$mean_subt_u10,

                               tp_mean=merged_df$mean_tp,
                               tp_min=merged_df$min_tp,tp_max=merged_df$max_tp,
                                tp_sd=merged_df$sd_tp,#, tp_mean_subt=merged_df$mean_subt_tp,
                               ID = merged_df$ID,
                               estacion = merged_df$estacion.x)
  
  # merged_df <- merged_df_subt %>%
  #   left_join(puntos, by = c("estacion"))
  # 
  # merged_df_subt <- data.frame(date=merged_df$date, blh_mean=merged_df$blh_mean,
  #                              blh_min=merged_df$blh_min,blh_max=merged_df$blh_max,
  #                              blh_sd=merged_df$blh_sd, blh_mean_subt=merged_df$blh_mean_subt,
  #                              
  #                              sp_mean=merged_df$sp_mean,
  #                              sp_min=merged_df$sp_min,sp_max=merged_df$sp_max,
  #                              sp_sd=merged_df$sp_sd, sp_mean_subt=merged_df$sp_mean_subt,
  #                              
  #                              d2m_mean=merged_df$d2m_mean,
  #                              d2m_min=merged_df$d2m_min,d2m_max=merged_df$d2m_max,
  #                              d2m_sd=merged_df$d2m_sd, d2m_mean_subt=merged_df$d2m_mean_subt,
  #                              # 
  #                              t2m_mean=merged_df$t2m_mean,
  #                              t2m_min=merged_df$t2m_min,t2m_max=merged_df$t2m_max,
  #                              t2m_sd=merged_df$t2m_sd, t2m_mean_subt=merged_df$t2m_mean_subt,
  #                              
  #                              v10_mean=merged_df$v10_mean,
  #                              v10_min=merged_df$v10_min,v10_max=merged_df$v10_max,
  #                              v10_sd=merged_df$v10_sd, v10_mean_subt=merged_df$v10_mean_subt,
  #                              
  #                              u10_mean=merged_df$u10_mean,
  #                              u10_min=merged_df$u10_min,u10_max=merged_df$u10_max,
  #                              u10_sd=merged_df$u10_sd, u10_mean_subt=merged_df$u10_mean_subt,
  #                              
  #                              tp_mean=merged_df$tp_mean,
  #                              tp_min=merged_df$tp_min,tp_max=merged_df$tp_max,
  #                              tp_sd=merged_df$tp_sd, tp_mean_subt=merged_df$tp_mean_subt,
  #                              ID = merged_df$ID, estacion = merged_df$estacion)
  # Elimina duplicados considerando todas las columnas
  df_sin_duplicados <- unique(merged_df_subt )
  
  # write.csv(merged_df_subt,paste("D:/Josefina/Proyectos/ProyectoChile/SP/proceed/05_ERA5/SP_data_meteo_sat_dia.csv",sep=""))
  write.csv(df_sin_duplicados,paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/proceed/05_ERA5/",estacion,"_ERA5.csv",sep=""))
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



# Revisar duplicados en `data_blh`
data_blh %>%
  group_by(date, ID) %>%
  summarise(count = n(), .groups = "drop") %>%
  filter(count > 1)

# Revisar duplicados en `data_d2m`
data_d2m %>%
  group_by(date, ID) %>%
  summarise(count = n(), .groups = "drop") %>%
  filter(count > 1)
