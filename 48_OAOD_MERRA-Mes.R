#SP
##################################################################
#################################################################
# EXTRACXCION DATOS
rm(list=ls())#
year <- 2024
numRaster<- "01"
estacion<- "MX"
for (j in 1:1){
  # data_estacciones <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/dataset/estaciones/sitios_",estacion,".csv",sep=""))
  # 
  # data_estacciones <- data_estacciones[data_estacciones$Considerado == "SI",]
  # puntos <- data_estacciones
  # MD
  #aeronet <- data.frame(long=-75.578, lat = 6.261,estacion="AERONET",ID="0") #Medellin ( 6.261N, 75.578W)
  # MX
  aeronet <- data.frame(long=-99.182, lat = 19.334,estacion="AERONET",ID="0") #Mexico_City ( 19.334N, 99.182W)
  #SP
  #aeronet <- data.frame(long=-46.735,lat = -23.561,estacion="AERONET",ID="0") #Sao_Paulo (23.561S, 46.735W)
  # CH
  
  #aeronet <- data.frame(long=-70.662, lat =-33.457,estacion="AERONET",ID="0")# santiago 33.457S, 70.662W)
  # BA
  # aeronet <- data.frame(long=-58.50641,lat = -34.55542,estacion="AERONET",ID="0") #CEILAP-BA (34.555S, 58.506W)
  
  # #Location of AERONET stations
     # 
  # #aeronet <- data.frame(-68.066, -16.539)# La Paz 16.539S, 68.066W
  
  
  puntos <- aeronet
  
  crs_project <- "+proj=longlat +datum=WGS84"
  #punto <- SpatialPoints(coords, proj4string = CRS(crs_project))
  
  dire <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/dataset/07_MERRA-2_Dia_AOD/",year, sep="")
  
  setwd(dire)
  id <- list.files(path = getwd(),
                   pattern = "*.nc",
                   full.names = FALSE)
  #raster_template <- raster(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/dataset/rasterTemplate/",numRaster,"_raster_template.tif",sep=""))
  raster_template <- raster(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/dataset/rasterTemplate/",numRaster,"_raster_template.tif",sep=""))
  
  i<-1
  df_rbind <- data.frame()
  for (i in 1:length(id)){
    print(i)
    #print(paste("Esto es i = ", i, sep= ""))
    archivo_nc = id[i]
    
    
    MIRRAraster <- raster(archivo_nc)
    unit<- MIRRAraster@data@unit
    date <- substr(archivo_nc,28,35)
    crs_project <- "+proj=longlat +datum=WGS84"
    MIRRAraster2 <- projectRaster(MIRRAraster,
                                  crs = crs_project,
                                  method = "bilinear")
    rst_resampling <- raster::resample(MIRRAraster2, raster_template)
    valores_raster <- extract(rst_resampling, puntos[, c("long", "lat")])
    
    # Unir los valores del raster al dataframe original
    puntos_con_valores <- puntos %>%
      mutate(valor_raster = valores_raster)
    
    df <- data.frame (date= date,variable = "AOD_MERRA2", unidad=unit, value=puntos_con_valores$valor_raster,estacion=puntos_con_valores$estacion, ID = puntos_con_valores$ID)
    
    df_rbind <- rbind(df_rbind,df)
  }
  
}
getwd()
#nombre<- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/proceed/07_MERRA-2_Dia_AOD/",numRaster,"_MERRA2-AOD_",year,".csv",sep="")
nombre<- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/proceed/07_MERRA-2_Dia_AOD/_MERRA2-AOD_",year,".csv",sep="")

write.csv(df_rbind,nombre)


cd ##############################################################
dire <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/proceed/07_MERRA-2_Dia_AOD/",sep="")
setwd(dire)
id <- list.files(path = getwd(),
                 pattern = "*.csv",
                 full.names = FALSE)
i<-1
df_rbind<- data.frame()
for (i in 1:length(id)){
  print(i)
  data <- read.csv(id[i])
  df_rbind <- rbind(df_rbind,data)
  
  
}

write.csv(df_rbind,paste(numRaster,"_MERRA2-AOD_dia_",estacion,".csv",sep=""))
################################################################
# # MERGE SOLO CON AERONET PROMEDIO DIARIO
#MD
#data_aeronet <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/aeronet/datasets_interp_s_L02/Latam/4_MD_2015-2022_interp-s_V02_L2.csv")

#MX
data_aeronet <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/aeronet/datasets_interp_s_L02/Latam/6_MX_2015-2022_interp-s_V02_L2.csv")

#
estacion <- "SP"
numRaster <- "01"
data_merra_AOD <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/proceed/07_MERRA-2_Dia_AOD/",numRaster,"_MERRA2-AOD_dia_",estacion,".csv",sep=""))
data_merra_AOD <- data_merra_AOD[complete.cases(data_merra_AOD),]
data_aeronet <- data_aeronet[complete.cases(data_aeronet),]

data_aeronet$date <-  strptime(data_aeronet$date, format = "%Y-%m-%d %H:%M:%S")
data_aeronet_mean <- data_aeronet %>%
  mutate(date = as.Date(date)) %>%  # Extraer solo la fecha
  group_by(date) %>%                    # Agrupar por fecha
  summarise(aod_aeronet = mean(aod_550_mod, na.rm = TRUE),
            len = n() )  # Calcular la media

data_aeronet_mean$date<- strptime(data_aeronet_mean$date, format = "%Y-%m-%d")
data_merra_AOD$date <-  strptime(data_merra_AOD$date, format = "%Y%m%d")
data_merra_AOD$aod_merra <- data_merra_AOD$value
merged_df <- merge(data_aeronet_mean,data_merra_AOD, by = c("date"), all = FALSE)
#merged_df_subt <- merged_df[complete.cases(merged_df),]

##### PLOT
#lm(valor satelital ~ valor de ref)
modelo <- lm(aod_merra ~ aod_aeronet, data = merged_df)
# Calcular R?
r2 <- summary(modelo)$r.squared

# Calcular RMSE (Root Mean Squared Error)
#valor satelital
#rmse <- sqrt(mean((merged_df$aod_merra - predict(modelo))^2))
rmse <- sqrt(mean((merged_df$aod_merra  - merged_df$aod_aeronet )^2))

# Calcular Bias
# (valor satelital - valor de ref)
bias <- mean(merged_df$aod_merra - merged_df$aod_aeronet)


plot <- ggplot(merged_df, aes(x = aod_aeronet , y = aod_merra)) +
  geom_point(color = "#99d8c9", alpha = 0.8, size = 2) +  # Puntos del scatter plot
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Línea de regresión lineal
  labs(x = "AERONET", y = "MERRA-2", title = "AOD MERRA-2") +
  theme_classic() +
  
  #scale_x_continuous(limits = c(0, 160), breaks = seq(0, 160, by = 40)) +  # Ticks en el eje X
  #scale_y_continuous(limits = c(0, 160), breaks = seq(0, 160, by = 40)) +  # Ticks en el eje Y
  # 
  # # Añadir texto para R^2
  # geom_text(aes(x = 5, y = 120),
  #           label = expression(R^2 == 0.71),  # Coloca aquí el valor real de R^2
  #           size = 4, color = "Black") +
  # 
  # # Añadir texto para coeficiente de correlación Pearson (r)
  # # geom_text(aes(x = 4.2, y = 110), 
  # #           label = paste("r = ", round(pearson, 2)), 
  # #           size = 4, color = "Black") +
  # 
  # # Añadir texto para RMSE
  # geom_text(aes(x = 7.4, y = 100), 
  #           label = paste("RMSE = ", round(rmse, 2)), 
  #           size = 4, color = "Black") +
  # 
  # # Añadir texto para Bias
  # geom_text(aes(x = 6.6, y = 90), 
  #           label = paste("Bias = ", round(bias, 2)), 
  #           size = 4, color = "Black") +
  
  # Aumentar el tamaño de los ticks en los ejes X e Y
  theme(
    axis.text.x = element_text(size = 12),  # Tamaño de los ticks en el eje X
    axis.text.y = element_text(size = 12)   # Tamaño de los ticks en el eje Y
  )

plot
##################################################################
#################################################################
# MERGE CON MAIAC Y AERONET 
estacion <- "MX"
estacion2 <- "MX"
numRaster <- "01"

dir_aeronet <- "D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_AER-MAIAC/Latam_C61/dia/1km/"
data_aeronet <- read.csv(paste(dir_aeronet,"6_",estacion2,"-1km-MAIAC-60-AER_MEAN_c61.csv",sep=""))
data_aeronet <- data_aeronet[complete.cases(data_aeronet),]
data_merra_AOD <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/proceed/07_MERRA-2_Dia_AOD/",numRaster,"_MERRA2-AOD_dia_",estacion,".csv",sep=""))
data_merra_AOD <- data_merra_AOD[complete.cases(data_merra_AOD),]


data_aeronet$date <-  strptime(data_aeronet$date, format = "%Y-%m-%d")

data_merra_AOD$date <-  strptime(data_merra_AOD$date, format = "%Y%m%d")
data_merra_AOD$aod_merra <- data_merra_AOD$value

merged_df <- merge(data_aeronet,data_merra_AOD, by = c("date"), all = FALSE)


##### PLOT
#lm(valor satelital ~ valor de ref)
modelo <- lm(aod_merra ~ AOD_550_maiac_mean, data = merged_df)
modelo <- lm(aod_merra ~ AOD_550_AER_mean, data = merged_df)
modelo <- lm(AOD_550_maiac_mean ~ AOD_550_AER_mean, data = merged_df)

# Calcular R?
r2 <- summary(modelo)$r.squared
print(r2)
# Calcular R
R <- cor(merged_df$aod_merra, merged_df$AOD_550_maiac_mean)
R <- cor(merged_df$aod_merra, merged_df$AOD_550_AER_mean)
R <- cor(merged_df$AOD_550_maiac_mean, merged_df$AOD_550_AER_mean)
print(R)
# Calcular RMSE (Root Mean Squared Error)
#valor satelital
#rmse <- sqrt(mean((merged_df$aod_merra - predict(modelo))^2))
rmse <- sqrt(mean((merged_df$aod_merra  - merged_df$AOD_550_maiac_mean )^2))
rmse <- sqrt(mean((merged_df$aod_merra  - merged_df$AOD_550_AER_mean )^2))
rmse <- sqrt(mean((merged_df$AOD_550_maiac_mean  - merged_df$AOD_550_AER_mean )^2))

print(rmse)
# Calcular Bias
# (valor satelital - valor de ref)
bias <- mean(merged_df$aod_merra - merged_df$AOD_550_maiac_mean)
bias <- mean(merged_df$aod_merra - merged_df$AOD_550_AER_mean)
bias <- mean(merged_df$AOD_550_maiac_mean - merged_df$AOD_550_AER_mean)
print(bias)

plot <- ggplot(merged_df, aes(x = AOD_550_AER_mean , y = aod_merra)) +
  geom_point(color = "#99d8c9", alpha = 0.8, size = 2) +  # Puntos del scatter plot
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Línea de regresión lineal
  labs(x = "AERONET", y = "MERRA-2", title = "AOD MERRA-2") +
  theme_classic() +

theme(
  axis.text.x = element_text(size = 12),  # Tamaño de los ticks en el eje X
  axis.text.y = element_text(size = 12)   # Tamaño de los ticks en el eje Y
)

plot

##################################################################
#################################################################
### RASTER MAPAS
estacion <- "CH"
year <- "2024"
numRaster<- "01"
#ndvi_raster <- raster(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/dataset/rasterTemplate/",numRaster,"_raster_template.tif",sep=""))
ndvi_raster <- raster(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/dataset/rasterTemplate/raster_template.tif",sep=""))

dir_merra <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/dataset/07_MERRA-2_Dia_AOD/",year,"/",sep="")

dir_merra_guardado <-paste( "D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/dataset_ejemplo/Prediccion_",year,"/",sep="")

setwd(dir_merra)


id <- list.files(path = dir_merra,
                 pattern = "*.nc",
                 full.names = FALSE)
crs_project <- "+proj=longlat +datum=WGS84"
i<-1
for(i in 1:length(id)){
  print(i)
  merra <- id[i]
  
  # nombre_merra <- substr(merra,39,46)
  nombre_merra <- substr(merra,28,35)
  data_merra_AOD<- raster(merra)
  
  SINU <- as.character(data_merra_AOD@srs)
  data_merra_AOD <- projectRaster(data_merra_AOD,crs = crs_project)
  # Resamplear el raster original a la nueva resoluci?n de 1km
  resampled_merra_AOD <- raster::resample(data_merra_AOD, ndvi_raster,method = "bilinear")
  cropped_merra_AOD <- crop(resampled_merra_AOD, extent(ndvi_raster))
  
     # 06
  writeRaster(cropped_merra_AOD, paste(dir_merra_guardado,"/tiff/07_MERRA-2_Dia_AOD/",nombre_merra,"-AOD-MERRA_raster",sep=""), format="GTiff", overwrite=TRUE)
  
}

######################################################
##################################################
merged_df$date <- as.POSIXct(merged_df$date)
ggplot(merged_df, aes(x = date)) +
  # Línea para Registros.validados
  # Línea para valor_Raster
  geom_line(aes(y = aod_merra, color = "aod_merra"), size = 0.3,na.rm = FALSE) +
  
  #geom_line(aes(y = aod_aeronet, color = "aod_aeronet"), size = 0.3, na.rm = FALSE)+#, linetype = "dashed") +
  geom_line(aes(y = AOD_550_AER_mean, color = "aod_aeronet"), size = 0.3, na.rm = FALSE)+#, linetype = "dashed") +
  geom_line(aes(y = AOD_550_maiac_mean, color = "MAIAC"), size = 0.3, na.rm = FALSE)+#, linetype = "dashed") +
  
  #geom_line(aes(y = Registros.preliminares, color = "Registros.no.validados"), size = 0.3, na.rm = FALSE)+#, linetype = "dashed") +
  
  # Separar en subplots por estación
  #facet_wrap(~ estacion, scales = "free_y") +
  # 
  #scale_y_continuous(limits = c(0, 120),breaks = seq(0, 120, by = 40)) +  # Ticks cada 10 en el eje Y
  
  # Títulos y etiquetas
  labs(title = "MX",
       x = "Date",
       y = "AOD",
       color = "Variables") +
  # Cambiar los colores de las líneas
  scale_color_manual(values = c("aod_aeronet" = "#2ca25f", "aod_merra" = "#feb24c", "MAIAC" = "#de2d26"),
                     labels = c("aod_aeronet" = "AERONET", "aod_merra" = "MERRA-2","MAIAC" = "MAIAC")) +
  
  # Personalización del tema
  
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))
  theme_classic() +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),  # Tamaño y alineación del título
    axis.title.x = element_text(size = 8),              # Tamaño del título del eje X
    axis.title.y = element_text(size = 8),              # Tamaño del título del eje Y
    axis.text.x = element_text(size = 6, angle = 45, hjust = 1), # Tamaño y rotación de los ticks del eje X
    axis.text.y = element_text(size = 6),               # Tamaño de los ticks del eje Y
    strip.text = element_text(size = 5),                # Tamaño del texto de los subplots
    legend.title = element_text(size = 8),              # Tamaño del título de la leyenda
    legend.text = element_text(size = 5)                # Tamaño del texto de la leyenda
  )
plot
