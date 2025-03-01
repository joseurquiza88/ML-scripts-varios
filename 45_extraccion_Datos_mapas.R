##############################################################################
##otra forma de extraer los datos
estacion <- "MX"
modelo <- "02-RF_cv_M1-070225-MX"
modelo <- "07-RF_ESP_cv_M6-180225-MX"
modelo <-"02-RF_cv_M1-080125_MD"
modelo <-"02-RF_cv_M1-251124_SP"
modelo<- "02-RF_cv_M1-251124_SP_AOD-MERRA"
modelo<- "07-RF-ESP_cv_M6-110225-SP"
modelo<- "07-RF-ESP_cv_M6-110225-MD"
modelo <- "02-RF_cv_M1-120225-CH"
modelo <- "07-RF_esp_cv_M6-180225-CH"
modelo <- "02-RF_cv_M1-070225-SP"
modelo <- "07-RF-ESP_cv_M6-110225-SP"
year<-2024
# modelo <- "Salida_03-XGB_cv_M1-041024"

# modelo <- "02-RF_cv_M1-261224_MX"
#modelo <- "02-RF_cv_M1-251124_SP"
dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/Salidas/SalidasDiarias/",modelo,"/",year,"/",sep="")



setwd(dir)
id <- list.files(path = dir,
                 pattern = "*.tif",
                 full.names = FALSE)

#data_estacciones <- read.csv("D:/Josefina/Proyectos/ProyectoChile/SP/dataset/sitios.csv")
data_estacciones <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/dataset/estaciones/sitios_",estacion,".csv",sep=""))
# data_estacciones <- data_estacciones[data_estacciones$tipo == "LCS",]
data_estacciones <- data_estacciones[data_estacciones$Considerado=="SI",]
data_estacciones <- data_estacciones[data_estacciones$tipo=="referencia",]

#data_estacciones <- data_estacciones[22:24,]
puntos <- data_estacciones
unique(puntos$archivo)



crs_project <- "+proj=longlat +datum=WGS84"
df_rbind <- data.frame()
#for (x in 1:1){
# Dataframe con coordenadas y nombres de estaciones
i<-1
for (i in 1:length(id)){
  print(i)
  pred_raster <- raster(id[i])
  
  #plot(pred_raster)
  # Extraer los valores del raster en las coordenadas especificadas
  valores_raster <- extract(pred_raster, puntos[, c("long", "lat")])
  
  # Unir los valores del raster al dataframe original
  puntos_con_valores <- puntos %>%
    mutate(valor_raster = valores_raster)
  
  fechaInteres <- as.Date(substr(id[i],4,13), format = "%Y-%m-%d")# Mostrar el dataframe resultante
  # print(puntos_con_valores)
  puntos_con_valores$date <- fechaInteres
  
  df_rbind <- rbind(df_rbind,puntos_con_valores)
}

getwd()
#write.csv(df_rbind, paste("Salida_03-XGB_cv_M1-041024_LCS-Sensor.csv",sep=""))
# write.csv(df_rbind, paste("02-RF_cv_M1-261224_MX.csv",sep=""))
# write.csv(df_rbind, paste("02-RF_cv_M1-251124_SP_MERRA.csv",sep=""))
write.csv(df_rbind, paste("07-RF-ESP_cv_M6-110225-SP.csv",sep=""))
write.csv(df_rbind, paste("07-RF-ESP_cv_M6-110225-MD.csv",sep=""))
write.csv(df_rbind, paste("02-RF_cv_M1-120225-CH.csv",sep=""))
write.csv(df_rbind, paste("02-RF_cv_M1-080125_MD.csv",sep=""))
write.csv(df_rbind, paste("07-RF_esp_cv_M6-180225-CH.csv",sep=""))
write.csv(df_rbind, paste("02-RF_cv_M1-070225-SP.csv",sep=""))
write.csv(df_rbind, paste("07-RF-ESP_cv_M6-110225-SP.csv",sep=""))
write.csv(df_rbind, paste("07-RF_ESP_cv_M6-180225-MX.csv",sep=""))
write.csv(df_rbind, paste("02-RF_cv_M1-070225-MX.csv",sep=""))
###############################################################
# Merge datos sensores
estacion <- "MX"
modelo <- "02-RF_cv_M1-070225-MX"
modelo <- "07-RF_ESP_cv_M6-180225-MX"
modelo <- "07-RF-ESP_cv_M6-110225-SP"
modelo <- "02-RF_cv_M1-070225-SP"
modelo <-"02-RF_cv_M1-080125_MD"
modelo <-"07-RF-ESP_cv_M6-110225-SP"
modelo <-"07-RF-ESP_cv_M6-110225-MD"
modelo <- "02-RF_cv_M1-080125_MD"
# modelo <- "Salida_03-XGB_cv_M1-041024"
modelo <- "02-RF_cv_M1-261224_MX"
modelo <- "02-RF_cv_M1-251124_SP"
modelo <- "02-RF_cv_M1-120225-CH"
modelo <- "07-RF_esp_cv_M6-180225-CH"
dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/Salidas/SalidasDiarias/",modelo,"/",sep="")
data_sensores <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/proceed/06_estaciones/",estacion,"_estaciones.csv",sep=""))

#CH
#data_sensores <- read.csv("D:/Josefina/Proyectos/Sensores/proceed/data/media_diria_sensores.csv")
year<-2024
#data_sensores <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/proceed/06_estaciones/",estacion,"_estaciones.csv",sep=""))
data_modelo <- read.csv( paste(dir,"/2024/",modelo,".csv",sep=""))
data_sensores <- data_sensores[complete.cases(data_sensores$date),]
data_sensores$date <- as.POSIXct(data_sensores$date, format = "%Y-%m-%d")#"%d/%m/%Y")
data_modelo$date <- as.POSIXct(data_modelo$date, format = "%Y-%m-%d")
#df_rbind_merra$date <- as.POSIXct(df_rbind_merra$date, format = "%Y-%m-%d")
#data_sensores$date <- as.POSIXct(data_sensores$date, format = "%Y-%m-%d")

#CH
#data_sensores$ID_archivo <- data_sensores$ID_archivo_num
#merged_df <- merge(data_modelo, data_sensores, by = c("ID_archivo", "date","archivo"), all.x = TRUE)
merged_df <- merge(data_modelo,data_sensores, by = c("ID", "date"), all.x = TRUE)

#merged_df_subt <- merged_df[complete.cases(merged_df$Registros.completos),]
merged_df_subt <- merged_df[complete.cases(merged_df$mean),]
merged_df_subt2 <- merged_df_subt[complete.cases(merged_df_subt$valor_raster),]
View(merged_df_subt)
getwd()

write.csv(merged_df_subt,"merged_df_subt.csv")
merged_df_subt <- read.csv("merged_df_subt.csv")

length(unique(merged_df_subt$estacion))
merged_df_subt$estacion<-merged_df_subt$estacion.x
merged_df_subt$mean<-merged_df_subt$Registros.completos
merged_df_subt$date<- as.POSIXct(merged_df_subt$date, format = "%Y-%m-%d")
merged_df_subt <- merged_df_subt[complete.cases(merged_df_subt$valor_raster),]
ggplot(merged_df_subt, aes(x = date)) +
  #ggplot(df_date_rbind, aes(x = date)) +
  # Línea para Registros.validados
    # Línea para valor_Raster
  geom_line(aes(y = mean, color = "Monitoreo"), size = 0.3,na.rm = TRUE) +
  
  geom_line(aes(y = valor_raster, color = "Modelo"), size = 0.3, na.rm = TRUE)+#, linetype = "dashed") +
  #geom_line(aes(y = valor_raster.y, color = "MERRA-2"), size = 0.3, na.rm = FALSE)+#, linetype = "dashed") +
  
    #geom_line(aes(y = Registros.preliminares, color = "Registros.no.validados"), size = 0.3, na.rm = FALSE)+#, linetype = "dashed") +
  
  # Separar en subplots por estación
  facet_wrap(~ estacion , scales = "free_y") +
  # 
  scale_y_continuous(limits = c(0, 120),breaks = seq(0, 120, by = 40)) +  # Ticks cada 10 en el eje Y
  
  # Títulos y etiquetas
  labs(title = "02-RF_cv_M1-070225-MX",
    x = "",
    y = "PM2.5",
    color = "Variables") +
  # Cambiar los colores de las líneas
  scale_color_manual(values = c("Monitoreo" = "#2ca25f", "Modelo" = "#feb24c"),#,"Monitoreo"="blue"),
                     labels = c("Monitoreo" = "Monitoreo", "Modelo" = "Modelo"))+#, "mean"="Monitoreo")) +
  
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
  
 
  ###################################################################
  ################################################################
  #################################################################
  #REGRESION LINEAL CON TODOS LOS SENSORES
  # Mi valor de referencia es el modelo no las mediciones de LCS
  #merged_df_subt <- merged_df_subt[complete.cases(merged_df_subt),]
  # Ajuste del modelo de regresión lineal
getwd()
  nombreModelo <- "02-RF_cv_M1-070225-MX"
  #modelo <- lm(valor_raster ~mean, data = merged_df_subt)
  modelo <- lm( mean~valor_raster , data = merged_df_subt)
  # Calculo de métricas de desempeño
  R2 <- summary(modelo)$r.squared
  RMSE <- sqrt(mean(residuals(modelo)^2))
  # Bias <- mean(merged_df_subt$valor_raster - merged_df_subt$mean)
  Bias <- mean(merged_df_subt$mean - merged_df_subt$valor_raster)
  n <- nrow(merged_df_subt)
  
  # Crear el gráfico con ggplot2
  plot <- ggplot(merged_df_subt, aes(y = mean, x= valor_raster)) +
    geom_abline(slope = 1, intercept = 0,  color = "black", size = 0.5) +  # Línea 1:1
    
    geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
    geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE, linetype = "dashed") +  # Línea de regresión
    # scale_y_continuous(limits = c(0, 140),breaks = seq(0, 140, by = 40)) +  # Ticks cada 10 en el eje Y
    # scale_x_continuous(limits = c(0, 140),breaks = seq(0, 140, by = 40)) +  # Ticks cada 10 en el eje Y
    scale_y_continuous(limits = c(0, 80),breaks = seq(0, 80, by = 20)) +  # Ticks cada 10 en el eje Y
    scale_x_continuous(limits = c(0, 80),breaks = seq(0, 80, by = 20)) +  # Ticks cada 10 en el eje Y
    
    labs(
      x = "Monitoreo",
      y = "Modelo",
      title = 
        nombreModelo,#"02-RF_cv_M1-120225-CH",
      subtitle = paste(
        "R2 =", round(R2, 2),
        "| RMSE =", round(RMSE, 2),
        "| Bias =", round(Bias, 2),
        "| n = ", n
      )
    ) +
    theme_classic() #+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))
  plot
  
  
  
  
  
  
  ###################################################################
  ################################################################
  ###################################################################
  ################################################################
  # Realizar el gráfico con los datos filtrados
  ggplot(df_date_rbind_filtered, aes(x = date)) +
    geom_line(aes(y = mean, color = "mean"), size = 0.3, na.rm = FALSE) +
    geom_line(aes(y = valor_raster, color = "valor_raster"), size = 0.3, na.rm = FALSE) +
    facet_wrap(~ ID, scales = "free_y") +
    labs(title = "Salida_02-RF_cv_M1-251124_SP",
         x = "Date",
         y = "PM2.5",
         color = "Variables") +
    scale_color_manual(values = c("valor_raster" = "#2ca25f", "mean" = "#feb24c"),
                       labels = c("valor_raster" = "Modelo", "mean" = "Monitoreo")) +
    theme_classic() +
    theme(
      plot.title = element_text(size = 10, hjust = 0.5),
      axis.title.x = element_text(size = 8),
      axis.title.y = element_text(size = 8),
      axis.text.x = element_text(size = 6, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 6),
      strip.text = element_text(size = 5),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 5)
    )
  

############ Sensores LCS CH
data_sensores_ST <- read.csv("D:/Josefina/Proyectos/Sensores/proceed/data/media_diaria_sensores.csv")
data_modelo_ST <- read.csv("D:/Josefina/Proyectos/ProyectoChile/CH/modelos/Salidas/SalidasDiarias/Salida_03-XGB_cv_M1-041024/Salida_03-XGB_cv_M1-041024_LCS-Sensor.csv")
data_sensores_ST$mean
data_sensores_ST <- data_sensores_ST[complete.cases(data_sensores_ST$mean),]
data_sensores_ST$date <- as.POSIXct(data_sensores_ST$date, format = "%d/%m/%Y")
data_modelo_ST$date <- as.POSIXct(data_modelo_ST$date, format = "%Y-%m-%d")
#Nombres para ver cuales unir
names(data_modelo_ST)
names(data_sensores_ST)
unique(data_modelo_ST$archivo)
unique(data_sensores_ST$archivo)
unique(data_sensores_ST$ID_archivo_num)
unique(data_modelo_ST$ID)
data_modelo_ST$ID_archivo
data_sensores_ST$ID_archivo<- data_sensores_ST$ID_archivo_num

merged_df <- merge(data_modelo_ST, data_sensores_ST, by = c("ID_archivo", "date","archivo"), all.x = TRUE)

merged_df_subt <- merged_df[complete.cases(merged_df),]

getwd()
write.csv(merged_df_subt,"D:/Josefina/Proyectos/ProyectoChile/CH/modelos/Salidas/Sensores/sensores_modelos_validacion.csv")
##########################################################
### Ploteamos solo algunos

data_ch <- read.csv("D:/Josefina/Proyectos/ProyectoChile/CH/modelos/Salidas/Sensores/sensores_modelos_validacion.csv")
merged_df_subt<- data_ch
# lista_maipu_invierno <- list("05-Dimension","07-Maipu Seguro", "10-Instituto Luis Gandarilla",
#               "14-Sede Social Ricardo Ayala","15-Casa del folklore","17-Señora Elba")
#merged_df_subt_maipuInv <- merged_df_subt[merged_df_subt$estacion %in% lista_maipu_invierno, ]
merged_df_subt_maipuInv <- merged_df_subt[merged_df_subt$archivo== "Maipu Invierno", ]



# Crear una secuencia completa de fechas desde la mínima hasta la máxima
merged_df_subt_maipuInv$date <- as.Date(merged_df_subt_maipuInv$date)
fechas_completas <- data.frame(date = seq(as.Date("2021-07-01"), as.Date("2021-07-31"), by = "day"))
maipuInv_date <- merge(fechas_completas, merged_df_subt_maipuInv, by = c( "date"), all.x = TRUE)
unique(maipuInv_date$estacion)

#maipuInv_date <- maipuInv_date[maipuInv_date$estacion != NA,]
# Ver resultado
#print(df_completo)
maipuInv_date_plot <- ggplot(maipuInv_date, aes(x = date)) +
  # Línea para Registros.validados
  geom_line(aes(y = valor_raster, color = "valor_raster"), size = 0.7,na.rm = TRUE) +
  geom_line(aes(y = mean, color = "mean"), size =0.7, na.rm = TRUE)+#, linetype = "dashed") +
  geom_point(aes(y = valor_raster, color = "valor_raster"), size = 1,na.rm = TRUE) +
  geom_point(aes(y = mean, color = "mean"), size = 1, na.rm = TRUE)+#, linetype = "dashed") +
  
  
  #geom_line(aes(y = Registros.preliminares, color = "Registros.no.validados"), size = 0.3, na.rm = FALSE)+#, linetype = "dashed") +
  
  # Separar en subplots por estación
  facet_wrap(~ ID_archivo, scales = "free_y") +
  # 
  scale_y_continuous(limits = c(0, 180),breaks = seq(0, 180, by = 40)) +  # Ticks cada 10 en el eje Y
  
  # Títulos y etiquetas
  labs(title = "Maipu Invierno_03-XGB_cv_M1-041024",
    x = "Date",
    y = "PM2.5",
    color = "Variables") +
  # Cambiar los colores de las líneas
  # Cambiar los colores y nombres de las variables en la leyenda
  scale_color_manual(
    values = c("valor_raster" = "#2ca25f", "mean" = "#feb24c"),
    labels = c("valor_raster" = "Modelo", "mean" = "LCS")
  ) +
  
  # Personalización del tema
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
maipuInv_date_plot
setwd("D:/Josefina/Proyectos/ProyectoChile/CH/plots/LCS")
ggsave("grafico_maipuInv.png", plot = maipuInv_date_plot, width = 11, height = 6)

####################
### Ploteamos solo algunos
#lista_maipu_Verano <- list("19-Casa del Folklore","20-Seguridad Ciudadana","23-Empresa DimensiÃ³n",
 #                            "24-Instituto Luis Gandarillas","27-Sede Social Ricardo Ayala","28-Casa SeÃ±ora Elbita, Jorge Obrien 1447")
#merged_df_subt_maipuVerano <- merged_df_subt[merged_df_subt$estacion %in% lista_maipu_Verano, ]
merged_df_subt_maipuVerano <- merged_df_subt[merged_df_subt$archivo== "Maipu Verano", ]

unique(merged_df_subt_maipuVerano$estacion)
# Crear una secuencia completa de fechas desde la mínima hasta la máxima
merged_df_subt_maipuVerano$date <- as.Date(merged_df_subt_maipuVerano$date)
fechas_completas_MVerano <- data.frame(date = seq(as.Date("2021-01-01"), as.Date("2021-01-31"), by = "day"))
maipuVerano_date <- merge(fechas_completas_MVerano, merged_df_subt_maipuVerano, by = c( "date"), all.x = TRUE)


maipuVer_date_plot <- ggplot(maipuVerano_date, aes(x = date)) +
  # Línea para Registros.validados
  geom_line(aes(y = valor_raster, color = "valor_raster"), size = 0.7,na.rm = TRUE) +
  geom_line(aes(y = mean, color = "mean"), size =0.7, na.rm = TRUE)+#, linetype = "dashed") +
  geom_point(aes(y = valor_raster, color = "valor_raster"), size = 1,na.rm = TRUE) +
  geom_point(aes(y = mean, color = "mean"), size = 1, na.rm = TRUE)+#, linetype = "dashed") +
  
  
  #geom_line(aes(y = Registros.preliminares, color = "Registros.no.validados"), size = 0.3, na.rm = FALSE)+#, linetype = "dashed") +
  
  # Separar en subplots por estación
  facet_wrap(~ ID_archivo, scales = "free_y") +
  # 
  scale_y_continuous(limits = c(0, 40),breaks = seq(0, 40, by = 10)) +  # Ticks cada 10 en el eje Y
  
  # Títulos y etiquetas
  labs(title = "Maipu Verano_03-XGB_cv_M1-041024",
       x = "Date",
       y = "PM2.5",
       color = "Variables") +
  # Cambiar los colores de las líneas
  scale_color_manual(
    values = c("valor_raster" = "#2ca25f", "mean" = "#feb24c"),
    labels = c("valor_raster" = "Modelo", "mean" = "LCS")
  ) +
  # Personalización del tema
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
maipuVer_date_plot
ggsave("grafico_maipuVerano.png", plot = maipuVer_date_plot, width = 11, height = 6)



####################
### Ploteamos solo algunos
# lista_Florida<- list("46-Colegio Los Almendros","47-Jardín Los Navíos",
#                      "50-Jardín Miguitas de Ternura","53-Carlos Castellón 1475",
#                      "55-Junta Vecinos Las Gardenias","57-Junta Vecinos Quinchao")
#merged_df_subt_florida <- merged_df_subt[merged_df_subt$estacion %in% lista_Florida, ]
merged_df_subt_florida_Invierno <- merged_df_subt[merged_df_subt$archivo== "Florida Invierno", ]


unique(merged_df_subt_florida$estacion)
# Crear una secuencia completa de fechas desde la mínima hasta la máxima
merged_df_subt_florida_Invierno$date <- as.Date(merged_df_subt_florida_Invierno$date)
fechas_completas_FloridaInvierno <- data.frame(date = seq(as.Date("2021-06-01"), as.Date("2021-07-31"), by = "day"))
Florida_date_invierno <- merge(fechas_completas_FloridaInvierno, merged_df_subt_florida_Invierno, by = c( "date"), all.x = TRUE)


florida_date_plot_inv <- ggplot(Florida_date_invierno, aes(x = date)) +
  # Línea para Registros.validados
  geom_line(aes(y = valor_raster, color = "valor_raster"), size = 0.7,na.rm = TRUE) +
  geom_line(aes(y = mean, color = "mean"), size =0.7, na.rm = TRUE)+#, linetype = "dashed") +
  geom_point(aes(y = valor_raster, color = "valor_raster"), size = 1,na.rm = TRUE) +
  geom_point(aes(y = mean, color = "mean"), size = 1, na.rm = TRUE)+#, linetype = "dashed") +
  
  
  #geom_line(aes(y = Registros.preliminares, color = "Registros.no.validados"), size = 0.3, na.rm = FALSE)+#, linetype = "dashed") +
  
  # Separar en subplots por estación
  facet_wrap(~ estacion, scales = "free_y") +
  # 
  scale_y_continuous(limits = c(0, 140),breaks = seq(0, 140, by = 40)) +  # Ticks cada 10 en el eje Y
  
  # Títulos y etiquetas
  labs(title = "La Florida Invierno_03-XGB_cv_M1-041024",
       x = "Date",
       y = "PM2.5",
       color = "Variables") +
  # Cambiar los colores de las líneas
  scale_color_manual(
    values = c("valor_raster" = "#2ca25f", "mean" = "#feb24c"),
    labels = c("valor_raster" = "Modelo", "mean" = "LCS")
  ) +
  # Personalización del tema
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
florida_date_plot_inv 
ggsave("grafico_FloridaInvierno.png", plot = florida_date_plot_inv , width = 11, height = 6)


####################
merged_df_subt_florida_verano <- merged_df_subt[merged_df_subt$archivo== "Florida Verano", ]


unique(merged_df_subt_florida$estacion)
# Crear una secuencia completa de fechas desde la mínima hasta la máxima
merged_df_subt_florida_verano$date <- as.Date(merged_df_subt_florida_verano$date)
fechas_completas_Florida <- data.frame(date = seq(as.Date("2021-03-01"), as.Date("2021-03-31"), by = "day"))
Florida_date <- merge(fechas_completas_Florida, merged_df_subt_florida_verano, by = c( "date"), all.x = TRUE)


florida_date_plot_verano <- ggplot(Florida_date, aes(x = date)) +
  # Línea para Registros.validados
  geom_line(aes(y = valor_raster, color = "valor_raster"), size = 0.7,na.rm = TRUE) +
  geom_line(aes(y = mean, color = "mean"), size =0.7, na.rm = TRUE)+#, linetype = "dashed") +
  geom_point(aes(y = valor_raster, color = "valor_raster"), size = 1,na.rm = TRUE) +
  geom_point(aes(y = mean, color = "mean"), size = 1, na.rm = TRUE)+#, linetype = "dashed") +
  
  
  #geom_line(aes(y = Registros.preliminares, color = "Registros.no.validados"), size = 0.3, na.rm = FALSE)+#, linetype = "dashed") +
  
  # Separar en subplots por estación
  facet_wrap(~ estacion, scales = "free_y") +
  # 
  scale_y_continuous(limits = c(0, 140),breaks = seq(0, 140, by = 40)) +  # Ticks cada 10 en el eje Y
  
  # Títulos y etiquetas
  labs(title = "La Florida Verano_03-XGB_cv_M1-041024",
       x = "Date",
       y = "PM2.5",
       color = "Variables") +
  # Cambiar los colores de las líneas
  scale_color_manual(
    values = c("valor_raster" = "#2ca25f", "mean" = "#feb24c"),
    labels = c("valor_raster" = "Modelo", "mean" = "LCS")
  ) +
  # Personalización del tema
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
florida_date_plot_verano
ggsave("grafico_FloridaVerano.png", plot = florida_date_plot, width = 11, height = 6)



####################
####################
merged_df_subt_quilicuraVerano <- merged_df_subt[merged_df_subt$archivo== "Quilicura Verano", ]


unique(merged_df_subt_quilicuraVerano$estacion)
# Crear una secuencia completa de fechas desde la mínima hasta la máxima
merged_df_subt_quilicuraVerano$date <- as.Date(merged_df_subt_quilicuraVerano$date)
fechas_completas_quilicuraVerano <- data.frame(date = seq(as.Date("2021-01-01"), as.Date("2021-01-31"), by = "day"))
QuilicuraVerano_date <- merge(fechas_completas_quilicuraVerano, merged_df_subt_quilicuraVerano, by = c( "date"), all.x = TRUE)


QuilicuraVerano_date_plot <- ggplot(QuilicuraVerano_date, aes(x = date)) +
  # Línea para Registros.validados
  geom_line(aes(y = valor_raster, color = "valor_raster"), size = 0.7,na.rm = TRUE) +
  geom_line(aes(y = mean, color = "mean"), size =0.7, na.rm = TRUE)+#, linetype = "dashed") +
  geom_point(aes(y = valor_raster, color = "valor_raster"), size = 1,na.rm = TRUE) +
  geom_point(aes(y = mean, color = "mean"), size = 1, na.rm = TRUE)+#, linetype = "dashed") +
  
  
  #geom_line(aes(y = Registros.preliminares, color = "Registros.no.validados"), size = 0.3, na.rm = FALSE)+#, linetype = "dashed") +
  
  # Separar en subplots por estación
  facet_wrap(~ estacion, scales = "free_y") +
  # 
  scale_y_continuous(limits = c(0, 40),breaks = seq(0, 40, by = 10)) +  # Ticks cada 10 en el eje Y
  
  # Títulos y etiquetas
  labs(title = "Quilicura Verano_03-XGB_cv_M1-041024",
       x = "Date",
       y = "PM2.5",
       color = "Variables") +
  # Cambiar los colores de las líneas
  scale_color_manual(
    values = c("valor_raster" = "#2ca25f", "mean" = "#feb24c"),
    labels = c("valor_raster" = "Modelo", "mean" = "LCS")
  ) +
  # Personalización del tema
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
QuilicuraVerano_date_plot
ggsave("grafico_QuilicuraVerano.png", plot = QuilicuraVerano_date_plot, width = 11, height = 6)

Quilicura_invierno
####################
####################
merged_df_subt_quilicuraInvierno <- merged_df_subt[merged_df_subt$archivo== "Quilicura_invierno", ]


unique(merged_df_subt_quilicuraInvierno$estacion)
# Crear una secuencia completa de fechas desde la mínima hasta la máxima
merged_df_subt_quilicuraInvierno$date <- as.Date(merged_df_subt_quilicuraInvierno$date)
fechas_completas_quilicuraInvierno <- data.frame(date = seq(as.Date("2021-07-01"), as.Date("2021-07-31"), by = "day"))
QuilicuraInvierno_date <- merge(fechas_completas_quilicuraInvierno, merged_df_subt_quilicuraInvierno, by = c( "date"), all.x = TRUE)


QuilicuraInvierno_date_plot <- ggplot(QuilicuraInvierno_date, aes(x = date)) +
  # Línea para Registros.validados
  geom_line(aes(y = valor_raster, color = "valor_raster"), size = 0.7,na.rm = TRUE) +
  geom_line(aes(y = mean, color = "mean"), size =0.7, na.rm = TRUE)+#, linetype = "dashed") +
  geom_point(aes(y = valor_raster, color = "valor_raster"), size = 1,na.rm = TRUE) +
  geom_point(aes(y = mean, color = "mean"), size = 1, na.rm = TRUE)+#, linetype = "dashed") +
  
  
  #geom_line(aes(y = Registros.preliminares, color = "Registros.no.validados"), size = 0.3, na.rm = FALSE)+#, linetype = "dashed") +
  
  # Separar en subplots por estación
  facet_wrap(~ estacion, scales = "free_y") +
  # 
  scale_y_continuous(limits = c(0, 180),breaks = seq(0, 180, by = 40)) +  # Ticks cada 10 en el eje Y
  
  # Títulos y etiquetas
  labs(title = "Quilicura Invierno_03-XGB_cv_M1-041024",
       x = "Date",
       y = "PM2.5",
       color = "Variables") +
  # Cambiar los colores de las líneas
  scale_color_manual(
    values = c("valor_raster" = "#2ca25f", "mean" = "#feb24c"),
    labels = c("valor_raster" = "Modelo", "mean" = "LCS")
  ) + 
  # Personalización del tema
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
QuilicuraInvierno_date_plot
ggsave("grafico_QuilicuraInvierno.png", plot = QuilicuraInvierno_date_plot, width = 11, height = 6)


####################
####################
# Vitacura Invierno
merged_df_subt_VitacuraInvierno <- merged_df_subt[merged_df_subt$archivo== "Vitacura Invierno", ]


unique(merged_df_subt_VitacuraInvierno$estacion)
# Crear una secuencia completa de fechas desde la mínima hasta la máxima
merged_df_subt_VitacuraInvierno$date <- as.Date(merged_df_subt_VitacuraInvierno$date)
fechas_completas_VitacuraInvierno <- data.frame(date = seq(as.Date("2021-07-01"), as.Date("2021-07-31"), by = "day"))
VitacuraInvierno_date <- merge(fechas_completas_VitacuraInvierno, merged_df_subt_VitacuraInvierno, by = c( "date"), all.x = TRUE)
VitacuraInvierno_date$ID_archivo

VitacuraInvierno_date_plot <- ggplot(VitacuraInvierno_date, aes(x = date)) +
  # Línea para Registros.validados
  geom_line(aes(y = valor_raster, color = "valor_raster"), size = 0.7,na.rm = TRUE) +
  geom_line(aes(y = mean, color = "mean"), size =0.7, na.rm = TRUE)+#, linetype = "dashed") +
  geom_point(aes(y = valor_raster, color = "valor_raster"), size = 1,na.rm = TRUE) +
  geom_point(aes(y = mean, color = "mean"), size = 1, na.rm = TRUE)+#, linetype = "dashed") +
  
  
  #geom_line(aes(y = Registros.preliminares, color = "Registros.no.validados"), size = 0.3, na.rm = FALSE)+#, linetype = "dashed") +
  
  # Separar en subplots por estación
  facet_wrap(~ ID_archivo, scales = "free_y") +
  # 
  scale_y_continuous(limits = c(0, 120),breaks = seq(0, 120, by = 40)) +  # Ticks cada 10 en el eje Y
  
  # Títulos y etiquetas
  labs(title = "Vitacura Invierno_03-XGB_cv_M1-041024",
       x = "Date",
       y = "PM2.5",
       color = "Variables") +
  # Cambiar los colores de las líneas
  scale_color_manual(
    values = c("valor_raster" = "#2ca25f", "mean" = "#feb24c"),
    labels = c("valor_raster" = "Modelo", "mean" = "LCS")
  ) + 
  # Personalización del tema
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
VitacuraInvierno_date_plot
ggsave("grafico_VitacuraInvierno.png", plot = VitacuraInvierno_date_plot, width = 11, height = 6)


####################
# Vitacura Invierno
merged_df_subt_VitacuraVerano <- merged_df_subt[merged_df_subt$archivo== "vitacura verano", ]


unique(merged_df_subt_VitacuraVerano$estacion)
# Crear una secuencia completa de fechas desde la mínima hasta la máxima
merged_df_subt_VitacuraVerano$date <- as.Date(merged_df_subt_VitacuraVerano$date)
fechas_completas_VitacuraVerano <- data.frame(date = seq(as.Date("2021-01-01"), as.Date("2021-02-28"), by = "day"))
VitacuraVerano_date <- merge(fechas_completas_VitacuraVerano, merged_df_subt_VitacuraVerano, by = c( "date"), all.x = TRUE)
VitacuraVerano_date$ID_archivo

VitacuraVerano_date_plot <- ggplot(VitacuraVerano_date, aes(x = date)) +
  # Línea para Registros.validados
  geom_line(aes(y = valor_raster, color = "valor_raster"), size = 0.7,na.rm = TRUE) +
  geom_line(aes(y = mean, color = "mean"), size =0.7, na.rm = TRUE)+#, linetype = "dashed") +
  geom_point(aes(y = valor_raster, color = "valor_raster"), size = 1,na.rm = TRUE) +
  geom_point(aes(y = mean, color = "mean"), size = 1, na.rm = TRUE)+#, linetype = "dashed") +
  
  
  #geom_line(aes(y = Registros.preliminares, color = "Registros.no.validados"), size = 0.3, na.rm = FALSE)+#, linetype = "dashed") +
  
  # Separar en subplots por estación
  facet_wrap(~ estacion, scales = "free_y") +
  # 
  scale_y_continuous(limits = c(0, 60),breaks = seq(0, 60, by = 20)) +  # Ticks cada 10 en el eje Y
  
  # Títulos y etiquetas
  labs(title = "Vitacura Verano_03-XGB_cv_M1-041024",
       x = "Date",
       y = "PM2.5",
       color = "Variables") +
  # Cambiar los colores de las líneas
  scale_color_manual(
    values = c("valor_raster" = "#2ca25f", "mean" = "#feb24c"),
    labels = c("valor_raster" = "Modelo", "mean" = "LCS")
  ) +
  # Personalización del tema
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
VitacuraVerano_date_plot
ggsave("grafico_VitacuraVerano.png", plot = VitacuraVerano_date_plot, width = 11, height = 6)

#########################################################################
#################################################################
#REGRESION LINEAL CON TODOS LOS SENSORES
# Mi valor de referencia es el modelo no las mediciones de LCS
merged_df_subt2 <- merged_df_subt[complete.cases(merged_df_subt),]
# Ajuste del modelo de regresión lineal
#modelo <- lm(valor_raster ~mean, data = merged_df_subt)
modelo <- lm( mean~valor_raster , data = merged_df_subt)
# Calculo de métricas de desempeño
R2 <- summary(modelo)$r.squared
RMSE <- sqrt(mean(residuals(modelo)^2))
# Bias <- mean(merged_df_subt$valor_raster - merged_df_subt$mean)
Bias <- mean(merged_df_subt$mean - merged_df_subt$valor_raster)
n <- nrow(merged_df_subt)

# Crear el gráfico con ggplot2
plot <- ggplot(merged_df_subt, aes(y = mean, x= valor_raster)) +
  geom_abline(slope = 1, intercept = 0,  color = "black", size = 0.5) +  # Línea 1:1
  
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 180),breaks = seq(0, 180, by = 50)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 180),breaks = seq(0, 180, by = 50)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "Monitoreo",
    y = "Modelo",
    title = "07-RF-ESP_cv_M6-110225-SP",
    subtitle = paste(
      "R2 =", round(R2, 3),
      "| RMSE =", round(RMSE, 2),
      "| Bias =", round(Bias, 2),
      "| n =", n
    )
  ) +
  theme_classic() #+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot
# Mostrar el gráfico
print(plot)
ggsave("Regresion_LCS-Modelo.png", plot = plot, width = 11, height = 6)




#######################################################################
###########################################################################

# media de los raster
dir <- "D:/Josefina/Proyectos/ProyectoChile/CH/modelos/Salidas/SalidasAnuales/Salida_03-XGB_cv_M1-041024/"
raster_file<-  raster(paste(dir,"promedio_anual_2023-03-XGB_cv_M1-041024.tif",sep=""))
raster_file<-  raster(paste(dir,"Promedio_anual_2024_01-07-XGB_cv_M1-041024.tif",sep=""))



media_pixeles <- cellStats(raster_file, stat = "mean", na.rm = TRUE)
media_pixeles
df <- data.frame(year = c(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023),
                 mean = c(24.8468,23.68629,23.92807, 23.68649,22.35271,22.06676,
                          23.63149,24.12955,23.37537))

# Crear el gráfico
ggplot(df, aes(x = year, y = mean)) +
  geom_line (color = "#99d8c9", size = 1) +   
  geom_point(color = "#2ca25f", size = 2) +
  scale_y_continuous(limits = c(20, 25)) +
  scale_x_continuous(breaks = df$year) +   
  labs(
    #title = "Promedio anual",
    x = "Año",
    y = "Media anual"
  ) +
  theme_classic()  

#########################################################################
#################################################################
#REGRESION LINEAL CON TODOS LOS SENSORES NO LCS

merged_df_subt <- merged_df_subt[complete.cases(merged_df_subt),]
# Ajuste del modelo de regresión lineal
modelo <- lm(valor_raster ~mean, data = merged_df_subt)
# Calculo de métricas de desempeño
R2 <- summary(modelo)$r.squared
RMSE <- sqrt(mean(residuals(modelo)^2))
Bias <- mean(merged_df_subt$valor_raster - merged_df_subt$mean)

n <- nrow(merged_df_subt)
max(merged_df_subt$mean)
max(merged_df_subt$valor_raster)
# Crear el gráfico con ggplot2
plot <- ggplot(merged_df_subt, aes(x = mean, y= valor_raster)) +
  geom_abline(slope = 1, intercept = 0,  color = "black", size = 0.5) +  # Línea 1:1
  
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE, linetype = "dashed") +  # Línea de regresión
  scale_y_continuous(limits = c(0, 120),breaks = seq(0, 120, by = 40)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 120),breaks = seq(0, 120, by = 40)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "Mediciones",
    y = "Modelo",
    title = "02-RF_cv_M1-080125_MD",
    subtitle = paste(
      "R2 =", round(R2, 3),
      "| RMSE =", round(RMSE, 2),
      "| Bias =", round(Bias, 2),
      "| n =", n
    )
  ) +
  theme_classic() +
  theme(
    # Ajuste de títulos y subtítulos
    plot.title = element_text(size = 11, face = "bold"),       # Título principal
    plot.subtitle = element_text(size = 10, face = "italic"), # Subtítulo
    # Ajuste de títulos de los ejes
    axis.title.x = element_text(size = 9), # Título del eje X
    axis.title.y = element_text(size = 9), # Título del eje Y
    # Ajuste de texto de los ticks de los ejes
    axis.text.x = element_text(size = 8, angle = 0, hjust = 1), # Ticks del eje X
    axis.text.y = element_text(size = 8)  # Ticks del eje Y
  )

plot
# Mostrar el gráfico
print(plot)
ggsave("Regresion_LCS-Modelo.png", plot = plot, width = 11, height = 6)



