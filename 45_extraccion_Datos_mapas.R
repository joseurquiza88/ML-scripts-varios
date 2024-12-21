##############################################################################
##otra forma de extraer los datos
estacion <- "CH"
dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/Salidas/SalidasDiarias/Salida_03-XGB_cv_M1-041024/",sep="")



setwd(dir)
id <- list.files(path = dir,
                 pattern = "*.tif",
                 full.names = FALSE)

#data_estacciones <- read.csv("D:/Josefina/Proyectos/ProyectoChile/SP/dataset/sitios.csv")
data_estacciones <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/dataset/estaciones/sitios_",estacion,".csv",sep=""))

data_estacciones <- data_estacciones[data_estacciones$tipo == "LCS",]
#data_estacciones <- data_estacciones[22:24,]
puntos <- data_estacciones




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


write.csv(df_rbind, "Salida_03-XGB_cv_M1-041024_LCS-Sensor.csv")

###############################################################
# Merge datos sensores
estacion <- "CH"
dir <- paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/modelos/Salidas/SalidasDiarias/Salida_03-XGB_cv_M1-041024/",sep="")

data_sensores <- read.csv("D:/Josefina/Proyectos/Sensores/proceed/data/media_diria_sensores.csv")
data_modelo <- read.csv( paste(dir,"Salida_03-XGB_cv_M1-041024_LCS-Sensor.csv",sep=""))
data_sensores <- data_sensores[complete.cases(data_sensores$date),]
data_sensores$date <- as.POSIXct(data_sensores$date, format = "%d/%m/%Y")
data_modelo$date <- as.POSIXct(data_modelo$date, format = "%Y-%m-%d")

names(data_modelo)
names(data_sensores)
data_sensores$ID_archivo <- data_sensores$ID_archivo_num
merged_df <- merge(data_modelo, data_sensores, by = c("ID_archivo", "date","archivo"), all.x = TRUE)
merged_df_subt <- merged_df[complete.cases(merged_df),]
write.csv(merged_df_subt,"merged_df_subt.csv")
length(unique(merged_df_subt$estacion))
# Filtrar solo las estaciones que están en el vector l
merged_df_subt_2 <- merged_df_subt[merged_df_subt$estacion %in% unique(merged_df_subt$estacion)[1:9], ]

merged_df_subt_2$date <- as.POSIXct(merged_df_subt_2$date, format = "%Y-%m-%d")
merged_df_subt_2$date <- as.Date(merged_df_subt_2$date)

ggplot(merged_df_subt_2, aes(x = date)) +
  # Línea para Registros.validados
  geom_line(aes(y = valor_raster, color = "valor_raster"), size = 0.3,na.rm = FALSE) +
  # Línea para valor_Raster
  geom_line(aes(y = mean, color = "mean"), size = 0.3, na.rm = FALSE)+#, linetype = "dashed") +
  #geom_line(aes(y = Registros.preliminares, color = "Registros.no.validados"), size = 0.3, na.rm = FALSE)+#, linetype = "dashed") +
  
  # Separar en subplots por estación
  facet_wrap(~ estacion, scales = "free_y") +
  # 
  scale_y_continuous(limits = c(0, 180),breaks = seq(0, 180, by = 40)) +  # Ticks cada 10 en el eje Y
  
  # Títulos y etiquetas
  labs(#title = "Modelo Salida_03-XGB_cv_M1-041024",
    x = "Date",
    y = "PM2.5",
    color = "Variables") +
  # Cambiar los colores de las líneas
  scale_color_manual(values = c("valor_raster" = "#2ca25f", "mean" = "#feb24c")) +
  
  # Personalización del tema
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



##########################################################
### Ploteamos solo algunos
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
print(df_completo)
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
  scale_color_manual(values = c("valor_raster" = "#2ca25f", "mean" = "#feb24c")) +
  
  # Personalización del tema
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
maipuInv_date_plot
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
  scale_color_manual(values = c("valor_raster" = "#2ca25f", "mean" = "#feb24c")) +
  
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
merged_df_subt_florida <- merged_df_subt[merged_df_subt$archivo== "Puntos La Florida Verano e Invierno Puntos Muestreo La Florida", ]


unique(merged_df_subt_florida$estacion)
# Crear una secuencia completa de fechas desde la mínima hasta la máxima
merged_df_subt_florida$date <- as.Date(merged_df_subt_florida$date)
fechas_completas_Florida <- data.frame(date = seq(as.Date("2021-06-01"), as.Date("2021-07-31"), by = "day"))
Florida_date <- merge(fechas_completas_Florida, merged_df_subt_florida, by = c( "date"), all.x = TRUE)


florida_date_plot <- ggplot(Florida_date, aes(x = date)) +
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
  labs(title = "La Florida_03-XGB_cv_M1-041024",
       x = "Date",
       y = "PM2.5",
       color = "Variables") +
  # Cambiar los colores de las líneas
  scale_color_manual(values = c("valor_raster" = "#2ca25f", "mean" = "#feb24c")) +
  
  # Personalización del tema
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
florida_date_plot
ggsave("grafico_Florida.png", plot = florida_date_plot, width = 11, height = 6)

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
  scale_color_manual(values = c("valor_raster" = "#2ca25f", "mean" = "#feb24c")) +
  
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
  scale_color_manual(values = c("valor_raster" = "#2ca25f", "mean" = "#feb24c")) +
  
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
  scale_color_manual(values = c("valor_raster" = "#2ca25f", "mean" = "#feb24c")) +
  
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
  scale_color_manual(values = c("valor_raster" = "#2ca25f", "mean" = "#feb24c")) +
  
  # Personalización del tema
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
VitacuraVerano_date_plot
ggsave("grafico_VitacuraVerano.png", plot = VitacuraVerano_date_plot, width = 11, height = 6)

#########################################################################
#################################################################
#REGRESION LINEAL CON TODOS LOS SENSORES
# Mi valor de referencia es el modelo no las mediciones de LCS
merged_df_subt <- merged_df_subt[complete.cases(merged_df_subt),]
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
  scale_y_continuous(limits = c(0, 200),breaks = seq(0, 200, by = 50)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 200),breaks = seq(0, 200, by = 50)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "Modelo",
    y = "LCS",
    title = "LCS - Modelo 03-XGB_cv_M1-041024",
    subtitle = paste(
      "R2 =", round(R2, 3),
      "| RMSE =", round(RMSE, 2),
      "| Bias =", round(Bias, 2),
      "| n =", n
    )
  ) +
  theme_classic() +
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

