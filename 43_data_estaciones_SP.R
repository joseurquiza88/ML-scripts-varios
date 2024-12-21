
#Leer datos de SP

data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/SP/dataset/estaciones/originales/all_stations_mp25.csv",fileEncoding = "UTF-8")

data$dateHour <- as.POSIXct( strptime (data$datetime, format = "%Y-%m-%d %H:%M:%S"))
data$date <- as.Date(data$datetime, format = "%Y-%m-%d %H:%M:%S")



# Agrupar por la columna 'estacion' y calcular las estad?sticas
df <- data %>%
  group_by(date,name) %>%
  summarise(                           # Primer valor de unidad
    mean = mean(val, na.rm = TRUE),               # Media
    min = min(val, na.rm = TRUE),                 # M?nimo
    max = max(val, na.rm = TRUE),                 # M?ximo
    sd = sd(val, na.rm = TRUE),                   # Desviaci?n est?ndar
    num = nrow(val)#,                         # Conteo de valores no NA
    #mean_subt = mean(data_subt$valor[data_subt$estacion == unique(estacion)], na.rm = TRUE) # Media para horas espec?ficas
  )

# Validacion
unique(df$name)
df_subt <- df[df$name == "Parque D.Pedro II",]
df_subt_date <- df_subt[df_subt$date == "2023-12-20",] 

data_subt <- data[data$name == "Parque D.Pedro II",]
data_subt_date <- data_subt[data_subt$date == "2023-12-20",] 
data_subt_date_hour <- data_subt_date[0:24,]
mean(data_subt_date_hour$val)
df_subt_date$mean

# Uniion con dataframe generado

data_estaciones <- read.csv("D:/Josefina/Proyectos/ProyectoChile/SP/dataset/sitios.csv")#,fileEncoding = "UTF-8")
data_estaciones$estacion <- iconv(data_estaciones$Nombre.sitio, from = "latin1", to = "UTF-8")

data_estaciones <- data_estaciones[data_estaciones$Considerado == "SI",]
puntos <- data_estaciones
#puntos$estacion <- puntos$Nombre.sitio 
df$estacion <- df$name 
#df$estacion <- df[df$name == "Cid.Universit?ria-USP-Ipen",]
df$estacion[df$name == "Cid.Universitária-USP-Ipen"] <- "Ciudad Universitaria - USP"
df$estacion[df$name == "Graja?-Parelheiros" ] <- "Grajau - Parelheiros"
df$estacion[df$name == "Guarulhos-Pimentas" ] <- "Guarulhos - Pimentas"
df$estacion[df$name == "Marg.Tietê-Pte Remédios" ] <- "Marginal Tietê - Ponte dos Remédios"
df$estacion[df$name == "S.Bernardo-Centro"  ] <- "S?o Bernardo - Centro"
df$estacion[df$name == "Mooca"  ] <- "Mo?ca"
df_merged <- merge(puntos, df, by = "estacion")
length(unique(df_merged$estacion))
unique(df$estacion)
unique(puntos$estacion)

unique(df$estacion)
unique(puntos$estacion)
unique(df_merged$estacion)

df_estaciones <- data.frame(date=df_merged$date,ID = df_merged$ID,
                            estacion = df_merged$estacion,mean = df_merged$mean,
                            min = df_merged$min,max = df_merged$max, sd =df_merged$sd)
View(df_estaciones)  


df_estaciones_subt <- df_estaciones[year(df_estaciones$date)>=2015,]

write.csv(df_estaciones_subt,"D:/Josefina/Proyectos/ProyectoChile/SP/proceed/06_estaciones/SP_estaciones.csv")
ggplot(df_estaciones_subt, aes(x = date)) +
  # Línea para Registros.validados
  geom_line(aes(y = mean, color = "mean"), size = 0.3,na.rm = FALSE) +
  
  # Separar en subplots por estación
  facet_wrap(~ estacion, scales = "free_y") +
  # 
  scale_y_continuous(limits = c(0,100),breaks = seq(0, 100, by = 20)) +  # Ticks cada 10 en el eje Y
  
  # Títulos y etiquetas
  labs(#title = "Modelo Salida_03-XGB_cv_M1-041024",
    x = "Date",
    y = "PM2.5",
    color = "Variables") +
  # Cambiar los colores de las líneas
  scale_color_manual(values = c("mean" = "#2ca25f")) +
  
  # Personalización del tema
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Falta en el df generado manualmente viendo cetesb
# 4
#  "Parque D.Pedro II, "Guarulhos-Pa?o Municipal", "Osasco", "S?o Caetano do Sul" 

df_ver <- df[df$estacion =="Parque D.Pedro II" ,]
df_ver <- df[df$estacion =="Guarulhos-Pa?o Municipal" ,]
df_ver <- df[df$estacion =="Osasco" ,]
df_ver <- df[df$estacion =="S?o Caetano do Sul" ,]

  