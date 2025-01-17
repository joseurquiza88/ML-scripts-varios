
#Leer datos de BA Embajada


data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/BA/dataset/estaciones/proceed/embajada/PM25_Embajada.csv",na.strings = -999)
data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/BA/dataset/estaciones/proceed/ACUMAR/PM25_Acumar.csv")

names(data)
data$horaDia <- paste(data$date, data$hora, sep=" ")
data$horaDia <- as.POSIXct( strptime (data$horaDia, format = "%d/%m/%Y %H:%M:%S"))
data$date <- as.POSIXct( strptime (data$date, format = "%d/%m/%Y"))
data_comp <- data[complete.cases(data),]


# Agrupar por la columna 'estacion' y calcular las estad?sticas
df <- data_comp %>%
  group_by(date, estacion) %>%
  summarise(
    # Media diaria general
    mean = mean(PM25, na.rm = TRUE),
    # Media diaria entre 08:00 y 23:00
    # Mínimo
    min = min(PM25, na.rm = TRUE),
    # Máximo
    max = max(PM25, na.rm = TRUE),
    # Desviación estándar
    sd = sd(PM25, na.rm = TRUE),
    mean_horario = mean(PM25[hour(horaDia) >= 8 & hour(horaDia) <= 23], na.rm = TRUE)
    
  )

df <- df[df$mean !=0,]
df <- df[df$mean_horario !=0,]
df <- df[complete.cases(df$mean),]
df <- df[complete.cases(df$mean_horario),]


write.csv(df,"D:/Josefina/Proyectos/ProyectoChile/BA/dataset/estaciones/proceed/PM25_Embajada_process.csv")
write.csv(df,"D:/Josefina/Proyectos/ProyectoChile/BA/dataset/estaciones/proceed/PM25_ACUMAR_process.csv")

data_embajada <- read.csv("D:/Josefina/Proyectos/ProyectoChile/BA/dataset/estaciones/proceed/PM25_embajada_process.csv")
data_acumar <- read.csv("D:/Josefina/Proyectos/ProyectoChile/BA/dataset/estaciones/proceed/PM25_ACUMAR_process.csv")
data_acumar$date <- as.POSIXct( strptime (data_acumar$date, format = "%Y-%m-%d"))#"%d/%m/%Y"))
data_embajada$date <- as.POSIXct( strptime (data_embajada$date, format = "%Y-%m-%d"))
data_embajada$mean_horario <- data_embajada$mean
df_estaciones <- rbind(data_embajada,data_acumar)

data_comp <- df_estaciones[complete.cases(df_estaciones$mean),]
ggplot(data_comp, aes(x = date)) +
# Línea para Registros.validados
geom_line(aes(y = mean, color = "mean"), size = 0.3,na.rm = TRUE) +

# Separar en subplots por estación
facet_wrap(~ data_comp$estacion, scales = "free_y") +
# 
#scale_y_continuous(limits = c(0,100),breaks = seq(0, 100, by = 20)) +  # Ticks cada 10 en el eje Y

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


#####
ggplot(data_comp, aes(x = date)) +
  # Línea para Registros.validados
  geom_line(aes(y = PM25, color = "mean"), size = 0.3,na.rm = TRUE) +
  
  # Separar en subplots por estación
  #facet_wrap(~ data_comp$estacion, scales = "free_y") +
  # 
  #scale_y_continuous(limits = c(0,100),breaks = seq(0, 100, by = 20)) +  # Ticks cada 10 en el eje Y
  
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
