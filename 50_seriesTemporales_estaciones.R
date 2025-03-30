
estacion <- "CH"
data_sensores <- read.csv(paste("D:/Josefina/Proyectos/ProyectoChile/",estacion,"/proceed/06_estaciones/",estacion,"_estaciones.csv",sep=""))
data_sensores$date <- as.POSIXct(data_sensores$date, format = "%d/%m/%Y")
data_sensores<-data_sensores[year(data_sensores$date) >=2015,]
data_sensores<- data_sensores[complete.cases(data_sensores$Registros.completos),]
data_sensores<- data_sensores[data_sensores$Registros.completos !=0,]
data_sensores$mean <- data_sensores$Registros.completos
length(unique(data_sensores$estacion))
ggplot(data_sensores, aes(x = date)) +
  geom_line(aes(y = mean, color = "Monitoreo"), size = 0.3,na.rm = FALSE) +
  facet_wrap(~ estacion, scales = "free_y") +
  # 
  scale_y_continuous(limits = c(0, 200),breaks = seq(0, 200, by = 50)) +  # Ticks cada 10 en el eje Y
  
  # Títulos y etiquetas
  labs(#title = "Estaciones de monitoreo",
       x = "Date",
       y = "SINCA PM2.5",
       color = "Variables") +
  # Cambiar los colores de las líneas
  scale_color_manual(values = c("Monitoreo" = "#2ca25f"))+#, "mean"="Monitoreo")) +
  
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

summary(data_sensores$mean)
