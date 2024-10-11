Interestyear = 2015
data_estaciones <- read.csv("D:/Josefina/Proyectos/ProyectoChile/dataset/estaciones/diarios/PM25_tot.csv",colClasses = c("FECHA..YYMMDD."  = "character"))
data_estaciones$date <- as.POSIXct(as.character(data_estaciones$FECHA..YYMMDD.), format = "%y%m%d")
data_estaciones <- data_estaciones[year(data_estaciones$date) == Interestyear,]
data_estaciones <- data_estaciones[complete.cases(data_estaciones$Registros.validados),]

prediccion_modelo <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/Prediccion_2015/Salida/Salida_03-XGB_cv_M1-041024/salida_03-XGB_cv_M1-041024_prediccion_2015.csv")

prediccion_modelo <- prediccion_modelo[complete.cases(prediccion_modelo$valor_raster),]
prediccion_modelo$date <- as.POSIXct(as.character(prediccion_modelo$date), format = "%d/%m/%Y")


# Ejemplo: Hacer un merge entre puntos y valores_raster basado en dos columnas
data_merge <- prediccion_modelo %>%
  left_join(data_estaciones, by = c("date", "estacion")) #%>%  # Merge basado en dos columnas
data_pm <- data_merge[complete.cases(data_merge$Registros.validados),]
data_pm <- data_pm[complete.cases(data_pm$valor_raster),]
data_pm <- data_pm[data_pm$valor_raster>0,]

data_pm$date <- as.POSIXct(data_pm$date, format = "%d/%m/%Y")
# Paso 1: Crear una serie temporal entre 01-01-2015 y 31-12-2015
serie_fechas <- data.frame(date = seq(as.Date("2015-01-01"), as.Date("2015-12-31"), by = "day"))

# Paso 2: Asegurarse de que la columna 'date' en tu dataset original esté en formato Date


# Paso 3: Hacer un merge para unir la serie temporal con el dataset original
# Usamos all.x = TRUE para mantener todas las fechas de 'serie_fechas', incluso si no hay datos en 'data'
data_completo <- merge(data_pm, serie_fechas ,by = "date", all= TRUE)
#write.csv(data_completo, "D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/Prediccion_2015/Salida/Salida_03-XGB_cv_M1-041024/MERGE_salida_03-XGB_cv_M1-041024_prediccion_2015.csv")# Ver los primeros registros del nuevo dataset
data_completo <- read.csv("D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/Prediccion_2015/Salida/Salida_03-XGB_cv_M1-041024/MERGE_salida_03-XGB_cv_M1-041024_prediccion_2015.csv")
data_completo$date <- as.POSIXct(data_completo$date, format = "%d/%m/%Y %H:%M")
data_completo$date <- as.Date(data_completo$date)

head(data_completo)



#########################################
#Series temporales por estacion 

# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)
# 365 DIAS * 9 ESTACUIONES ==> 3285
# Supongamos que ya tienes un dataframe llamado 'data' con las columnas 'date', 'estacion', 'Registros.validados' y 'valor_Raster'

# Asegurarse de que 'estacion' sea un factor
data_completo$estacion <- as.factor(data_completo$estacion)

# Crear el gráfico
ggplot(data_completo, aes(x = date)) +
  # Línea para Registros.validados
  geom_line(aes(y = Registros.validados, color = "SINCA"), size = 0.5,na.rm = FALSE) +
  # Línea para valor_Raster
  geom_line(aes(y = valor_raster, color = "Modelo"), size = 0.5, na.rm = FALSE)+#, linetype = "dashed") +
  # Separar en subplots por estación
  facet_wrap(~ estacion, scales = "free_y") +
   # 
  scale_y_continuous(limits = c(0, 160),breaks = seq(0, 160, by = 40)) +  # Ticks cada 10 en el eje Y
  
  # Títulos y etiquetas
  labs(title = "Modelo Salida_03-XGB_cv_M1-041024",
       x = "Date",
       y = "PM2.5",
       color = "Variables") +
  # Cambiar los colores de las líneas
  scale_color_manual(values = c("SINCA" = "#ef3b2c", "Modelo" = "#3690c0")) +
  
  # Personalización del tema
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Guardar el gráfico en formato PNG
ggsave("grafico_estaciones.png", plot = plot, width = 10, height = 6, dpi = 300)


data_completo_2 <- data_completo[complete.cases(data_completo$valor_raster),]
data_completo_2 <- data_completo_2[complete.cases(data_completo_2$Registros.validados),]
data_completo_2 <- data_completo_2[complete.cases(data_completo_2$date),]
estacion <- "IND"
data_subst <- data_completo_2[data_completo_2$estacion == estacion,]
length(data_subst$valor_raster)
(length(data_subst$valor_raster)/365)*100
