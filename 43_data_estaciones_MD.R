
direccion <- "D:/Josefina/Proyectos/ProyectoChile/MD/dataset/estaciones/"
estacion <- "ID 28 V02"
setwd(paste(direccion,estacion, sep =""))
id <- list.files(path = getwd(),
                 pattern = "*.csv",
                 full.names = FALSE)
i <-1
df_rbind <- data.frame()
nombres <- names(read.csv(id[1], na.strings = "-9999"))
for (i in 1:length(id)) {
    print(i)
    data <- read.csv(id[i], na.strings = -9999)
    data_names <- names(data)
    
  if (identical(nombres, data_names)){
      df_rbind <- rbind(df_rbind, data)
    } else {
      stop(paste("Error: Los nombres no coinciden en el archivo", id[i]))
    }
}

###### ----- CALIDAD DATOS -------
##3https://siata.gov.co/descarga_siata/index.php/info/aire/
# Valor Flag Calidad del dato
# 1 Dato v´alido
# -1 Dato v´alido por el operado anterior
# 1.8 – 2.5 Dato dudoso
# 2.6 – 3.9 Dato malo
# >= 4.0 Dato faltante
# Dato -9999 y calidad (flag) 1 Equipo fuera de operaci´on

df_rbind_calidad <- df_rbind[df_rbind$calidad_pm25<2.6,]
df_rbind_calidad <- df_rbind_calidad[df_rbind_calidad$pm25!= -9999,]
# df_rbind_calidad$dateH <- as.POSIXct( strptime (df_rbind_calidad$X, format = "%Y-%m-%d %H:%M:%S"))
df_rbind_calidad$dateH <- as.POSIXct( strptime (df_rbind_calidad$Fecha_Hora, format = "%Y-%m-%d %H:%M:%S"))
df_rbind_calidad$date <- as.Date(df_rbind_calidad$dateH)
names(df_rbind_calidad)
df <- df_rbind_calidad %>%
  group_by(date) %>%
  summarise( 
    # Primer valor de unidad
    mean = mean(pm25, na.rm = TRUE),               # Media
    min = min(pm25, na.rm = TRUE),                 # M?nimo
    max = max(pm25, na.rm = TRUE),                 # M?ximo
    sd = sd(pm25, na.rm = TRUE)
    # Desviaci?n est?ndar
    #,                         # Conteo de valores no NA
    #mean_subt = mean(data_subt$valor[data_subt$estacion == unique(estacion)], na.rm = TRUE) # Media para horas espec?ficas
  )
View(df)
df$ID <- df_rbind_calidad$codigoSerial[1]
names <- paste(direccion,"/mediasDiarias/mediaDiaria_",estacion,".csv",sep="")
write.csv(df,names)

##########################################################
##########################################################
direccion <- "D:/Josefina/Proyectos/ProyectoChile/MD/dataset/estaciones/mediasDiarias/"

setwd(direccion)
id <- list.files(path = getwd(),
                 pattern = "*.csv",
                 full.names = FALSE)
i <-1
df_rbind <- data.frame()

for (i in 1:length(id)) {
  print(i)
  data <- read.csv(id[i])
  data_names <- names(data)
  df_rbind <- rbind(df_rbind, data)

}

class(df_rbind$date)
df_rbind$date <- as.POSIXct( strptime (df_rbind$date, format = "%Y-%m-%d"))

write.csv(df_rbind,"D:/Josefina/Proyectos/ProyectoChile/MD/proceed/06_estaciones/MD_estaciones.csv")

df_rbind<- read.csv("D:/Josefina/Proyectos/ProyectoChile/MD/proceed/06_estaciones/MD_estaciones.csv")
df_rbind$date <- as.POSIXct(strptime(df_rbind$date, format = "%d/%m/%Y"))
ggplot(df_rbind, aes(x = date)) +
  # Línea para Registros.validados
  geom_line(aes(y = mean, color = "mean"), size = 0.3,na.rm = TRUE) +
  
  # Separar en subplots por estación
  facet_wrap(~ df_rbind$estacion, scales = "free_y") +
  # 
  scale_y_continuous(limits = c(0,120),breaks = seq(0, 120, by = 40)) +  # Ticks cada 10 en el eje Y
  
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


## Cuantos dias superan los limites de la OMS 15 ug/m3
#menor a 15
df_rbind_menor15 <- df_rbind[df_rbind$mean < 15,]
df_rbind_mayor15 <- df_rbind[df_rbind$mean >= 15,]

#Que estaciones superaron los limites
length(unique(df_rbind_mayor15$estacion))
# Cuantos dias al año se superaron las directrices
filtro_year_min <- df_rbind_menor15[year(df_rbind_menor15$date) == 2020,]
filtro_year <- df_rbind_mayor15[year(df_rbind_mayor15$date) == 2024,]
length(unique(filtro_year$date))
2020 334
#################################################################################
#################################################################################
#################################################################################
# Metricas por estacion

library(dplyr)

# Agrupar por estación y calcular métricas
estadisticas_PM25 <- df_rbind %>%
  group_by(estacion) %>%
  summarise(
    media = round(mean(mean, na.rm = TRUE),2),
    mediana = round(median(mean, na.rm = TRUE),2),
    minimo =round(min(mean, na.rm = TRUE),2),
    maximo =round( max(mean, na.rm = TRUE),2),
    sd =round( sd(mean, na.rm = TRUE),2),
    varianza =round( var(mean, na.rm = TRUE),2),
    q25 = round(quantile(mean, 0.25, na.rm = TRUE),2),
    q75 =round( quantile(mean, 0.75, na.rm = TRUE),2),
    n = n()
  )

print(estadisticas_PM25)

#################################################################################
#################################################################################
#################################################################################
# Promedios mensuales por estacion

library(dplyr)
library(lubridate)

# Convertir la columna date a formato fecha si no lo está
df$date <- as.Date(df$date)

# Agrupar por estación y mes, y calcular el promedio
promedio_mensual <- df_rbind %>%
  mutate(YearMonth = floor_date(date, "month")) %>%  # Extraer el año y mes
  group_by(estacion, YearMonth) %>%
  summarise(mean = mean(mean, na.rm = TRUE)) %>%
  ungroup()
unique(promedio_mensual$estacion)

promedio_mensual_subt_01 <- promedio_mensual[promedio_mensual$estacion == "Medellin - Santa Elena"   |
                                               
                                               promedio_mensual$estacion ==   "Medellin, Altavista - I.E. Pedro Octavio Amado"  |
                                             promedio_mensual$estacion == "Medellin, Aranjuez - I.E Ciro Mendia"|
                                             promedio_mensual$estacion =="Medellin, Belen - I.E Pedro Justo Berrio" |
                                             promedio_mensual$estacion =="Medellin, El Poblado - I.E INEM sede Santa Catalina" |
                                             promedio_mensual$estacion =="Medellin, San Cristobal - Parque Biblioteca Fernando Botero"|
                                             promedio_mensual$estacion == "Medellin, Villahermosa - Planta de produccion de agua potable EPM"|
                                             promedio_mensual$estacion == "Sabaneta - I.E. Rafael J. Mejia",]
unique(promedio_mensual_subt_01$estacion)

promedio_mensual_subt_02 <- promedio_mensual[promedio_mensual$estacion == "Barbosa - Torre Social"  |
                                               promedio_mensual$estacion == "Bello - I.E. Fernando Velez" |
                                               promedio_mensual$estacion ==   "Caldas - E U Joaquin Aristizabal" |
                                               promedio_mensual$estacion == "Copacabana - Ciudadela Educativa La Vida"|
                                               promedio_mensual$estacion =="Envigado - E.S.E. Santa Gertrudis"|
                                               promedio_mensual$estacion =="Estacion Trafico Centro"|
                                               promedio_mensual$estacion =="Itagui - Casa de Justicia Itagui"|
                                               promedio_mensual$estacion == "Itagui - I.E. Concejo Municipal de Itagui"|
                                               promedio_mensual$estacion == "La Estrella - Hospital",]

nrow(promedio_mensual_subt_02)+nrow(promedio_mensual_subt_01) == nrow(promedio_mensual)
# Ver el resultado
print(promedio_mensual)
# ggplot(promedio_mensual_subt_01, aes(x = YearMonth)) +
  ggplot(promedio_mensual_subt_02, aes(x = YearMonth)) +
  # Línea para Registros.validados
  geom_line(aes(y = mean, color = "mean"), size = 0.3, na.rm = TRUE) +
  
  # Separar en subplots por estación
  facet_wrap(~ estacion, scales = "free_y") +
  
  # Escala del eje Y
  scale_y_continuous(limits = c(0, 120), breaks = seq(0, 120, by = 40)) +
  
  # Títulos y etiquetas
  labs(
    x = "Date",
    y = "PM2.5",
    color = "Variables"
  ) +
  
  # Cambiar los colores de las líneas
  scale_color_manual(values = c("mean" = "#2ca25f")) +
  
  # Personalización del tema con reducción de tamaño de texto
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 5, angle = 45, hjust = 1),  # Reducir tamaño del texto en eje X
    axis.text.y = element_text(size = 5),  # Reducir tamaño del texto en eje Y
    strip.text = element_text(size = 5),  # Reducir tamaño de los títulos de los subplots
    legend.text = element_text(size = 5),  # Reducir tamaño del texto de la leyenda
    legend.title = element_text(size = 5)  # Reducir tamaño del título de la leyenda
  )


