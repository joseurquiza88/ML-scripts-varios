


#### Analisis SINCA
data_estaciones <- read.csv("D:/Josefina/Proyectos/ProyectoChile/dataset/estaciones/diarios/PM25_tot.csv",colClasses = c("FECHA..YYMMDD."  = "character"))
data_estaciones$date <- as.POSIXct(as.character(data_estaciones$FECHA..YYMMDD.), format = "%y%m%d")
data_estaciones <- data_estaciones[year(data_estaciones$date) >= 2015, ]
data_estaciones <- data_estaciones[!is.na(data_estaciones$estacion),]

ggplot(data_estaciones, aes(x = date)) +
  # Línea para Registros.validados
  geom_line(aes(y = Registros.validados, color = "Registros.validados"), size = 0.3,na.rm = FALSE) +
  # Línea para valor_Raster
  geom_line(aes(y = Registros.preliminares, color = "Registros.preliminares"), size = 0.3, na.rm = FALSE)+#, linetype = "dashed") +
  geom_line(aes(y = Registros.preliminares, color = "Registros.no.validados"), size = 0.3, na.rm = FALSE)+#, linetype = "dashed") +
  
  # Separar en subplots por estación
  facet_wrap(~ estacion, scales = "free_y") +
  # 
  scale_y_continuous(limits = c(0, 200),breaks = seq(0, 200, by = 40)) +  # Ticks cada 10 en el eje Y
  
  # Títulos y etiquetas
  labs(#title = "Modelo Salida_03-XGB_cv_M1-041024",
       x = "Date",
       y = "PM2.5",
       color = "Variables") +
  # Cambiar los colores de las líneas
  scale_color_manual(values = c("Registros.validados" = "#2ca25f", "Registros.preliminares" = "#feb24c", "Registros.no.validados" = "#ef3b2c")) +
  
  # Personalización del tema
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Guardar el gráfico en formato PNG
ggsave("grafico_estaciones.png", plot = plot, width = 10, height = 6, dpi = 300)

##############################################################
# Datos completos
ggplot(data_estaciones, aes(x = date)) +
  # Línea para Registros.validados
  geom_line(aes(y = Registros.completos, color = "Registros.completos"), size = 0.3,na.rm = FALSE) +
  
  # Separar en subplots por estación
  facet_wrap(~ estacion, scales = "free_y") +
  # 
  scale_y_continuous(limits = c(0, 200),breaks = seq(0, 200, by = 40)) +  # Ticks cada 10 en el eje Y
  
  # Títulos y etiquetas
  labs(#title = "Modelo Salida_03-XGB_cv_M1-041024",
    x = "Date",
    y = "PM2.5",
    color = "Variables") +
  # Cambiar los colores de las líneas
  scale_color_manual(values = c("Registros.completos" = "#6a51a3")) +
  
  # Personalización del tema
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Metricas de los datos por estacion

data_estaciones %>%
  dplyr::group_by(estacion) %>%  
  dplyr::group_split() -> dat_agrupado
df_rbind <- data.frame()
for (i in 1:length(dat_agrupado)){
df <- data.frame(estacion = dat_agrupado[[i]][["estacion"]][1],
  mean_Registros.validados = mean(dat_agrupado[[i]][["Registros.validados"]],na.rm=TRUE),
                  min_Registros.validados = min(dat_agrupado[[i]][["Registros.validados"]],na.rm=TRUE),
                  max_Registros.validados = max (dat_agrupado[[i]][["Registros.validados"]],na.rm=TRUE),
                  median_Registros.validados = median(dat_agrupado[[i]][["Registros.validados"]],na.rm=TRUE),
  
  mean_Registros.preliminares = mean(dat_agrupado[[i]][["Registros.preliminares"]],na.rm=TRUE),
  min_Registros.preliminares = min(dat_agrupado[[i]][["Registros.preliminares"]],na.rm=TRUE),
  max_Registros.preliminares = max (dat_agrupado[[i]][["Registros.preliminares"]],na.rm=TRUE),
  median_Registros.preliminares = median(dat_agrupado[[i]][["Registros.preliminares"]],na.rm=TRUE),

  mean_Registros.no.validados = mean(dat_agrupado[[i]][["Registros.no.validados"]],na.rm=TRUE),
  min_Registros.no.validados = min(dat_agrupado[[i]][["Registros.no.validados"]],na.rm=TRUE),
  max_Registros.no.validados = max (dat_agrupado[[i]][["Registros.no.validados"]],na.rm=TRUE),
  median_Registros.no.validados = median(dat_agrupado[[i]][["Registros.no.validados"]],na.rm=TRUE),
  
  mean_Registros.completos = mean(dat_agrupado[[i]][["Registros.completos"]],na.rm=TRUE),
  min_Registros.completos = min(dat_agrupado[[i]][["Registros.completos"]],na.rm=TRUE),
  max_Registros.completos = max (dat_agrupado[[i]][["Registros.completos"]],na.rm=TRUE),
  median_Registros.completos = median(dat_agrupado[[i]][["Registros.completos"]],na.rm=TRUE))

df_rbind <- rbind(df_rbind, df)
}
# resumen_estaciones <- data_estaciones %>%
#   group_by(estacion)## %>%  # agrupar por estación
#   summarise(
#   
#    min_validados = min(Registros.validados, na.rm = TRUE),
#     max_validados = max(Registros.validados, na.rm = TRUE),
#     media_validados = mean(Registros.validados, na.rm = TRUE),
#     mediana_validados = median(Registros.validados, na.rm = TRUE),
#     
#     min_preliminares = min(Registros.preliminares, na.rm = TRUE),
#     max_preliminares = max(Registros.preliminares, na.rm = TRUE),
#     media_preliminares = mean(Registros.preliminares, na.rm = TRUE),
#     mediana_preliminares = median(Registros.preliminares, na.rm = TRUE),
#     
#     min_no_validados = min(Registros.no.validados, na.rm = TRUE),
#     max_no_validados = max(Registros.no.validados, na.rm = TRUE),
#     media_no_validados = mean(Registros.no.validados, na.rm = TRUE),
#     mediana_no_validados = median(Registros.no.validados, na.rm = TRUE),
#     
#     min_totales = min(Registros.completos, na.rm = TRUE),
#     max_totales = max(Registros.completos, na.rm = TRUE),
#     media_totales = mean(Registros.completos, na.rm = TRUE),
#     mediana_totales = median(Registros.completos, na.rm = TRUE)
#   )
# 
# # Mostrar el resumen
# print(resumen_estaciones)

write.csv(df_rbind, "metricas.csv")

#################################################################
# Datos completos
ggplot(data_estaciones) +
  # Línea para Registros.validados
  geom_histogram(binwidth = 10,aes(x = Registros.completos), fill = "#bcbddc",color = "#54278f" , size = 0.3,na.rm = FALSE) +
  
  # Separar en subplots por estación
  facet_wrap(~ estacion, scales = "free_y") +
  # 
  scale_y_continuous(limits = c(0, 1500),breaks = seq(0, 1500, by = 500)) +  # Ticks cada 10 en el eje Y
  scale_x_continuous(limits = c(0, 200),breaks = seq(0, 200, by = 20)) +  # Ticks cada 10 en el eje Y
  
  # Títulos y etiquetas
  labs(#title = "Modelo Salida_03-XGB_cv_M1-041024",
    #x = "Date",
    x = "PM2.5",
    color = "Variables") +
  # Cambiar los colores de las líneas
  #scale_fill_manual(values = c("Registros.completos" = "#6a51a3")) +
  
  # Personalización del tema
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

