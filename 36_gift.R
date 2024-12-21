library(dplyr)


# Especifica la ruta de los archivos TIFF
ruta_archivos <- "D:/Josefina/Proyectos/ProyectoChile/modelos/Salidas/SalidasMensuales/Salida_03-XGB_cv_M1-041024/2015/"
meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
           "Julio")#, "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
#archivos_tiff <- paste0(ruta_archivos, "PM25_", 2015, "_", sprintf("%02d", 1:12), ".tif")
setwd(ruta_archivos)
lista_raster <- list.files(pattern = "*.tif")
# Crear una lista vacía para almacenar los datos
datos_pm25 <- list()



# Definir los colores y los límites
colores <- c("#2b83ba", "#4696b6", "#61a9b1", "#7cbcac","#97cfa8","#b0dfa6","#c1e6ab",
             "#d3eeb1", "#e5f5b7", "#f7fcbc", "#fff7b5","#ffe6a1","#fed58e","#fec47a",
             "#feb266", "#f79756", "#ef7747","#e75839", "#df382a","#d7191c" )
rango_pm25 <- c("0-5", "6-10", "11-15", "16-20","21-25","26-30","31-35","36-40","41-45","46-50",
                "51-55","56-60","61-65","66-70","71-75","76-80","81-85","86-90","91-95",">95")

# Crear la lista de nombres de meses en orden
meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
           "Julio")#, "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

# Cargar cada TIFF y clasificar los valores de PM2.5
for (i in 1:length(lista_raster)) {
  raster_data <- raster(lista_raster[i])
  df <- as.data.frame(raster_data, xy = TRUE)
  colnames(df) <- c("x", "y", "PM25")
  df$Mes <- factor(meses[i], levels = meses)  # Convertir Mes en factor con orden
  
  # Crear una columna de rango de PM2.5 para asignar colores
  df <- df %>%
    mutate(Rango_PM25 = cut(PM25, 
                            breaks = c(0, 5, 10, 15, 20,25,30,35,40,45,
                                       50,55,60,65,70,75,80,85,90,95,Inf), 
                            labels = rango_pm25,
                            right = FALSE))
  datos_pm25[[i]] <- df
}

# Combinar todos los dataframes en uno solo
datos_pm25 <- do.call(rbind, datos_pm25)


# Cargar el shapefile de las comunas
comunas <- st_read("D:/Josefina/Proyectos/ProyectoChile/shape/Comunas/Comunas_cortados.shp")
estaciones <- st_read("D:/Josefina/Proyectos/ProyectoChile/shape/Estaciones/estaciones.shp")

# Crear el gráfico base con colores personalizados
p <- ggplot() +
  geom_raster(data = datos_pm25, aes(x = x, y = y, fill = Rango_PM25)) +
  
  scale_fill_manual(values = colores, name = "PM2.5") +
  geom_sf(data = comunas, color = "black", fill = NA) + 
  geom_sf(data = estaciones, color = "black", fill = NA) +
  
  labs(title = "Concentración de PM2.5 - {closest_state} - 2024") +
  theme_minimal() +
  transition_states(Mes, transition_length = 0, state_length = 1) +
  enter_fade() + exit_fade()

# Guardar el GIF
#anim_save("concentracion_pm25_2015.gif", animate(p, duration = 10, width = 800, height = 600))

anim_save("concentracion_pm25_2024.gif", 
          animate(p, duration = 10, width = 800, height = 600,#, nframes = 12, fps = 1, 
                  renderer = gifski_renderer(loop = FALSE)))

