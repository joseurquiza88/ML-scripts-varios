mes <- "12"
setwd("D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/Prediccion_2019/tiff/")
promedio_mensual_062015 <- raster(paste("D:/Josefina/Proyectos/ProyectoChile/modelos/Salidas/SalidasMensuales/Salida_03-XGB_cv_M1-041024/promedio_mensual_",mes,"-2015-03-XGB_cv_M1-041024.tif",sep=""))
year <- 2023
for (i in 1:1){

  promedio_mensual_062015 <- raster(paste("D:/Josefina/Proyectos/ProyectoChile/modelos/Salidas/SalidasAnuales/Salida_03-XGB_cv_M1-041024/promedio_anual_",year,"-03-XGB_cv_M1-041024.tif",sep=""))
  
  prueba_puntos <- read.csv("prueba_puntos.csv")
  DEM_raster <- raster("03_DEM/DEM_raster.tif")
  
  # Convertir el dataframe a un objeto sf de puntos espaciales
  puntos_sf <- st_as_sf(prueba_puntos, coords = c("x", "y"), crs = 4326) 
  
  
  # # Crear una línea entre las coordenadas de inicio y fin
  # corte_linea <- SpatialLines(list(Lines(Line(rbind(coord_inicio, coord_fin)), ID = "1")))
  # crs(corte_linea)<- "+proj=longlat +datum=WGS84"
  
  #dem_values <- extract(DEM_raster, corte_linea, along = TRUE, cellnumbers = TRUE, xy = TRUE)[[1]]
  dem_values <- extract(DEM_raster, puntos_sf,cellnumbers = TRUE)#, xy = TRUE)[[1]]
  #conc_values <- extract(promedio_mensual_062015, corte_linea, along = TRUE, cellnumbers = TRUE, xy = TRUE)[[1]]
  conc_values <- extract(promedio_mensual_062015, puntos_sf,cellnumbers = TRUE)#
  
  # Convertir ambos resultados en data frames
  dem_df <- as.data.frame(dem_values)
  conc_df <- as.data.frame(conc_values)
  # View(dem_df)
  
  dem_cells <- dem_df$cell
  conc_cells <- conc_df$cell
  
  # Extrae las coordenadas del centroide de cada celda del DEM
  centroides_dem <- xyFromCell(DEM_raster, dem_cells)
  
  # Extrae las coordenadas del centroide de cada celda del raster de concentraciones
  centroides_conc <- xyFromCell(promedio_mensual_062015, conc_cells)
  
  # Convertir a data frames para facilitar la manipulación
  centroides_dem_df <- as.data.frame(centroides_dem)
  centroides_conc_df <- as.data.frame(centroides_conc)
  
  #View(centroides_dem_df)
  # Agrega nombres a las columnas
  colnames(centroides_dem_df) <- c("x", "y")
  colnames(centroides_conc_df) <- c("x", "y")
  
  # Combina con los valores extraídos originales para cada raster
  dem_df <- cbind(centroides_dem_df, dem_values)
  conc_df <- cbind(centroides_conc_df, conc_values)
  
  # Realizar el merge de ambos data frames en base a las coordenadas 'x' e 'y'
  combined_df <- merge(dem_df, conc_df, by = "cells",suffixes = c("_DEM", "_CONC"))
  combined_df <- combined_df[complete.cases(combined_df),]
  #View(combined_df)
  combined_df$x <- round(combined_df$x_DEM,3)
  combined_df$x <- factor(combined_df$x, levels = combined_df$x)
  names(combined_df) <- c( "cells", "x_DEM", "y_DEM", "DEM_raster" ,"x_CONC",
                           "y_CONC", "promedio_mensual", "x")
  # ggplot(combined_df) +
  #   geom_line(aes(x = x, y = DEM_raster, group = 1), color = "#99d8c9", size = 1) +
  #   geom_point(aes(x = x, y = DEM_raster, group = 1), color =  "#99d8c9", size = 3, shape=1) + 
  #   geom_line(aes(x = x, y = promedio_mensual * (max(DEM_raster) / max(promedio_mensual)), group = 1), 
  #             color = "#fc9272", size = 1) +
  #   geom_point(aes(x = x, y = promedio_mensual* (max(DEM_raster) / max(promedio_mensual)), group = 1), color = "#fc9272", size = 3,shape=1) +
  #   
  #   scale_y_continuous(
  #     name = "Elevación (m)",
  #     sec.axis = sec_axis(~ . * (max(combined_df$promedio_mensual) / max(combined_df$DEM_raster)), 
  #                         name = "Concentración")
  #   ) +
  #   scale_x_discrete(
  #     name = "Longitud (°)"
  #   ) +
  #   labs(title = "Promedio mensual - 07-2015 XGB_cv_M1-041024") +
  #   theme_classic() +
  #   theme(
  #     axis.title.y = element_text(color = "#2ca25f"),
  #     axis.title.y.right = element_text(color = "#de2d26"),
  #     axis.text.x = element_text(angle = 45, hjust = 1)  # Rota las etiquetas si es necesario
  #   )
  # 
  # 
  # 
  
  
  library(ggplot2)
  
  # Define los valores mínimos y máximos para los ejes
  min_elevacion <- 300
  max_elevacion <- 1100  # Ajusta según tu rango de datos de DEM_raster
  min_concentracion <- 0
  max_concentracion <- 100  # Ajusta según tu rango de datos de promedio_mensual

}
# Genera el gráfico
plot <- ggplot(combined_df) +
  geom_line(aes(x = x, y = DEM_raster, group = 1), color = "#99d8c9", size = 1) +
  geom_point(aes(x = x, y = DEM_raster, group = 1), color = "#99d8c9", size = 3, shape = 1) + 
  geom_line(aes(x = x, y = promedio_mensual * (max(DEM_raster) / max(promedio_mensual)), group = 1), 
            color = "#fc9272", size = 1) +
  geom_point(aes(x = x, y = promedio_mensual * (max(DEM_raster) / max(promedio_mensual)), group = 1), 
             color = "#fc9272", size = 3, shape = 1) +
  
  scale_y_continuous(
    name = "Elevación (m)",
    limits = c(min_elevacion, max_elevacion),  # Límites para el eje principal
    sec.axis = sec_axis(~ . * (max_concentracion / max_elevacion), name = "Concentración")
  ) +
  scale_x_discrete(name = "Longitud (°)") +
  labs(title = "Promedio anual - 2023 XGB_cv_M1-041024") +
  theme_classic() +
  theme(
    axis.title.y = element_text(color = "#2ca25f"),
    axis.title.y.right = element_text(color = "#de2d26"),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rota las etiquetas si es necesario
  )

plot
year
mes


library(raster)

