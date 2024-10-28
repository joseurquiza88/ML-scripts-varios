
### Pasas de raster a poligono
# lo controlamos en el qgis
# Cargar librerías necesarias
library(raster)     # Para manejar archivos raster
library(rgdal)      # Para exportar a Shapefile
rm(list=ls())
month <- "07"
year <- "202"
for (i in 1:1){
  # setwd("D:/Josefina/Proyectos/ProyectoChile/modelos/Salidas/SalidasMensuales/Salida_03-XGB_cv_M1-041024")
  setwd("D:/Josefina/Proyectos/ProyectoChile/modelos/Salidas/SalidasAnuales/Salida_03-XGB_cv_M1-041024")
  # Leer el archivo TIFF
  # raster_data <- raster(paste("promedio_mensual_",month,"-",year,"-03-XGB_cv_M1-041024.tif",sep=""))
  raster_data <- raster(paste("promedio_anual_",year,"-03-XGB_cv_M1-041024.tif",sep=""))
  
  # Convertir el raster a un polígono
  vector_data <- rasterToPolygons(raster_data, dissolve = TRUE)
  
  crs(vector_data) <- crs(raster_data)
  
  # Guardar el archivo como Shapefile
  writeOGR(vector_data, "./shapefile", paste("promedio_anual_",year,"-03-XGB_cv_M1-041024",sep=""), driver = "ESRI Shapefile")
}
##############################################################################
################################################################################
# Interseccion entre concentraciones y comunas

# Cargar la librería sf
library(sf)
rm(list=ls())
# setwd("D:/Josefina/Proyectos/ProyectoChile/modelos/Salidas/SalidasMensuales/Salida_03-XGB_cv_M1-041024/shapefile/")
setwd("D:/Josefina/Proyectos/ProyectoChile/modelos/Salidas/SalidasAnuales/Salida_03-XGB_cv_M1-041024/shapefile/")

month <- "07"
year <- "2023"
# Leer los dos archivos Shapefile
# poligonos_concentraciones <- st_read(paste("promedio_mensual_",month,"-",year,"-03-XGB_cv_M1-041024.shp",sep=""))
for (i in 1:1){
  poligonos_concentraciones <- st_read(paste("promedio_anual_",year,"-03-XGB_cv_M1-041024.shp",sep=""))
  
  poligonos_comunas <- st_read("D:/Josefina/Proyectos/ProyectoChile/shape/Comunas/Comunas_cortados.shp")
  
  poligonos_concentraciones <- st_transform(poligonos_concentraciones, crs = 4326)
  poligonos_comunas <- st_transform(poligonos_comunas, crs = 4326)
  
  # Confirmar que ambos tengan el mismo CRS
  st_crs(poligonos_concentraciones) == st_crs(poligonos_comunas)  # Debe devolver TRUE
  
  # Ahora puedes proceder con la intersección
  interseccion <- st_intersection(poligonos_concentraciones, poligonos_comunas)
  names(interseccion) <- c("pm","objectid", "shape_leng","dis_elec","cir_sena", "cod_comuna",
                           "codregion",  "st_area_sh", "st_length_" ,"Region","Comuna",
                           "Provincia" ,"geometry")
  ############################################
  #Agrupamos por comuna
  # Cargar librería dplyr
  #
  #library(dplyr)
  
  # Suponiendo que tienes un dataframe llamado "datos" con las columnas "comuna" y "p__05_2"
  resultado <- interseccion %>%
    group_by(Comuna) %>%
    summarise(
      media = mean(pm, na.rm = TRUE),     # Media de p__05_2
      suma = sum(pm, na.rm = TRUE),       # Suma de p__05_2
      sd = sd(pm, na.rm = TRUE),          # Desviación estándar de p__05_2
      min = min(pm, na.rm = TRUE),
      max = max(pm, na.rm = TRUE),
      n = n()   # Ordenar de mayor a menor                                 # Número de datos por comuna
    )
}
resultado_orderMayor <- resultado %>%
  arrange(desc(media))
View(resultado_orderMayor)
resultado_orderMenor <- resultado %>%
  arrange(media)
View(resultado_orderMenor)

#corroboramos con santiago
Santiago <- interseccion [ interseccion$Comuna== "Santiago",]
mean <- mean(Santiago$p__05_2,na.rm=TRUE)
sd <- sd(Santiago$p__05_2,na.rm=TRUE)
sum <- sum(Santiago$p__05_2,na.rm=TRUE)
# Ver el resultado
print(resultado)



