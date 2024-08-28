
## Rutas - calles

library(sf)
library(raster)

dir<- "D:/Josefina/Proyectos/ProyectoChile/shape/Red_Vial/"# Cargar la capa de líneas (carreteras)
lineas <- st_read(paste(dir,"redvial_OSM_interseccion.shp",sep=""))
poligono_grid <-st_read("D:/Josefina/Proyectos/ProyectoChile/shape/data_referencia/grilla_1km_utm.shp")
# Definir el CRS proyectado (en este caso, UTM zona 19S para Chile central)
crs_proyectado <- "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs"

# Transformar las líneas a la proyección en metros
lineas_proj <- st_transform(lineas, crs_proyectado)

# Calcular la longitud de cada segmento de línea dentro de cada píxel
lineas_proj$length_km <- as.numeric(st_length(lineas_proj) / 1000)  # Convierte de metros a km

# Cuales son las categorias que t
unique(lineas_proj$fclass)
"secondary"      "primary"        "tertiary" "motorway"    "residential" 
#Descartadas: "service"   "track"  "busway" "bridleway", "cycleway
# "footway"living_street , motorway_link(son los ingresos a las autopistas)
# path primary_link

lineas_proj_select <- lineas_proj[lineas_proj$fclass == "secondary" |
                                    lineas_proj$fclass == "primary" |
                                    lineas_proj$fclass == "tertiary" |
                                    lineas_proj$fclass =="motorway"  | 
                                    lineas_proj$fclass == "residential" ,]


# Guardar la cuadrícula con la densidad como shapefile
# st_write(lineas_proj_select, paste(dir,"lineas_proj_select.shp",sep=""))
# Agregar la longitud total por píxel
densidad_por_pixel <- aggregate(lineas_proj_select$length_km, 
                                by = list(lineas_proj_select$ID, lineas_proj_select$fclass), 
                                sum)
# Renombrar las columnas para mayor claridad
names(densidad_por_pixel) <- c("ID", "fclass", "total_length_km")

# Contar cuántos objetos (segmentos de línea) hay por píxel y categoría
conteo_por_pixel <- aggregate(lineas_proj_select$ID, 
                              by = list(lineas_proj_select$ID, lineas_proj_select$fclass), 
                              length)

# Renombrar las columnas para mayor claridad
names(conteo_por_pixel) <- c("ID", "fclass", "count")

# Unir el conteo con la densidad
densidad_por_pixel <- merge(densidad_por_pixel, conteo_por_pixel, 
                            by = c("ID", "fclass"))

# Calcular la densidad en km/km² (en este caso, el área es 1 km²)
# en el qgis el area es menor a 1km2 es 0.87739
densidad_por_pixel$density_km2 <- densidad_por_pixel$total_length_km / 0.87739  # Dividir por el área en km² (que es 1 en este caso)


# Pivotar los datos para que cada tipo de carretera tenga su propia columna
densidad_pivot <- densidad_por_pixel %>%
  pivot_wider(names_from = fclass, values_from = c(total_length_km, count,density_km2), values_fill = list(total_length_km = 0, count =0,density_km2=0))




# Unir la densidad con la cuadrícula
grid_merged <- poligono_grid %>%
  left_join(densidad_pivot, by = c("ID" = "ID"))

# Realizar el join y luego reemplazar los NA con 0
grid_merged2 <- poligono_grid %>%
  left_join(densidad_pivot, by = "ID") %>%
  replace_na(list(
    densidad_km2 = 0, 
    count_motorway = 0, 
    count_primary = 0, 
    count_residential = 0, 
    count_secondary = 0, 
    count_tertiary = 0,
    density_km2_residential = 0,
    density_km2_primary = 0,
    density_km2_secondary =0,
    density_km2_tertiary = 0,
    density_km2_motorway=0
    
    
  ))


names(grid_merged2) <- c("ID","Area", "len_res","len_ter","len_pri", 
                        "len_mot", "len_sec","count_res","count_ter",
                        "count_pri","count_mot", "count_sec", "dens_res", "dens_ter",       
                        "dens_pri", "dens_mot", "dens_sec","geometry")
shapefile_wgs84 <- st_transform(grid_merged2, crs = 4326)  # CRS 4326 corresponde a WGS84

# Guardar la cuadrícula con la densidad como shapefile
st_write(grid_merged, paste(dir,"densidad_carreteras.shp",sep=""))
# Crear un raster vacío con la extensión del shapefile y la resolución deseada
raster_template <- raster("D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/tiff/NDVI_raster.tif")

rasterizado_dens_res <- rasterize(shapefile_wgs84, raster_template, field = "dens_res", fun = "mean")
rasterizado_dens_primaria <- rasterize(shapefile_wgs84, raster_template, field = "dens_pri", fun = "mean")
rasterizado_dens_secundario <- rasterize(shapefile_wgs84, raster_template, field = "dens_sec", fun = "mean")
rasterizado_dens_terciario <- rasterize(shapefile_wgs84, raster_template, field = "dens_ter", fun = "mean")
rasterizado_dens_motorway <- rasterize(shapefile_wgs84, raster_template, field = "dens_mot", fun = "mean")

#Guardar el raster en un archivo
writeRaster(rasterizado_dens_res, "D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/tiff/densRutaResidencial_raster.tif", format = "GTiff")
writeRaster(rasterizado_dens_primaria, "D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/tiff/densRutaPrimario_raster.tif", format = "GTiff")
writeRaster(rasterizado_dens_secundario, "D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/tiff/densRutaSecundario_raster.tif", format = "GTiff")
writeRaster(rasterizado_dens_terciario, "D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/tiff/densRutaTerciario_raster.tif", format = "GTiff")
writeRaster(rasterizado_dens_motorway, "D:/Josefina/Proyectos/ProyectoChile/modelos/dataset_ejemplo/tiff/densRutaMotorway_raster.tif", format = "GTiff")

