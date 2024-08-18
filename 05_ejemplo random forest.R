
################################################################################
##################################################################################

# Crear un raster de ejemplo para temperatura
ncol <- 100
nrow <- 100
extent <- extent(-70, -69, -33, -32)




#Raster de AOD 550
AOD_550 <- raster(ncol=ncol, nrow=nrow, crs="+proj=longlat +datum=WGS84")
extent(AOD_550) <- extent
values(AOD_550) <- runif(ncell(AOD_550), min=10, max=30)

# Crear un raster de ejemplo para blh_mean
blh_mean<- raster(ncol=ncol, nrow=nrow, crs="+proj=longlat +datum=WGS84")
extent(blh_mean) <- extent
values(blh_mean) <- runif(ncell(blh_mean), min=0, max=100)

#sp_mean 

sp_mean <- raster(ncol=ncol, nrow=nrow, crs="+proj=longlat +datum=WGS84")
extent(sp_mean) <- extent
values(sp_mean) <- runif(ncell(sp_mean), min=10, max=30)

# Crear un raster d2m_mean 
d2m_mean <- raster(ncol=ncol, nrow=nrow, crs="+proj=longlat +datum=WGS84")
extent(d2m_mean) <- extent
values(d2m_mean) <- runif(ncell(d2m_mean), min=0, max=100)

#t2m_mean = runif(10, 0, 1),

t2m_mean <- raster(ncol=ncol, nrow=nrow, crs="+proj=longlat +datum=WGS84")
extent(t2m_mean) <- extent
values(t2m_mean) <- runif(ncell(t2m_mean), min=10, max=30)

# v10_mean = runif(10, 10, 30),

v10_mean <- raster(ncol=ncol, nrow=nrow, crs="+proj=longlat +datum=WGS84")
extent(v10_mean) <- extent
values(v10_mean) <- runif(ncell(v10_mean), min=0, max=100)

#u10_mean 

u10_mean <- raster(ncol=ncol, nrow=nrow, crs="+proj=longlat +datum=WGS84")
extent(u10_mean) <- extent
values(u10_mean) <- runif(ncell(u10_mean), min=10, max=30)

# Crear un raster de ejemplo tp_mean 
tp_mean <- raster(ncol=ncol, nrow=nrow, crs="+proj=longlat +datum=WGS84")
extent(tp_mean) <- extent
values(tp_mean) <- runif(ncell(tp_mean), min=0, max=100)

# Combinar los raster en un RasterStack
r_stack <- stack(AOD_550,blh_mean, sp_mean,d2m_mean,t2m_mean,v10_mean,u10_mean,tp_mean)

# Asignar nombres a las capas
names(r_stack) <- c("AOD_550","blh_mean", "sp_mean","d2m_mean","t2m_mean","v10_mean","u10_mean","tp_mean")

################################################################################
##################################################################################
# Cargar el modelo entrenado
load("random_forest_model.RData")

# Convertir el RasterStack en un data frame para la predicción
r_stack_df <- as.data.frame(r_stack, na.rm = TRUE)

# Aplicar el modelo de Random Forest al data frame
predictions <- predict(rf_model, newdata = r_stack_df)

# Crear un raster vacío con la misma extensión y resolución que el stack
pred_raster <- raster(r_stack)

# Asignar las predicciones al raster
pred_raster[] <- NA  # Inicia con valores NA

# Reinsertar las predicciones en las celdas correspondientes
pred_raster[!is.na(values(r_stack[[1]]))] <- predictions

getwd()
# Guardar el raster de predicciones
writeRaster(pred_raster, filename = "predicted_raster.tif", format = "GTiff", overwrite = TRUE)

print("Predicciones aplicadas y guardadas en 'predicted_raster.tif'.")




