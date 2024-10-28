
#Comparativa PM2.5 vs formaula de merra - 2

data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/merge_tot/09_TOT_merge_tot.csv")
data <- data[complete.cases(data$PM25),]
data$PM25_MERRA <- data$DUSMASS_dia + data$OCSMASS_dia + data$BCSMASS_dia + data$SSSMASS_dia + 
  data$SO4SMASS_dia * (132.14/96.06)

data$PM25_MERRA_hora <- data$DUSMASS + data$OCSMASS + data$BCSMASS + data$SSSMASS + 
  data$SO4SMASS * (132.14/96.06)
#PM2.5 = DUSMASS25 + OCSMASS+ BCSMASS + SSSMASS25 + SO4SMASS* (132.14/96.06)




data <- data.frame (PM25 = data$PM25, PM25_MERRA=data$PM25_MERRA,PM25_MERRA_hora=data$PM25_MERRA_hora)
data <- data[complete.cases(data$PM25),]
data <- data[complete.cases(data$PM25_MERRA),]
data <- data[complete.cases(data$PM25_MERRA_hora),]
# https://gmao.gsfc.nasa.gov/reanalysis/MERRA-2/FAQ/
# ulfate requires a multiplication factor since the species tracer in MERRA-2 is the sulfate ion.
# Unlike with MERRAero, a multiplicative factor is not included for OCSMASS to convert organic 
# carbon to organic matter since this is already handled within the model. For users of GEOS FP,
# please note that this equation is not applicable to FP as MERRA-2 does not include nitrate aerosol.

#####  EVALUAR EL MODELO


  # Ajuste del modelo de regresión lineal
modelo <- lm(PM25_MERRA ~ PM25, data =data)
modelo <- lm(PM25_MERRA_hora ~ PM25, data =data)

# Resumen del modelo
summary(modelo)

# Obtener coeficiente de determinación R^2
r2 <- summary(modelo)$r.squared

# Coeficiente de correlación r
correlacion <- cor(data$PM25, data$PM25_MERRA_hora)

# Predicciones del modelo
#predicciones <- predict(modelo, newdata = data$PM25_MERRA_hora)

# Calcular el RMSE
#rmse <- sqrt(mean((data$PM25_MERRA - predicciones)^2))

# Calcular el sesgo (bias)
bias <- mean(data$PM25_MERRA- predicciones)



# Mostrar resultados
cat("R^2:", r2, "\n")
cat("Coeficiente de correlación r:", correlacion, "\n")
cat("RMSE:", rmse, "\n")
cat("Bias:", bias, "\n")




