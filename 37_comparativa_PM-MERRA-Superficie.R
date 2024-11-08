
#Comparativa PM2.5 vs formaula de merra - 2

data <- read.csv("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/merge_tot/09_TOT_merge_tot.csv")
data <- data[complete.cases(data$PM25),]
data$PM25_MERRA <- data$DUSMASS_dia + data$OCSMASS_dia + data$BCSMASS_dia + data$SSSMASS25_dia + 
  data$SO4SMASS_dia * (132.14/96.06)

data$PM25_MERRA_hora <- data$DUSMASS + data$OCSMASS + data$BCSMASS + data$SSSMASS25 + 
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
#predicciones <- predict(modelo, newdata = data$PM25_MERRA)

# Calcular el RMSE
rmse <- sqrt(mean((data$PM25_MERRA - predicciones)^2))

# Calcular el sesgo (bias)
bias <- mean(data$PM25_MERRA- predicciones)



# Mostrar resultados
cat("R^2:", r2, "\n")
cat("Coeficiente de correlación r:", correlacion, "\n")
cat("RMSE:", rmse, "\n")
cat("Bias:", bias, "\n")




# Calculo de métricas de desempeño
R2 <- summary(modelo)$r.squared
RMSE <- sqrt(mean(residuals(modelo)^2))
Bias <- mean(data$PM25_MERRA - data$PM25)
n <- nrow(data)

# Crear el gráfico con ggplot2
plot <- ggplot(data, aes(x = PM25, y = PM25_MERRA)) +
  geom_point(color = "#3690c0", size = 1.5, alpha = 0.6) +  # Puntos de datos
  geom_smooth(method = "lm", color = "#ef3b2c", se = FALSE, linetype = "dashed") +  # Línea de regresión
  #scale_y_continuous(limits = c(0, 200),breaks = seq(0, 200, by = 50)) +  # Ticks cada 10 en el eje Y
  #scale_x_continuous(limits = c(0, 200),breaks = seq(0, 200, by = 50)) +  # Ticks cada 10 en el eje Y
  labs(
    x = "SINCA",
    y = "Prediccion",
    title = "Salida_03-XGB_cv_M1-041024 - Periodo 2015-2024 - Total",
    subtitle = paste(
      "R2 =", round(R2, 3),
      "| RMSE =", round(RMSE, 2),
      "| Bias =", round(Bias, 2),
      "| n =", n
    )
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot
# Mostrar el gráfico
print(plot)
