
data = read.csv("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/merge_pm-maiac-meteo/1000/08-TOT_merge_PM25-MAIAC-Meteo.csv") 
data = data2[data2$estacion == "OHG",]

data$fecha <- as.Date(data$date,"%d/%m/%Y")
data$month <- as.numeric(format(data$fecha,"%m"))
data$Dia1 <- rep(data$fecha[1],nrow(data))
data$Dias <- (interval(data$Dia1,data$fecha)%/% days(1))+1
data$dayofweek <- wday(data$fecha)
data$dayofyear <- yday(data$fecha)


# Imprimir el dataframe con la nueva columna
print(data)
head(data)
str(data)
summary(data)
data3 <- data.frame(AOD_550 = data$AOD_550,
                    PM25 = data$PM25,
                    temperatura = data$temperatura,
                    humedad = data$humedad)
plot(data3$PM25)
frequency(data3$PM25)
hist(data3$AOD_550)
hist(data3$PM25)
hist(data3$temperatura)
hist(data3$humedad)#distribucion normal

log_humedad <- log(data3$humedad)
log_PM25 <- log(data3$PM25)
log_temperatura <- log(data3$temperatura)
log_AOD_550 <- log(data3$AOD_550)

hist(log_AOD_550)
hist(log_PM25)
hist(log_temperatura)#no!!
hist(log_humedad)#no!!

#Sobredispersion de los datos. La varianza es mas alta que la media
# Para esto se puede usar distribuicion binomial positiva o cuasi poisson
var(data$PM25)
mean(data$PM25)
ggplot(data, aes(x=Dias, y=PM25))+
  geom_line()+ geom_point()+ theme(legend.position = "none")


#Modelo 1
# Funcion suave s()
# 2 efectos
#Estacional: s(mes)/spline cubico ciclico(cc)
#Interanual: s(dias)/spline cubico (cr)

modelo1 <- gamm(PM25 ~ s(mes, bs = "cc")+ s(Dias, bs="cr"),
                        family= quasipoisson, data=data,
                        correlation =corCAR1(form = ~ Dias)) #termino aleatorio de la autorcorrelacion de la serie temporal


# Revisar el resumen del modelo
summary(modelo1$gam)
############################
#ejmplo 2

modelo1 <- gam(PM25 ~ s(AOD_550), family = gaussian, data=data3) #termino aleatorio de la autorcorrelacion de la serie temporal
summary(modelo1)
# Vemos que (Intercept) es = <2e-16 *** y que tiene un error esyandar de 0.3143
# es significativamente distinto de 0

# s(AOD_550) es la funcion de suavizado con Ref.df (numero de grados de libertad)
# y con p-value significativo que nos dice que es to es distinto que nos dice 
# que la funcion de suavizado es distinto a una linea horizontlal con pendiente 0

# y tiene un r2 de 3.36% es decir una desviacion explicada. Este modelo no es bueno
modelo2 <- gam(PM25 ~ s(AOD_550)+s(temperatura)+s(humedad), family = gaussian, data=data3) #termino aleatorio de la autorcorrelacion de la serie temporal
summary(modelo2) # el r2 es de 40%

plot(modelo1)
plot(modelo2)

#### el modelo 1/algortimo estiomo 6.117gl(grados de libertad) pero 
#esto puede variar y explorar sus efectos en el ajuste con el valor de k

modelo1B <- gam(PM25 ~ s(AOD_550,fx=F, k=2), family = gaussian, data=data3) #termino aleatorio de la autorcorrelacion de la serie temporal
summary(modelo1B)
modelo1C <- gam(PM25 ~ s(AOD_550,fx=F, k=4), family = gaussian, data=data3) #termino aleatorio de la autorcorrelacion de la serie temporal
summary(modelo1C)
modelo1D <- gam(PM25 ~ s(AOD_550,fx=F, k=6), family = gaussian, data=data3) #termino aleatorio de la autorcorrelacion de la serie temporal
summary(modelo1D)
modelo1E <- gam(PM25 ~ s(AOD_550,fx=F, k=15), family = gaussian, data=data3) #termino aleatorio de la autorcorrelacion de la serie temporal
summary(modelo1E)
summary(modelo1)
plot(modelo1B)
# A medida que aumenta el k nos da el grado de curvinelidad local, aumentamos k, umentan las curvas
# Cambiamos varias veces el K y no hay diferencias
#### modelo 2 cambiamos el k
modelo2B <- gam(PM25 ~ s(AOD_550, k=25)+s(temperatura,fx=F, k=25)+s(humedad,fx=F, k=25), family = gaussian, data=data3) #termino aleatorio de la autorcorrelacion de la serie temporal
summary(modelo2B) # el r2 es de 40%


# Analisis de los residuos
par(mfrow=c(1,3),cex.axis=1.3,cex.lab=1.5)
plot(resid(modelo1)~fitted(modelo1));abline(0,0)
qqnorm(resid(modelo1),main="");qqline(resid(modelo1))
plot(sqrt(abs(resid(modelo1)))~fitted(modelo1))
par(mfrow=c(1,1),cex.axis=1,cex.lab=1)

#vemos que no hay un buen ajuste del modelo 2


# Analisis de los residuos
par(mfrow=c(1,3),cex.axis=1.3,cex.lab=1.5)
plot(resid(modelo2)~fitted(modelo2));abline(0,0)
qqnorm(resid(modelo2),main="");qqline(resid(modelo2))
plot(sqrt(abs(resid(modelo2)))~fitted(modelo2))
par(mfrow=c(1,1),cex.axis=1,cex.lab=1)
# para el modelo 2 vemos que hay un mejor ajuste



## Seleccion de modelos con el test LRT 
anova(modelo1, modelo2)
# Analisis de residuos
gam.check(modelo1)
gam.check(modelo2)
###############################################################
modelo <- gam(PM25 ~ s(AOD_550) + s(temperatura) + s(humedad), 
              family = Gamma(link = "log"), 
              data = data)
summary(modelo)

modelo1 <- gam(PM25 ~ s(AOD_550) + s(temperatura) + s(humedad)+ s(velViento)+ s(dirViento), 
              family = Gamma(link = "log"), 
              data = data)
summary(modelo1)

modelo2 <- gam(PM25 ~ s(AOD_550,k=20) + s(temperatura,k=10) + s(humedad,k=1)+ s(velViento,k=3)+ s(dirViento), 
               family = Gamma(link = "log"),
               data = data)
summary(modelo2)


modelo3 <- gam(PM25 ~ s(AOD_550,k=20) + s(temperatura,bs="cr") + s(humedad,k=1)+ s(velViento,bs="cc",k=3)+ s(dirViento,bs="cc")+s(dayofweek,k=7,bs="cr")+s(dayofyear,bs="cr"), 
               family = Gamma(link = "log"),
               data = data)

summary(modelo3)

modelo4 <- gam(PM25 ~ s(AOD_550) + s(temperatura) + s(humedad)+ s(dayofweek,k=7,bs="cr")+s(dayofyear,bs="cr"), 
               family = Gamma(link = "log"),
               data = data)
gaussian
modelo5 <- gam(PM25 ~ s(AOD_550) + s(temperatura) + s(humedad)+ s(dayofweek,k=7,bs="cr")+s(dayofyear,bs="cr"), 
               family = gaussian(link = "log"),
               data = data)
# s(velViento,bs="cc")+ s(dirViento,bs="cc")
summary(modelo4)
plot(modelo4)
# Analisis de los residuos
par(mfrow=c(1,3),cex.axis=1.3,cex.lab=1.5)
plot(resid(modelo4)~fitted(modelo4));abline(0,0)
qqnorm(resid(modelo4),main="");qqline(resid(modelo4))
plot(sqrt(abs(resid(modelo4)))~fitted(modelo4))
par(mfrow=c(1,1),cex.axis=1,cex.lab=1)

##############################################################
####################################################################

# Cargar las librerías necesarias
library(mgcv)
library(caret)

# Simular un conjunto de datos como ejemplo
set.seed(123)
data <- data.frame(
  PM2.5 = rnorm(1000, mean = 50, sd = 10),
  AOD_550 = rnorm(1000, mean = 0.3, sd = 0.1),
  temperatura = rnorm(1000, mean = 25, sd = 5),
  humedad = rnorm(1000, mean = 60, sd = 10),
  date = seq.Date(from = as.Date("2013-01-01"), by = "day", length.out = 1000)
)

# Crear índices de validación cruzada de 10 folds
set.seed(123)
folds <- createFolds(data$PM2.5, k = 10)

# Lista para almacenar los resultados del modelo en cada fold
results <- list()

# Realizar validación cruzada manualmente
for(i in 1:10) {
  # Separar los datos en entrenamiento y validación
  train_indices <- unlist(folds[-i])
  test_indices <- unlist(folds[i])
  train_data <- data[train_indices, ]
  test_data <- data[test_indices, ]
  
  # Ajustar el modelo GAM con los datos de entrenamiento
  gam_model <- gam(PM2.5 ~ s(AOD_550) + s(temperatura) + s(humedad), data = train_data)
  
  # Predicciones en el conjunto de validación
  predictions <- predict(gam_model, newdata = test_data)
  
  # Evaluar el rendimiento del modelo
  actuals <- test_data$PM2.5
  rmse <- sqrt(mean((predictions - actuals)^2))
  results[[i]] <- rmse
}

# Calcular el RMSE promedio de la validación cruzada
mean_rmse <- mean(unlist(results))
print(paste("Average RMSE from 10-fold cross-validation: ", round(mean_rmse, 2)))

# Ajustar el modelo final con todos los datos
final_gam_model <- gam(PM2.5 ~ s(AOD_550) + s(temperatura) + s(humedad), data = data)

# Resumen del modelo final
summary(final_gam_model)
# Visualizar los resultados
plot(actuals, predictions, main = "Actual vs Predicted PM2.5",
     xlab = "Actual PM2.5", ylab = "Predicted PM2.5")
abline(0, 1, col = "red")

# Graficar diagnóstico del modelo
par(mfrow = c(2, 2))
plot(gam_model$finalModel, pages = 1)
###################################################################
# 01 Correlation of PM2.5 with Meteorological Factors con GAMs (100% datos)
# 02. GAM Fitting: train (90%) test (10%)
# Metrica: R2, AIC, F-Value, p-Value, % variabilidad (devianza explicada)
# 03. Validation of the GAM Models gam.check function
# 04.We used cross-validation (CV) to further evaluate the GAM results.
# 05. Plots:-  Q-Q plots (sample quantiles against predicted quantiles),
#           -  scatter plots (residuals against linear predictor), 
#           -  histogram of the residuals, and scatter plots (response against fitted values)


# 06. Plot de serie temporal en observado vs prediccion
