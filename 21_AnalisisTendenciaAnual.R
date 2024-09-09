setwd("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/PM25/")
data_PM_OHG <- read.csv("01-OHG_PM25.csv")
data_PM_BSQ <- read.csv("02-BSQ_PM25.csv")
data_PM_CNA <- read.csv("03-CNA_PM25.csv")
data_PM_PDH <- read.csv("04-PDH_PM25.csv")
data_PM_FLD <- read.csv("05-FLD_PM25.csv")
data_PM_PTA <- read.csv("06-PTA_PM25.csv")
data_PM_CDE <- read.csv("07-CDE_PM25.csv")
data_PM_QUI <- read.csv("08-QUI_PM25.csv")

df_rbind <- rbind(data_PM_OHG,data_PM_BSQ,data_PM_CNA,
                  data_PM_PDH, data_PM_FLD,data_PM_PTA,
                  data_PM_CDE, data_PM_QUI)
df_rbind$date <- strptime(df_rbind$fecha, format = "%Y-%m-%d")

df_rbind <- df_rbind[complete.cases(df_rbind$valor),]
df_rbind <- df_rbind[year(df_rbind$date) == 2015 |
                               year(df_rbind$date) == 2016 |
                               year(df_rbind$date) == 2017 |
                               year(df_rbind$date) == 2018 |
                               year(df_rbind$date) == 2019 |
                               year(df_rbind$date) == 2020 |
                               year(df_rbind$date) == 2021 |
                               year(df_rbind$date) == 2022 |
                               year(df_rbind$date) == 2023 |
                               year(df_rbind$date) == 2024 ,]

df_rbind <- df_rbind[complete.cases(df_rbind$valor),]
# Metricas
summary_estaciones <- df_rbind %>% 
  group_by(estacion) %>%
  summarise(mean = round(mean(valor, na.rm = TRUE),3),
            min = round(min(valor, na.rm = TRUE),3),
            max = round(max(valor, na.rm = TRUE),3))

#####################################
# Plot violin
dir <- "D:/Josefina/Proyectos/ProyectoChile/plots/Concentraciones/"

png(file = paste(dir,"lim_dia_PM.png",sep=""), width = 4000, height = 3000, res = 500)
df_rbind %>% 
  #melt( id = "date") %>%
  group_by(estacion) %>%
  ggplot(aes(x= estacion, y = valor)) + 
  geom_violin(na.rm = TRUE)  +
  theme_bw() +  
  labs(x = "", y ="", title= "Mediciones diarias de PM25") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1) ) + 
  # geom_hline(yintercept=5, linetype="dashed", color = "red")  +  # legal
  geom_hline(yintercept= 15, linetype="dashed", color = "blue")    # OMS
dev.off()

##########################################################
##############################################################
#lIMITES OMS
limite <- 15
estacion <- "QUI"
# Datos de la estacion de interes
data_estacion <- df_rbind[df_rbind$estacion == estacion, ]
# numero de datos diarios que estan por debajo de este limite
num_limites <- data_estacion[data_estacion$valor < limite,]
nrow(num_limites)/(nrow(data_estacion))*100


summary_estaciones_year2 <- df_rbind %>% 
  group_by(estacion,year) %>%
  summarise(mean = round(mean(valor, na.rm = TRUE), 3), .groups = 'keep')
limite_anual <- summary_estaciones_year[summary_estaciones_year$mean > 5,]


########################################################
class(df_rbind$date)
# Guardar gráficos como archivos PNG
dir <- "D:/Josefina/Proyectos/ProyectoChile/plots/concentraciones/"
#### BSQ

df_rbind$date <- as.POSIXct(df_rbind$date)
a<- timeVariation(df_rbind, pollutant = "valor", normalise = FALSE ,group="estacion",
                  ylab="Concentraciones PM25 diarias", xlab="Dia",key.columns = 3,key.header="velocidad del viento")
png(paste(dir,"timeVariation_day_2.png",sep=""), width = 4000, height = 3000, res = 500)
print(a$plot$day)
dev.off()


###############
# PM2.5
data_PM_OHG <- df_rbind[df_rbind$estacion == "OHG",]
data_PM_OHG$date <- as.POSIXct(strptime(data_PM_OHG$fecha, format = "%Y-%m-%d"))
data_PM_OHG$pm25 <- data_PM_OHG$valor
df_rbind$date <- as.POSIXct(strptime(df_rbind$fecha, format = "%Y-%m-%d"))

p<- TheilSen(df_rbind, pollutant = "valor", ylab = "PM2.5", 
         deseason = TRUE, 
         slope.percent = TRUE,
         type = "estacion",
         layout = c(2, 6),
         #scales = list(y = list(rot=45), x = list(rot = 45)))
         scales = list(
           x = list(
             at = as.Date(c("2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01","2019-01-01","2020-01-01", "2021-01-01","2022-01-01",
                            "2023-01-01","2024-01-01")), # especificar las fechas
             labels = c("2015", "2016", "2017", "2018","2019","2020","2021","2022","2023","2024"),  # Etiquetas solo con el año
             rot = 45  # Ajuste de rotación si lo deseas
           ),
           y = list(rot = 45)  # Rotar el eje Y
         ))
png(paste(dir,"timeVariation_day_4.png",sep=""), width = 700, height = 900)#width = 4000, height = 3000, res = 500)
print(p)
dev.off()

plot(p$plot)
dev.off
p$data$res2
a <-p$data$res2
a <- a[,c(1,2,12,16, 17)]
a <- data.frame( site = a$estacion,
                 p.stars = a$p.stars,
                 tendencia = paste(round(a$slope, 3), 
                                   " [", 
                                   round(a$lower, 3), ", ", 
                                   round(a$upper, 3), "]", sep="" ))

