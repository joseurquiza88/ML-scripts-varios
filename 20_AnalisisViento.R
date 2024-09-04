
# Tendencia de viento
library(ggplot2)
library(reshape2)
library(dplyr)
library(lubridate)
library(RColorBrewer)
library(openair)
library(classInt)
setwd("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/otras/meteo_superficial/")
data_velocidad_OHG <- read.csv("velViento/01-OHG_velViento.csv")
data_velocidad_OHG$date <- strptime(data_velocidad_OHG$fecha, format = "%Y-%m-%d")
data_dir_OHG <- read.csv("dirViento/01-OHG_dirViento.csv")
data_dir_OHG$date <- strptime(data_dir_OHG$fecha, format = "%Y-%m-%d")
df_merge_OHG <- merge(x = data_dir_OHG, y = data_velocidad_OHG, by = "date") # Equivalente
df_merge_OHG <- data.frame(date= df_merge_OHG$date, wd = df_merge_OHG$valor.x,
                       ws = df_merge_OHG$valor.y)
df_merge_OHG <- df_merge_OHG[complete.cases(df_merge_OHG),]
df_merge_OHG <- df_merge_OHG[year(df_merge_OHG$date) == 2015 |
                               year(df_merge_OHG$date) == 2016 |
                               year(df_merge_OHG$date) == 2017 |
                               year(df_merge_OHG$date) == 2018 |
                               year(df_merge_OHG$date) == 2019 |
                               year(df_merge_OHG$date) == 2020 |
                               year(df_merge_OHG$date) == 2021 |
                               year(df_merge_OHG$date) == 2022 |
                               year(df_merge_OHG$date) == 2023 |
                               year(df_merge_OHG$date) == 2024 ,]
# Generar la rosa de vientos
col <- c("#74ADD1" , "#E0F3F8", "#FEE090", "#F46D43" )
# Generar la rosa de vientos utilizando los intervalos de Jenks
plot_OHG <- windRose(df_merge_OHG, ws = "ws", wd = "wd", 
         angle = 45, paddle = FALSE, 
         dig.lab = 4, # Hace referencia a los decimales de mean
         cols = col,#"jet", 
         breaks = 9,  # Usar los breakpoints calculados
         key.position = "right",
         hemisphere= "southern",
         type = "season",
         main = "01 OHG",key.header = NULL)
##############################################################
##################################################################
data_velocidad_BSQ <- read.csv("velViento/02-BSQ_velViento.csv")
data_velocidad_BSQ$date <- strptime(data_velocidad_BSQ$fecha, format = "%Y-%m-%d")
data_dir_BSQ <- read.csv("dirViento/02-BSQ_dirViento.csv")
data_dir_BSQ$date <- strptime(data_dir_BSQ$fecha, format = "%Y-%m-%d")
df_merge_BSQ <- merge(x = data_dir_BSQ, y = data_velocidad_BSQ, by = "date") # Equivalente
df_merge_BSQ <- data.frame(date= df_merge_BSQ$date, dirViento = df_merge_BSQ$valor.x,
                           velViento = df_merge_BSQ$valor.y)
df_merge_BSQ <- df_merge_BSQ[complete.cases(df_merge_BSQ),]
df_merge_BSQ <- df_merge_BSQ[year(df_merge_BSQ$date) == 2015 |
                               year(df_merge_BSQ$date) == 2016 |
                               year(df_merge_BSQ$date) == 2017 |
                               year(df_merge_BSQ$date) == 2018 |
                               year(df_merge_BSQ$date) == 2019 |
                               year(df_merge_BSQ$date) == 2020 |
                               year(df_merge_BSQ$date) == 2021 |
                               year(df_merge_BSQ$date) == 2022 |
                               year(df_merge_BSQ$date) == 2023 |
                               year(df_merge_BSQ$date) == 2024 ,]
# Generar la rosa de vientos
col <- c("#74ADD1" , "#E0F3F8", "#FEE090", "#F46D43" )
# Generar la rosa de vientos utilizando los intervalos de Jenks
plot_BSQ <- windRose(df_merge_BSQ, ws = "velViento", wd = "dirViento", 
                     angle = 45, paddle = FALSE, 
                     dig.lab = 4, # Hace referencia a los decimales de mean
                     cols = col,#"jet", 
                     breaks = 9,  # Usar los breakpoints calculados
                     key.position = "right",
                     hemisphere= "southern",
                     type = "season",
                     main = "BSQ",key.header = NULL)

##############################################################
##################################################################
data_velocidad_PDH <- read.csv("velViento/04-PDH_velViento.csv")
data_velocidad_PDH$date <- strptime(data_velocidad_PDH$fecha, format = "%Y-%m-%d")
data_dir_PDH <- read.csv("dirViento/04-PDH_dirViento.csv")
data_dir_PDH$date <- strptime(data_dir_PDH$fecha, format = "%Y-%m-%d")
df_merge_PDH <- merge(x = data_dir_PDH, y = data_velocidad_PDH, by = "date") # Equivalente
df_merge_PDH <- data.frame(date= df_merge_PDH$date, dirViento = df_merge_PDH$valor.x,
                           velViento = df_merge_PDH$valor.y)
df_merge_PDH <- df_merge_PDH[complete.cases(df_merge_PDH),]
df_merge_PDH <- df_merge_PDH[year(df_merge_PDH$date) == 2015 |
                               year(df_merge_PDH$date) == 2016 |
                               year(df_merge_PDH$date) == 2017 |
                               year(df_merge_PDH$date) == 2018 |
                               year(df_merge_PDH$date) == 2019 |
                               year(df_merge_PDH$date) == 2020 |
                               year(df_merge_PDH$date) == 2021 |
                               year(df_merge_PDH$date) == 2022 |
                               year(df_merge_PDH$date) == 2023 |
                               year(df_merge_PDH$date) == 2024 ,]
# Generar la rosa de vientos
col <- c("#74ADD1" , "#E0F3F8", "#FEE090", "#F46D43" )
# Generar la rosa de vientos utilizando los intervalos de Jenks
plot_PDH <- windRose(df_merge_PDH, ws = "velViento", wd = "dirViento", 
                     angle = 45, paddle = FALSE, 
                     dig.lab = 4, # Hace referencia a los decimales de mean
                     cols = col,#"jet", 
                     breaks = 9,  # Usar los breakpoints calculados
                     key.position = "right",
                     hemisphere= "southern",
                     type = "season",
                     main = "PDH",key.header = NULL)

##############################################################
##################################################################
data_velocidad_CNA <- read.csv("velViento/03-CNA_velViento.csv")
data_velocidad_CNA$date <- strptime(data_velocidad_CNA$fecha, format = "%Y-%m-%d")
data_dir_CNA <- read.csv("dirViento/03-CNA_dirViento.csv")
data_dir_CNA$date <- strptime(data_dir_CNA$fecha, format = "%Y-%m-%d")
df_merge_CNA <- merge(x = data_dir_CNA, y = data_velocidad_CNA, by = "date") # Equivalente
df_merge_CNA <- data.frame(date= df_merge_CNA$date, dirViento = df_merge_CNA$valor.x,
                           velViento = df_merge_CNA$valor.y)
df_merge_CNA <- df_merge_CNA[complete.cases(df_merge_CNA),]
df_merge_CNA <- df_merge_CNA[year(df_merge_CNA$date) == 2015 |
                               year(df_merge_CNA$date) == 2016 |
                               year(df_merge_CNA$date) == 2017 |
                               year(df_merge_CNA$date) == 2018 |
                               year(df_merge_CNA$date) == 2019 |
                               year(df_merge_CNA$date) == 2020 |
                               year(df_merge_CNA$date) == 2021 |
                               year(df_merge_CNA$date) == 2022 |
                               year(df_merge_CNA$date) == 2023 |
                               year(df_merge_CNA$date) == 2024 ,]
# Generar la rosa de vientos
col <- c("#74ADD1" , "#E0F3F8", "#FEE090", "#F46D43" )
# Generar la rosa de vientos utilizando los intervalos de Jenks
plot_CNA <- windRose(df_merge_CNA, ws = "velViento", wd = "dirViento", 
                     angle = 45, paddle = FALSE, 
                     dig.lab = 4, # Hace referencia a los decimales de mean
                     cols = col,#"jet", 
                     breaks = 9,  # Usar los breakpoints calculados
                     key.position = "right",
                     hemisphere= "southern",
                     type = "season",
                     main = "CNA",key.header = NULL)

##############################################################
##################################################################
data_velocidad_PTA <- read.csv("velViento/06-PTA_velViento.csv")
data_velocidad_PTA$date <- strptime(data_velocidad_PTA$fecha, format = "%Y-%m-%d")
data_dir_PTA <- read.csv("dirViento/06-PTA_dirViento.csv")
data_dir_PTA$date <- strptime(data_dir_PTA$fecha, format = "%Y-%m-%d")
df_merge_PTA <- merge(x = data_dir_PTA, y = data_velocidad_PTA, by = "date") # Equivalente
df_merge_PTA <- data.frame(date= df_merge_PTA$date, dirViento = df_merge_PTA$valor.x,
                           velViento = df_merge_PTA$valor.y)
df_merge_PTA <- df_merge_PTA[complete.cases(df_merge_PTA),]
df_merge_PTA <- df_merge_PTA[year(df_merge_PTA$date) == 2015 |
                               year(df_merge_PTA$date) == 2016 |
                               year(df_merge_PTA$date) == 2017 |
                               year(df_merge_PTA$date) == 2018 |
                               year(df_merge_PTA$date) == 2019 |
                               year(df_merge_PTA$date) == 2020 |
                               year(df_merge_PTA$date) == 2021 |
                               year(df_merge_PTA$date) == 2022 |
                               year(df_merge_PTA$date) == 2023 |
                               year(df_merge_PTA$date) == 2024 ,]
# Generar la rosa de vientos
col <- c("#74ADD1" , "#E0F3F8", "#FEE090", "#F46D43" )
# Generar la rosa de vientos utilizando los intervalos de Jenks
plot_PTA <- windRose(df_merge_PTA, ws = "velViento", wd = "dirViento", 
                     angle = 45, paddle = FALSE, 
                     dig.lab = 4, # Hace referencia a los decimales de mean
                     cols = col,#"jet", 
                     breaks = 9,  # Usar los breakpoints calculados
                     key.position = "right",
                     hemisphere= "southern",
                     type = "season",
                     main = "PTA",key.header = NULL)

##############################################################
##################################################################
data_velocidad_FLD <- read.csv("velViento/05-FLD_velViento.csv")
data_velocidad_FLD$date <- strptime(data_velocidad_FLD$fecha, format = "%Y-%m-%d")
data_dir_FLD <- read.csv("dirViento/05-FLD_dirViento.csv")
data_dir_FLD$date <- strptime(data_dir_FLD$fecha, format = "%Y-%m-%d")
df_merge_FLD <- merge(x = data_dir_FLD, y = data_velocidad_FLD, by = "date") # Equivalente
df_merge_FLD <- data.frame(date= df_merge_FLD$date, dirViento = df_merge_FLD$valor.x,
                           velViento = df_merge_FLD$valor.y)
df_merge_FLD <- df_merge_FLD[complete.cases(df_merge_FLD),]
df_merge_FLD <- df_merge_FLD[year(df_merge_FLD$date) == 2015 |
                               year(df_merge_FLD$date) == 2016 |
                               year(df_merge_FLD$date) == 2017 |
                               year(df_merge_FLD$date) == 2018 |
                               year(df_merge_FLD$date) == 2019 |
                               year(df_merge_FLD$date) == 2020 |
                               year(df_merge_FLD$date) == 2021 |
                               year(df_merge_FLD$date) == 2022 |
                               year(df_merge_FLD$date) == 2023 |
                               year(df_merge_FLD$date) == 2024 ,]
# Generar la rosa de vientos
col <- c("#74ADD1" , "#E0F3F8", "#FEE090", "#F46D43" )
# Generar la rosa de vientos utilizando los intervalos de Jenks
plot_FLD <- windRose(df_merge_FLD, ws = "velViento", wd = "dirViento", 
                     angle = 45, paddle = FALSE, 
                     dig.lab = 4, # Hace referencia a los decimales de mean
                     cols = col,#"jet", 
                     breaks = 9,  # Usar los breakpoints calculados
                     key.position = "right",
                     hemisphere= "southern",
                     type = "season",
                     main = "FLD",key.header = NULL)

##############################################################
##################################################################
data_velocidad_CDE <- read.csv("velViento/07-CDE_velViento.csv")
data_velocidad_CDE$date <- strptime(data_velocidad_CDE$fecha, format = "%Y-%m-%d")
data_dir_CDE <- read.csv("dirViento/07-CDE_dirViento.csv")
data_dir_CDE$date <- strptime(data_dir_CDE$fecha, format = "%Y-%m-%d")
df_merge_CDE <- merge(x = data_dir_CDE, y = data_velocidad_CDE, by = "date") # Equivalente
df_merge_CDE <- data.frame(date= df_merge_CDE$date, dirViento = df_merge_CDE$valor.x,
                           velViento = df_merge_CDE$valor.y)
df_merge_CDE <- df_merge_CDE[complete.cases(df_merge_CDE),]
df_merge_CDE <- df_merge_CDE[year(df_merge_CDE$date) == 2015 |
                               year(df_merge_CDE$date) == 2016 |
                               year(df_merge_CDE$date) == 2017 |
                               year(df_merge_CDE$date) == 2018 |
                               year(df_merge_CDE$date) == 2019 |
                               year(df_merge_CDE$date) == 2020 |
                               year(df_merge_CDE$date) == 2021 |
                               year(df_merge_CDE$date) == 2022 |
                               year(df_merge_CDE$date) == 2023 |
                               year(df_merge_CDE$date) == 2024 ,]
# Generar la rosa de vientos
col <- c("#74ADD1" , "#E0F3F8", "#FEE090", "#F46D43" )
# Generar la rosa de vientos utilizando los intervalos de Jenks
plot_CDE <- windRose(df_merge_CDE, ws = "velViento", wd = "dirViento", 
                     angle = 45, paddle = FALSE, 
                     dig.lab = 4, # Hace referencia a los decimales de mean
                     cols = col,#"jet", 
                     breaks = 9,  # Usar los breakpoints calculados
                     key.position = "right",
                     # type="season",
                     hemisphere= "southern",
                     type = "season",
                     main = "CDE",key.header = NULL)


####################################
# Guardar gráficos como archivos PNG
dir <- "D:/Josefina/Proyectos/ProyectoChile/plots/Viento/"
#### BSQ
png(paste(dir,"plot_BSQ_season.png",sep=""), width = 4000, height = 3000, res = 500)
print(plot_BSQ)
dev.off()


#### OHG
png(paste(dir,"plot_OHG_season.png",sep=""), width = 4000, height = 3000, res = 500)
print(plot_OHG)
dev.off()


#### FLD
png(paste(dir,"plot_FLD_season.png",sep=""), width = 4000, height = 3000, res = 500)
print(plot_FLD)
dev.off()


#### CNA
png(paste(dir,"plot_CNA_season.png",sep=""), width = 4000, height = 3000, res = 500)
print(plot_CNA)
dev.off()


#### PTA
png(paste(dir,"plot_PTA_season.png",sep=""), width = 4000, height = 3000, res = 500)
print(plot_PTA)
dev.off()


#### PDH
png(paste(dir,"plot_PDH_season.png",sep=""), width = 4000, height = 3000, res = 500)
print(plot_PDH)
dev.off()

#### CDE
png(paste(dir,"plot_CDE_season.png",sep=""), width = 4000, height = 3000, res = 500)
print(plot_CDE)
dev.off()

########################################################
##########################################################
df_merge_OHG$estacion <- "OHG"
df_merge_BSQ$estacion <- "BSQ"
df_merge_CDE$estacion <- "CDE"
df_merge_PTA$estacion <- "PTA"
df_merge_PDH$estacion <- "PDH"
df_merge_FLD$estacion <- "FLD"
df_merge_CDE$estacion <- "CDE"
merge_estaciones <- rbind(df_merge_OHG,df_merge_BSQ,df_merge_CDE,
                          df_merge_PTA,df_merge_PDH,df_merge_FLD,
                          df_merge_CDE)
summary_estaciones <- merge_estaciones %>% 
  group_by(estacion) %>%
  summarise(mean_velocidad = mean(velViento, na.rm = TRUE),
            max_velocidad = max(velViento, na.rm = TRUE))

#Para ver la Escala de Beaufort  tenemos que pasar de
#m/s, km/hora
merge_estaciones$velViento_kmH <- merge_estaciones$velViento *3.6

summary_estaciones <- merge_estaciones %>% 
  group_by(estacion) %>%
  summarise(mean_velocidad = mean(velViento, na.rm = TRUE),
            max_velocidad = max(velViento, na.rm = TRUE),
            mean_velocidad_kmH = mean(velViento_kmH, na.rm = TRUE),
            max_velocidad_kmH = max(velViento_kmH, na.rm = TRUE))

###############################################################
################################################################
###################################################################
#####################################################################
#ROSA DE CONTAMINANTES
setwd("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/otras/meteo_superficial/")
data_velocidad_OHG <- read.csv("velViento/01-OHG_velViento.csv")
data_velocidad_OHG$date <- strptime(data_velocidad_OHG$fecha, format = "%Y-%m-%d")
data_dir_OHG <- read.csv("dirViento/01-OHG_dirViento.csv")
data_dir_OHG$date <- strptime(data_dir_OHG$fecha, format = "%Y-%m-%d")
data_PM_OHG <- read.csv("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/PM25/01-OHG_PM25.csv")
data_PM_OHG$date <- strptime(data_PM_OHG$fecha, format = "%Y-%m-%d")


df_merge_OHG <- merge(x = data_dir_OHG, y = data_velocidad_OHG, by = "date") # Equivalente
df_merge_OHG <- data.frame(date= df_merge_OHG$date, dirViento = df_merge_OHG$valor.x,
                           velViento = df_merge_OHG$valor.y)
df_merge_OHG$date <- strptime(df_merge_OHG$date, format = "%Y-%m-%d")
df_merge_OHG <- merge(x = df_merge_OHG, y = data_PM_OHG, by = "date") # Equivalente
df_merge_OHG <- data.frame(date= df_merge_OHG$date, wd = df_merge_OHG$dirViento,
                             ws = df_merge_OHG$velViento, PM25 = df_merge_OHG$valor)

df_merge_OHG <- df_merge_OHG[complete.cases(df_merge_OHG),]
df_merge_OHG <- df_merge_OHG[year(df_merge_OHG$date) == 2015 |
                               year(df_merge_OHG$date) == 2016 |
                               year(df_merge_OHG$date) == 2017 |
                               year(df_merge_OHG$date) == 2018 |
                               year(df_merge_OHG$date) == 2019 |
                               year(df_merge_OHG$date) == 2020 |
                               year(df_merge_OHG$date) == 2021 |
                               year(df_merge_OHG$date) == 2022 |
                               year(df_merge_OHG$date) == 2023 |
                               year(df_merge_OHG$date) == 2024 ,]
# clase <- classIntervals(sample(df_merge_OHG$PM25, size = 3100), 5, style = "jenks")
# breaks<- clase$brks
pollutionRose_OHG <- pollutionRose(df_merge_OHG, 
              pollutant = "PM25",
              hemisphere= "southern",
              #type = "season", 
              annotate = FALSE,
              grid.line = 0.5,
              normalise = TRUE, 
              angle = 45,
              # breaks = 10,#breaks,
              breaks = breaks,
              main = "OHG")

###################################################################
#####################################################################

setwd("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/otras/meteo_superficial/")
data_velocidad_BSQ <- read.csv("velViento/02-BSQ_velViento.csv")
data_velocidad_BSQ$date <- strptime(data_velocidad_BSQ$fecha, format = "%Y-%m-%d")
data_dir_BSQ <- read.csv("dirViento/02-BSQ_dirViento.csv")
data_dir_BSQ$date <- strptime(data_dir_BSQ$fecha, format = "%Y-%m-%d")
data_PM_BSQ <- read.csv("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/PM25/02-BSQ_PM25.csv")
data_PM_BSQ$date <- strptime(data_PM_BSQ$fecha, format = "%Y-%m-%d")
df_merge_BSQ <- merge(x = data_dir_BSQ, y = data_velocidad_BSQ, by = "date") # Equivalente
df_merge_BSQ <- data.frame(date= df_merge_BSQ$date, dirViento = df_merge_BSQ$valor.x,
                           velViento = df_merge_BSQ$valor.y)
df_merge_BSQ$date <- strptime(df_merge_BSQ$date, format = "%Y-%m-%d")
df_merge_BSQ <- merge(x = df_merge_BSQ, y = data_PM_BSQ, by = "date") # Equivalente
df_merge_BSQ <- data.frame(date= df_merge_BSQ$date, wd = df_merge_BSQ$dirViento,
                           ws = df_merge_BSQ$velViento, PM25 = df_merge_BSQ$valor)

df_merge_BSQ <- df_merge_BSQ[complete.cases(df_merge_BSQ),]
df_merge_BSQ <- df_merge_BSQ[year(df_merge_BSQ$date) == 2015 |
                               year(df_merge_BSQ$date) == 2016 |
                               year(df_merge_BSQ$date) == 2017 |
                               year(df_merge_BSQ$date) == 2018 |
                               year(df_merge_BSQ$date) == 2019 |
                               year(df_merge_BSQ$date) == 2020 |
                               year(df_merge_BSQ$date) == 2021 |
                               year(df_merge_BSQ$date) == 2022 |
                               year(df_merge_BSQ$date) == 2023 |
                               year(df_merge_BSQ$date) == 2024 ,]
# clase <- classIntervals(sample(df_merge_BSQ$PM25, size = 3100), 5, style = "jenks")
# breaks<- clase$brks
pollutionRose_BSQ <- pollutionRose(df_merge_BSQ, 
                                   pollutant = "PM25",
                                   hemisphere= "southern",
                                   # type = "season", 
                                   annotate = FALSE,
                                   grid.line = 0.5,
                                   normalise = TRUE, 
                                   angle = 45,
                                   # breaks = 10,#breaks,
                                   breaks = breaks,
                                   main = "BSQ")

###################################################################
#####################################################################

setwd("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/otras/meteo_superficial/")
data_velocidad_CNA <- read.csv("velViento/03-CNA_velViento.csv")
data_velocidad_CNA$date <- strptime(data_velocidad_CNA$fecha, format = "%Y-%m-%d")
data_dir_CNA <- read.csv("dirViento/03-CNA_dirViento.csv")
data_dir_CNA$date <- strptime(data_dir_CNA$fecha, format = "%Y-%m-%d")
data_PM_CNA <- read.csv("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/PM25/03-CNA_PM25.csv")
data_PM_CNA$date <- strptime(data_PM_CNA$fecha, format = "%Y-%m-%d")
df_merge_CNA <- merge(x = data_dir_CNA, y = data_velocidad_CNA, by = "date") # Equivalente
df_merge_CNA <- data.frame(date= df_merge_CNA$date, dirViento = df_merge_CNA$valor.x,
                           velViento = df_merge_CNA$valor.y)
df_merge_CNA$date <- strptime(df_merge_CNA$date, format = "%Y-%m-%d")
df_merge_CNA <- merge(x = df_merge_CNA, y = data_PM_CNA, by = "date") # Equivalente
df_merge_CNA <- data.frame(date= df_merge_CNA$date, wd = df_merge_CNA$dirViento,
                           ws = df_merge_CNA$velViento, PM25 = df_merge_CNA$valor)

df_merge_CNA <- df_merge_CNA[complete.cases(df_merge_CNA),]
df_merge_CNA <- df_merge_CNA[year(df_merge_CNA$date) == 2015 |
                               year(df_merge_CNA$date) == 2016 |
                               year(df_merge_CNA$date) == 2017 |
                               year(df_merge_CNA$date) == 2018 |
                               year(df_merge_CNA$date) == 2019 |
                               year(df_merge_CNA$date) == 2020 |
                               year(df_merge_CNA$date) == 2021 |
                               year(df_merge_CNA$date) == 2022 |
                               year(df_merge_CNA$date) == 2023 |
                               year(df_merge_CNA$date) == 2024 ,]
clase <- classIntervals(sample(df_merge_CNA$PM25, size = 2900), 5, style = "jenks")
breaks<- clase$brks
pollutionRose_CNA <- pollutionRose(df_merge_CNA, 
                                   pollutant = "PM25",
                                   hemisphere= "southern",
                                   # type = "season", 
                                   annotate = FALSE,
                                   grid.line = 0.5,
                                   normalise = TRUE, 
                                   angle = 45,
                                   breaks = breaks,
                                   main = "CNA")


###################################################################
#####################################################################

setwd("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/otras/meteo_superficial/")
data_velocidad_PDH <- read.csv("velViento/04-PDH_velViento.csv")
data_velocidad_PDH$date <- strptime(data_velocidad_PDH$fecha, format = "%Y-%m-%d")
data_dir_PDH <- read.csv("dirViento/04-PDH_dirViento.csv")
data_dir_PDH$date <- strptime(data_dir_PDH$fecha, format = "%Y-%m-%d")
data_PM_PDH <- read.csv("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/PM25/04-PDH_PM25.csv")
data_PM_PDH$date <- strptime(data_PM_PDH$fecha, format = "%Y-%m-%d")
df_merge_PDH <- merge(x = data_dir_PDH, y = data_velocidad_PDH, by = "date") # Equivalente
df_merge_PDH <- data.frame(date= df_merge_PDH$date, dirViento = df_merge_PDH$valor.x,
                           velViento = df_merge_PDH$valor.y)
df_merge_PDH$date <- strptime(df_merge_PDH$date, format = "%Y-%m-%d")
df_merge_PDH <- merge(x = df_merge_PDH, y = data_PM_PDH, by = "date") # Equivalente
df_merge_PDH <- data.frame(date= df_merge_PDH$date, wd = df_merge_PDH$dirViento,
                           ws = df_merge_PDH$velViento, PM25 = df_merge_PDH$valor)

df_merge_PDH <- df_merge_PDH[complete.cases(df_merge_PDH),]
df_merge_PDH <- df_merge_PDH[year(df_merge_PDH$date) == 2015 |
                               year(df_merge_PDH$date) == 2016 |
                               year(df_merge_PDH$date) == 2017 |
                               year(df_merge_PDH$date) == 2018 |
                               year(df_merge_PDH$date) == 2019 |
                               year(df_merge_PDH$date) == 2020 |
                               year(df_merge_PDH$date) == 2021 |
                               year(df_merge_PDH$date) == 2022 |
                               year(df_merge_PDH$date) == 2023 |
                               year(df_merge_PDH$date) == 2024 ,]
# clase <- classIntervals(sample(df_merge_PDH$PM25, size = 3100), 5, style = "jenks")
# breaks<- clase$brks
pollutionRose_PDH <- pollutionRose(df_merge_PDH, 
                                   pollutant = "PM25",
                                   hemisphere= "southern",
                                   # type = "season", 
                                   annotate = FALSE,
                                   grid.line = 0.5,
                                   normalise = TRUE, 
                                   angle = 45,
                                   # breaks = 10,#breaks,
                                   breaks = breaks,
                                   main = "PDH")


###################################################################
#####################################################################

setwd("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/otras/meteo_superficial/")
data_velocidad_FLD <- read.csv("velViento/05-FLD_velViento.csv")
data_velocidad_FLD$date <- strptime(data_velocidad_FLD$fecha, format = "%Y-%m-%d")
data_dir_FLD <- read.csv("dirViento/05-FLD_dirViento.csv")
data_dir_FLD$date <- strptime(data_dir_FLD$fecha, format = "%Y-%m-%d")
data_PM_FLD <- read.csv("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/PM25/05-FLD_PM25.csv")
data_PM_FLD$date <- strptime(data_PM_FLD$fecha, format = "%Y-%m-%d")
df_merge_FLD <- merge(x = data_dir_FLD, y = data_velocidad_FLD, by = "date") # Equivalente
df_merge_FLD <- data.frame(date= df_merge_FLD$date, dirViento = df_merge_FLD$valor.x,
                           velViento = df_merge_FLD$valor.y)
df_merge_FLD$date <- strptime(df_merge_FLD$date, format = "%Y-%m-%d")
df_merge_FLD <- merge(x = df_merge_FLD, y = data_PM_FLD, by = "date") # Equivalente
df_merge_FLD <- data.frame(date= df_merge_FLD$date, wd = df_merge_FLD$dirViento,
                           ws = df_merge_FLD$velViento, PM25 = df_merge_FLD$valor)

df_merge_FLD <- df_merge_FLD[complete.cases(df_merge_FLD),]
df_merge_FLD <- df_merge_FLD[year(df_merge_FLD$date) == 2015 |
                               year(df_merge_FLD$date) == 2016 |
                               year(df_merge_FLD$date) == 2017 |
                               year(df_merge_FLD$date) == 2018 |
                               year(df_merge_FLD$date) == 2019 |
                               year(df_merge_FLD$date) == 2020 |
                               year(df_merge_FLD$date) == 2021 |
                               year(df_merge_FLD$date) == 2022 |
                               year(df_merge_FLD$date) == 2023 |
                               year(df_merge_FLD$date) == 2024 ,]
# clase <- classIntervals(sample(df_merge_FLD$PM25, size = 3100), 5, style = "jenks")
# breaks<- clase$brks
pollutionRose_FLD <- pollutionRose(df_merge_FLD, 
                                   pollutant = "PM25",
                                   hemisphere= "southern",
                                   # type = "season", 
                                   annotate = FALSE,
                                   grid.line = 0.5,
                                   normalise = TRUE, 
                                   angle = 45,
                                   # breaks = 10,#breaks,
                                   breaks = breaks,
                                   main = "FLD")

###################################################################
#####################################################################

setwd("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/otras/meteo_superficial/")
data_velocidad_PTA <- read.csv("velViento/06-PTA_velViento.csv")
data_velocidad_PTA$date <- strptime(data_velocidad_PTA$fecha, format = "%Y-%m-%d")
data_dir_PTA <- read.csv("dirViento/06-PTA_dirViento.csv")
data_dir_PTA$date <- strptime(data_dir_PTA$fecha, format = "%Y-%m-%d")
data_PM_PTA <- read.csv("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/PM25/06-PTA_PM25.csv")
data_PM_PTA$date <- strptime(data_PM_PTA$fecha, format = "%Y-%m-%d")
df_merge_PTA <- merge(x = data_dir_PTA, y = data_velocidad_PTA, by = "date") # Equivalente
df_merge_PTA <- data.frame(date= df_merge_PTA$date, dirViento = df_merge_PTA$valor.x,
                           velViento = df_merge_PTA$valor.y)
df_merge_PTA$date <- strptime(df_merge_PTA$date, format = "%Y-%m-%d")
df_merge_PTA <- merge(x = df_merge_PTA, y = data_PM_PTA, by = "date") # Equivalente
df_merge_PTA <- data.frame(date= df_merge_PTA$date, wd = df_merge_PTA$dirViento,
                           ws = df_merge_PTA$velViento, PM25 = df_merge_PTA$valor)

df_merge_PTA <- df_merge_PTA[complete.cases(df_merge_PTA),]
df_merge_PTA <- df_merge_PTA[year(df_merge_PTA$date) == 2015 |
                               year(df_merge_PTA$date) == 2016 |
                               year(df_merge_PTA$date) == 2017 |
                               year(df_merge_PTA$date) == 2018 |
                               year(df_merge_PTA$date) == 2019 |
                               year(df_merge_PTA$date) == 2020 |
                               year(df_merge_PTA$date) == 2021 |
                               year(df_merge_PTA$date) == 2022 |
                               year(df_merge_PTA$date) == 2023 |
                               year(df_merge_PTA$date) == 2024 ,]
# clase <- classIntervals(sample(df_merge_PTA$PM25, size = 3100), 5, style = "jenks")
# breaks<- clase$brks
pollutionRose_PTA <- pollutionRose(df_merge_PTA, 
                                   pollutant = "PM25",
                                   hemisphere= "southern",
                                   # type = "season", 
                                   annotate = FALSE,
                                   grid.line = 0.5,
                                   normalise = TRUE, 
                                   angle = 45,
                                   # breaks = 10,#breaks,
                                   breaks = breaks,
                                   main = "PTA")


###################################################################
#####################################################################

setwd("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/otras/meteo_superficial/")
data_velocidad_CDE <- read.csv("velViento/07-CDE_velViento.csv")
data_velocidad_CDE$date <- strptime(data_velocidad_CDE$fecha, format = "%Y-%m-%d")
data_dir_CDE <- read.csv("dirViento/07-CDE_dirViento.csv")
data_dir_CDE$date <- strptime(data_dir_CDE$fecha, format = "%Y-%m-%d")
data_PM_CDE <- read.csv("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/PM25/07-CDE_PM25.csv")
data_PM_CDE$date <- strptime(data_PM_CDE$fecha, format = "%Y-%m-%d")
df_merge_CDE <- merge(x = data_dir_CDE, y = data_velocidad_CDE, by = "date") # Equivalente
df_merge_CDE <- data.frame(date= df_merge_CDE$date, dirViento = df_merge_CDE$valor.x,
                           velViento = df_merge_CDE$valor.y)
df_merge_CDE$date <- strptime(df_merge_CDE$date, format = "%Y-%m-%d")
df_merge_CDE <- merge(x = df_merge_CDE, y = data_PM_CDE, by = "date") # Equivalente
df_merge_CDE <- data.frame(date= df_merge_CDE$date, wd = df_merge_CDE$dirViento,
                           ws = df_merge_CDE$velViento, PM25 = df_merge_CDE$valor)

df_merge_CDE <- df_merge_CDE[complete.cases(df_merge_CDE),]
df_merge_CDE <- df_merge_CDE[year(df_merge_CDE$date) == 2015 |
                               year(df_merge_CDE$date) == 2016 |
                               year(df_merge_CDE$date) == 2017 |
                               year(df_merge_CDE$date) == 2018 |
                               year(df_merge_CDE$date) == 2019 |
                               year(df_merge_CDE$date) == 2020 |
                               year(df_merge_CDE$date) == 2021 |
                               year(df_merge_CDE$date) == 2022 |
                               year(df_merge_CDE$date) == 2023 |
                               year(df_merge_CDE$date) == 2024 ,]
# clase <- classIntervals(sample(df_merge_CDE$PM25, size = 3100), 5, style = "jenks")
# breaks<- clase$brks
pollutionRose_CDE <- pollutionRose(df_merge_CDE, 
                                   pollutant = "PM25",
                                   hemisphere= "southern",
                                   # type = "season", 
                                   annotate = FALSE,
                                   grid.line = 0.5,
                                   normalise = TRUE, 
                                   angle = 45,
                                   # breaks = 10,#breaks,
                                   breaks = breaks,
                                   main = "CDE")



####################################
# Guardar gráficos como archivos PNG
dir <- "D:/Josefina/Proyectos/ProyectoChile/plots/pollutionRose/"
#### BSQ
png(paste(dir,"pollutionRose_BSQ.png",sep=""), width = 4000, height = 3000, res = 500)
print(pollutionRose_BSQ)
dev.off()


#### OHG
png(paste(dir,"pollutionRose_OHG.png",sep=""), width = 4000, height = 3000, res = 500)
print(pollutionRose_OHG)
dev.off()


#### FLD
png(paste(dir,"pollutionRose_FLD.png",sep=""), width = 4000, height = 3000, res = 500)
print(pollutionRose_FLD)
dev.off()


#### CNA
png(paste(dir,"pollutionRose_CNA.png",sep=""), width = 4000, height = 3000, res = 500)
print(pollutionRose_CNA)
dev.off()


#### PTA
png(paste(dir,"pollutionRose_PTA.png",sep=""), width = 4000, height = 3000, res = 500)
print(pollutionRose_PTA)
dev.off()


#### PDH
png(paste(dir,"pollutionRose_PDH.png",sep=""), width = 4000, height = 3000, res = 500)
print(pollutionRose_PDH)
dev.off()

#### CDE
png(paste(dir,"pollutionRose_CDE.png",sep=""), width = 4000, height = 3000, res = 500)
print(pollutionRose_CDE)
dev.off()
