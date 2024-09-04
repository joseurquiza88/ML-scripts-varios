
# Tendencia de viento
library(ggplot2)
library(reshape2)
library(dplyr)
library(lubridate)
library(RColorBrewer)
library(openair)
library(classInt)
setwd("D:/Josefina/Proyectos/ProyectoChile/dataset/proceed/otras/meteo_superficial/")
data_velocidad_OHG <- read.csv("velViento/02-OHG_velViento.csv")
data_velocidad_OHG$date <- strptime(data_velocidad_OHG$fecha, format = "%Y-%m-%d")
data_dir_OHG <- read.csv("dirViento/02-OHG_dirViento.csv")
data_dir_OHG$date <- strptime(data_dir_OHG$fecha, format = "%Y-%m-%d")
df_merge_OHG <- merge(x = data_dir_OHG, y = data_velocidad_OHG, by = "date") # Equivalente
df_merge_OHG <- data.frame(date= df_merge_OHG$date, dirViento = df_merge_OHG$valor.x,
                       velViento = df_merge_OHG$valor.y)
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
plot_OHG <- windRose(df_merge_OHG, ws = "velViento", wd = "dirViento", 
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
data_velocidad_BSQ <- read.csv("velViento/01-BSQ_velViento.csv")
data_velocidad_BSQ$date <- strptime(data_velocidad_BSQ$fecha, format = "%Y-%m-%d")
data_dir_BSQ <- read.csv("dirViento/01-BSQ_dirViento.csv")
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
data_velocidad_PDH <- read.csv("velViento/03-PDH_velViento.csv")
data_velocidad_PDH$date <- strptime(data_velocidad_PDH$fecha, format = "%Y-%m-%d")
data_dir_PDH <- read.csv("dirViento/03-PDH_dirViento.csv")
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
data_velocidad_CNA <- read.csv("velViento/04-CNA_velViento.csv")
data_velocidad_CNA$date <- strptime(data_velocidad_CNA$fecha, format = "%Y-%m-%d")
data_dir_CNA <- read.csv("dirViento/04-CNA_dirViento.csv")
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
data_velocidad_PTA <- read.csv("velViento/05-PTA_velViento.csv")
data_velocidad_PTA$date <- strptime(data_velocidad_PTA$fecha, format = "%Y-%m-%d")
data_dir_PTA <- read.csv("dirViento/05-PTA_dirViento.csv")
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
data_velocidad_FLD <- read.csv("velViento/06-FLD_velViento.csv")
data_velocidad_FLD$date <- strptime(data_velocidad_FLD$fecha, format = "%Y-%m-%d")
data_dir_FLD <- read.csv("dirViento/06-FLD_dirViento.csv")
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

