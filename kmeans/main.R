rm(list=ls())
library('lubridate')
library('dplyr')
library('tidyr')
library('ggplot2')
library('sf')

var.kmeans <- "" # variable que define si trabajar con BHOA o meteo
source("./fcts_query-API-CRC.R")
source("./kmeans/Sem_Epid.R")
source("./kmeans/kmeans_espacial.R")

# ---------------------------------------------------------------------------- #
# Abre y post procesa los datos a los cuales calcular kmeans
data <- read.csv("./kmeans/ETP-ETR-ALM.csv", skip = 1, header = FALSE)
data <- data[,-6]
colnames(data) <- c("Nint", "Fecha", "ETP", "ETR", "ALM")

data <- data %>%
  filter(!Fecha %in% as.character(seq(as.Date("1989-11-01"),
                                     as.Date("1989-12-30"),
                                     by = "1 day")))

data <- data %>%
  filter(!Fecha %in% as.character(seq(as.Date("2024-12-29"),
                                      as.Date("2024-12-31"),
                                      by = "1 day")))


cfg <- config::get(file = "./Credentials_CRC.yml", value = "Credentials")

#meteo.request <- ConsumirDatosEstacion(url.consulta = cfg[1],
#                                       usuario = cfg[2], clave = cfg[3],
#                                       fecha.inicial = '1989-12-31',
#                                       fecha.final = '2024-12-28',
#                                       id.estacion = unique(data$Nint))

if (var.kmeans == 'meteo') {data <- meteo.request}
# ------------------------------------------------------------------------------

# ---------------------------------------------------------------------------- #
# Armado de vector de Semanas Epidemiológicas ----
Sem.Epid <- Sem_Epid()
Pos <- which(Sem.Epid == "52/1992" | Sem.Epid == "52/1998" |
             Sem.Epid == "52/2004" | Sem.Epid == "52/2009" |
             Sem.Epid == "52/2015" | Sem.Epid == "52/2020")

Sem.Epid.new <- Sem.Epid  # copiamos el vector original

# Se agrega un NA después de cada semana donde debiera existir SE53
for (p in rev(Pos)) {
  Sem.Epid.new <- append(Sem.Epid.new, NA, after = p)
}

# Se completa el NA con Semana 53 para esos años
Sem.Epid.new[which(is.na(Sem.Epid.new))] <- c("53/1992", "53/1998", "53/2004",
                                              "53/2009", "53/2015", "53/2020")
# ------------------------------------------------------------------------------

# Número total de semanas considerando TODAS las estaciones
Total.Weeks <- nrow(data) / 7

# Se crea indicador de semana como columna en data
data$ind.sem <- sort(rep(1:Total.Weeks, 7))

# Vector con números internacionales de estaciones
NroInt <- unique(data$Nint)

# for que calcula los datos semanales a partir de los diarios
for (i in 1:length(NroInt)){
  
  data.by.station <- data[which(data$Nint == NroInt[i]), ]
  
  nro.rows.NA <-12782 - nrow(data.by.station)
  
  
  data.to.append <- c(data.by.station$ALM, rep(NA, nro.rows.NA))
  
  #data.aux <- data.by.station %>%
  #  group_by(ind.sem) %>%
  #  summarise(ALM = mean(ALM, na.rm = TRUE))
  
  if (i == 1) 
    {#data.final <- data.frame(Sem.Epid.new, data.to.append)
     data.final <- data.frame(1:12782, data.to.append)} else
    {data.final <- cbind(data.final, data.to.append)}
  
}
colnames(data.final) <- c("SE", NroInt)

# Ejecuta kmeans
salida <- kmeans.espacial(data.final,
                          "./kmeans/METADATA_Kmeans.csv",
                          k = 10)

print(salida)



