rm(list=ls())

library("config")
library("dplyr")
library("stringr")

source("./tidymodels/fcts_query-API-CRC.R")

# Credenciales para API del CRC-SAS
cfg <- config::get(file = "./Credentials_CRC.yml", value = "Credentials")

# Cordoba Obs ; Rio Cuarto ; Ceres ; Rosario ; Parana ; Junin
id.station <- c('87345', '87453', '87257', '87480', '87374', '87548')


for (i in 1:length(id.station)){
  
  # Consulta datos meteorológicos
  meteo.request <- ConsumirDatosEstacion(url.consulta = cfg[1],
                                         usuario = cfg[2], clave = cfg[3],
                                         fecha.inicial = '2022-07-31',
                                         fecha.final = '2025-06-14',
                                         id.estacion = id.station[i])
  # Cambio de formato a Date
  meteo.request$Fecha <- as.Date(meteo.request$Fecha)
  
  # Consulta datos BHOA
  bhoa.files <- list.files("./tidymodels/bhoa/")
  bhoa.files <- tibble(nombre = bhoa.files)
  bhoa.station <- bhoa.files %>%
    filter(str_starts(nombre, id.station[i])) %>%
    as.character()
  bhoa.station <- read.csv(paste0("./tidymodels/bhoa/", bhoa.station))
  
  # Cambio de formato a Date
  bhoa.station$Fecha <- as.Date(bhoa.station$Fecha)
  
  # Datos meteorológicos y BHOA unificados en única variable
  data.station <- base::merge(meteo.request, bhoa.station, by = "Fecha")
  print(head(data.station))
  
}

