rm(list=ls())

library('readxl')
library('googledrive')
library('stringr')
library('dplyr')

source("./tidymodels/fcts/load-epidemio-data.R")
source("./tidymodels/fcts/query-API-CRC.R")

# Credenciales para API del CRC-SAS
cfg <- config::get(file = "./Credentials_CRC.yml", value = "Credentials")

# Elección de región
region <- "PAMPEANA" # CENTRO, CUYO, NEA, NOA, PAMPEANA, PATAGONIA

reg.x.sta <- readxl::read_excel("tidymodels/ESTACIONES_SMN_Regiones_v5.0.xls",
                                sheet = region)

# Se abren los datos epidemiológicos de todas las temporadas
ola.19.23 <- load.epidemio.data(wave = "19-23", week = 30)
ola.23.24 <- load.epidemio.data(wave = "23-24", week = 30)
ola.24.25 <- load.epidemio.data(wave = "24-25", week = 30)

olas.anteriores <- rbind(ola.19.23, ola.23.24, ola.24.25)


for (i in 1:nrow(reg.x.sta))
{
  # Códigos de Departamentos asociados a la estación de referencia
  cod.depto <- reg.x.sta[i, ] %>%
    unlist() %>%
    as.character() %>%
    .[-c(1, 2, 3)] %>%
    .[. != "NA"]
 
  # Números de filas de los Departamentos en la variable olas anteriores
  filas.deptos <- which(olas.anteriores$COD_DEPTO %in% cod.depto == TRUE)
  
  # Casos de los Departamentos asociados a la estación de referencia
  olas.por.deptos <- olas.anteriores[filas.deptos,]
  
  # Agrupado de todos los datos por SE por departamento (Autóctonos y Totales)
  casos.agrupados <- olas.por.deptos %>%
    group_by(ANIO_SEPI_MIN) %>%
    summarise(
      Autóctono = sum(Autóctono, na.rm = TRUE),
      Total = sum(Total_confirmados, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Consulta datos meteorológicos
  meteo.request <- ConsumirDatosEstacion(url.consulta = cfg[1],
                                         usuario = cfg[2], clave = cfg[3],
                                         fecha.inicial = '2022-07-31',
                                         fecha.final = '2025-07-26',
                                         id.estacion = reg.x.sta$NRO_ESTACION[i])
  
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
  
  
  
  print(nrow(casos.agrupados))
}
