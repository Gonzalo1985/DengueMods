rm(list=ls())

library('readxl')
library('googledrive')
library('stringr')
library('dplyr')
library('tibble')
library('zoo')

source("./tidymodels/fcts/load-epidemio-data.R")
source("./tidymodels/fcts/load-meteo-data.R")
source("./tidymodels/fcts/query-API-CRC.R")

# Credenciales para API del CRC-SAS
cfg <- config::get(file = "./Credentials_CRC.yml", value = "Credentials")

# Elección de región y variable final
data.training.region <- c()
region <- "PATAGONIA" # CENTRO, CUYO, NEA, NOA, PAMPEANA, PATAGONIA

# Abre archivo con estaciones y regiones asociadas a cada estación
reg.x.sta <- readxl::read_excel("tidymodels/ESTACIONES_SMN_Regiones_v5.0.xls",
                                sheet = region)

# Abre datos BHOA de todas las estaciones
bhoa.data <- read.csv(
  "./tidymodels/data/ETP-ETR-ALM.csv",
  header = TRUE
)
bhoa.data$Fecha <- as.Date(bhoa.data$Fecha)

# Se abren los datos epidemiológicos de todas las temporadas
ola.19.23 <- load.epidemio.data(wave = "19-23", week = 30)
ola.23.24 <- load.epidemio.data(wave = "23-24", week = 30)
ola.24.25 <- load.epidemio.data(wave = "24-25", week = 30)
olas.training <- rbind(ola.19.23, ola.23.24, ola.24.25)
rm(ola.19.23) ; rm(ola.23.24) ; rm(ola.24.25)

reg.x.sta <- reg.x.sta[-2,]


for (i in 1:nrow(reg.x.sta))
{
  # Códigos de Departamentos asociados a la estación de referencia
  cod.depto <- reg.x.sta[i, ] %>%
    unlist() %>%
    as.character() %>%
    .[-c(1, 2, 3)] %>%
    .[. != "NA"]
 
  # Números de filas de los Departamentos en la variable olas anteriores
  filas.deptos <- which(olas.training$COD_DEPTO %in% cod.depto == TRUE)
  
  # Casos de los Departamentos asociados a la estación de referencia
  olas.por.deptos <- olas.training[filas.deptos,]
  
  # Agrupado de todos los datos por SE por departamento (Autóctonos y Totales)
  olas.agrupadas <- olas.por.deptos %>%
    group_by(ANIO_SEPI_MIN) %>%
    summarise(
      Autóctono = sum(Autóctono, na.rm = TRUE),
      Total = sum(Total_confirmados, na.rm = TRUE),
      .groups = "drop"
    )
  olas.agrupadas <- olas.agrupadas[which(olas.agrupadas$ANIO_SEPI_MIN == "22/31"):
                                     which(olas.agrupadas$ANIO_SEPI_MIN == "25/30"),
                                   ]
  
  
  # Armado de base de datos meteorológicos y de bhoa
  data.meteo.station <- load.meteo.data(initial.date = '2022-07-31',
                                        final.date = '2025-07-26',
                                        url.serv = cfg[1],
                                        user = cfg[2],
                                        password = cfg[3],
                                        nro.station = reg.x.sta$NRO_ESTACION[i],
                                        bhoa.table = bhoa.data)
  print(nrow(data.meteo.station))
  
  # Armado de base semanal de datos meteorológicos y de bhoa
  data.meteo.station$Semana <- sort(rep(1:156,7))
  
  data.meteo.grouped <- data.meteo.station %>%
    group_by(Semana) %>%
    summarise(nro.estacion = max(nro.estacion),
              ETP = mean(ETP.mm., na.rm = TRUE),
              ETR = mean(ETR.mm., na.rm = TRUE),
              ALM = mean(ALM.mm., na.rm = TRUE),
              prcp = sum(prcp, na.rm = TRUE),
              prcp.1m = sum(Prcp.1m, na.rm = TRUE),
              tmin = mean(tmin, na.rm = TRUE),
              hr = mean(hr, na.rm = TRUE),
              tmin.count.4d = sum(Tmin.count.4d, na.rm = TRUE),
              tmin.count.7d = sum(Tmin.count.7d, na.rm = TRUE))
  
  # Unificación de bases meteo/bhoa con epidemiológica
  data.epidemio.meteo <- cbind(
    olas.agrupadas,
    data.meteo.grouped[,2:11]
    )
  colnames(data.epidemio.meteo)[1] <- "Semana.Obs.Epidemio"

  # Armado de base lagueada
  data.lagged <- cbind(
    olas.agrupadas[,c(1,2)],
    c(NA, NA, olas.agrupadas$Total[1:154]),
    data.meteo.grouped$nro.estacion,
    rbind(NA,NA, data.meteo.grouped[1:154,3:11])
  )
  colnames(data.lagged)[c(1,3)] <- c("Semana.Obs.Epidemio", "Total")
  colnames(data.lagged)[4] <- "nro.estacion"
  
  # Base final para entrenar por región
  data.training.region <- rbind(data.training.region, data.lagged)
  data.training.region[data.training.region == -99.9] <- NA
}



