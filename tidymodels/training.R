rm(list=ls())

library('readxl')
library('googledrive')
library('stringr')

source("./tidymodels/fcts/load-epidemio-data.R")

# Elección de región
region <- "NEA" # CENTRO, CUYO, NEA, NOA, PAMPEANA, PATAGONIA

reg.x.sta <- readxl::read_excel("tidymodels/ESTACIONES_SMN_Regiones_v5.0.xls",
                                sheet = region)

# Se abren los datos epidemiológicos de todas las temporadas
ola.19.23 <- load.epidemio.data(wave = "19-23", week = 30)
ola.23.24 <- load.epidemio.data(wave = "23-24", week = 30)
ola.24.25 <- load.epidemio.data(wave = "24-25", week = 30)

olas.anteriores <- rbind(ola.19.23, ola.23.24, ola.24.25)


for (i in 1:nrow(reg.x.sta))
{
  # Se guardan los códigos de departamento a la estación de referencia
  cod.depto <- reg.x.sta[i, ] %>%
    as.numeric() %>%
    .[!is.na(.)]
 
  filas.deptos <- which(olas.anteriores$COD_DEPTO %in% cod.depto == TRUE)
  
  olas.por.deptos <- olas.anteriores[filas.deptos,]
  
  casos.agrupados <- olas.por.deptos %>%
    group_by(ANIO_SEPI_MIN) %>%
    summarise(
      Autóctono = sum(Autóctono, na.rm = TRUE),
      Total = sum(Total_confirmados, na.rm = TRUE),
      .groups = "drop"
    )
   
  print(nrow(casos.agrupados))
}
