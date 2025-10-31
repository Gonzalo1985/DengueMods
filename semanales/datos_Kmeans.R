rm(list=ls())

path.ppal <- "./"
source(paste0(path.ppal, "fcts_query-API-CRC.R"))
source("./semanales/sezen.R")

# cambiar archivo de lectura de metadata
metadata <- read.csv(paste0(path.ppal, "ESTACIONES_SMN.csv"), skip = 1, header = F)
metadata <- metadata[-59,]
rownames(metadata) <- seq(1, 105, 1)

# ------------------------------------------------------------------------------
# Armado de tabla de datos
cfg <- config::get(file = "./Credentials_CRC.yml", value = "Credentials")

meteo.request <- ConsumirDatosEstacion(url.consulta = cfg[1],
                                       usuario = cfg[2], clave = cfg[3],
                                       fecha.inicial = '2025-06-22',
                                       fecha.final = '2025-07-19',
                                       id.estacion = metadata$V3)

# armar vector en función a cantidad de semanas * cantidad de estaciones
meteo.request$Semana <- sort(rep(1:420, 7))

prcp.semanal <- meteo.request %>% 
  group_by(Semana) %>%
  summarise(prcp = sum(prcp, na.rm = TRUE))

tmax.semanal <- meteo.request %>% 
  group_by(Semana) %>%
  summarise(tmax = mean(tmax))

tmin.semanal <- meteo.request %>% 
  group_by(Semana) %>%
  summarise(tmin = mean(tmin))

# reemplazar esto por función Sem_Epid
Semana <- rep(c("26/25", "27/25", "28/25", "29/25"), 105)

aux.final <- c()
for (i in 1:105) {aux <- rep(metadata$V3[i], 4) ; aux.final <- c(aux.final, aux)}


tabla.final <- data.frame(
  nro.estacion = aux.final,
  SE = Semana,
  prcp = prcp.semanal$prcp, tmax = tmax.semanal$tmax, tmin = tmin.semanal$tmin
)

print.to.file(tabla.final, "./semanales/datosSMN_Semana.txt")
