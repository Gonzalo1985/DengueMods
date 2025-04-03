rm(list=ls())

path.ppal <- "./"
source(paste0(path.ppal, "fcts_query-API-CRC.R"))
source("/home/gdiaz/Documentos/funciones-R/sezen.R")


metadata <- read.csv(paste0(path.ppal, "ESTACIONES_SMN.csv"), skip = 1, header = F)
metadata <- metadata[-59,]
rownames(metadata) <- seq(1, 105, 1)

# ------------------------------------------------------------------------------
# Armado de tabla de datos
cfg <- config::get(file = "./Credentials_CRC.yml", value = "Credentials")

meteo.request <- ConsumirDatosEstacion(url.consulta = cfg[1],
                                       usuario = cfg[2], clave = cfg[3],
                                       fecha.inicial = '2025-03-02',
                                       fecha.final = '2025-03-29',
                                       id.estacion = metadata$V3)

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

Semana <- rep(c("10/25", "11/25", "12/25", "13/25"), 105)

aux.final <- c()
for (i in 1:105) {aux <- rep(metadata$V3[i], 4) ; aux.final <- c(aux.final, aux)}


tabla.final <- data.frame(
  nro.estacion = aux.final,
  SE = Semana,
  prcp = prcp.semanal$prcp, tmax = tmax.semanal$tmax, tmin = tmin.semanal$tmin
)

print.to.file(tabla.final, "datosSMN_Semana.txt")
