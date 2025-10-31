rm(list=ls())

path.ppal <- "./"
source(paste0(path.ppal, "fcts_query-API-CRC.R"))
source("./semanales/sezen.R")


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

#meteo.request <- ConsumirDatosEstacion(url.consulta = cfg[1],
#                                       usuario = cfg[2], clave = cfg[3],
#                                       fecha.inicial = '2024-09-01',
#                                       fecha.final = '2025-05-03',
#                                       id.estacion = metadata$V3)

meteo.request$Semana <- sort(rep(1:420, 7))
#meteo.request$Semana <- sort(rep(1:3675, 7))

prcp.semanal <- meteo.request %>% 
  group_by(Semana) %>%
  summarise(prcp = sum(prcp, na.rm = TRUE))

tmax.semanal <- meteo.request %>% 
  group_by(Semana) %>%
  summarise(tmax = mean(tmax))

tmin.semanal <- meteo.request %>% 
  group_by(Semana) %>%
  summarise(tmin = mean(tmin))


Semana <- rep(c("26/25", "27/25", "28/25", "29/25"), 105)

#aux.1 <- c(seq(36,52,1), "01", "02", "03", "04", "05", "06", "07", "08", "09", seq(10,18,1))
#aux.2 <- c(rep(24,17), rep(25,18))
#aux.3 <- paste0(aux.1, "/", aux.2)
#Semana <- rep(aux.3, 105)


aux.final <- c()
for (i in 1:105) {aux <- rep(metadata$V3[i], 4) ; aux.final <- c(aux.final, aux)}

#aux.final <- c()
#for (i in 1:105) {aux <- rep(metadata$V3[i], 35) ; aux.final <- c(aux.final, aux)}



tabla.final <- data.frame(
  nro.estacion = aux.final,
  SE = Semana,
  prcp = prcp.semanal$prcp, tmax = tmax.semanal$tmax, tmin = tmin.semanal$tmin
)

print.to.file(tabla.final, "./semanales/datosSMN_Semana.txt")
