rm(list = ls())

library("readxl")
library("vroom")
library("dplyr")
library("aws.wrfsmn")
library("ggplot2")
library("reshape2")
library("zoo")
library("lubridate")
library("randomForest")
library("e1071")
library("caret")

path.ppal <- "./"
source(paste0(path.ppal, "fcts_datatable-models.R"))
source(paste0(path.ppal, "fcts_query-API-CRC.R"))

station.number <- 87155

Prcp.1m.Tucu <- c(0.0, 3.1, 3.1, 7.8, 7.8, 4.8, 4.7, 1.7, 11.0, 11.0, 101, 107,
                  99.0, 125, 109.1, 67.9, 69.2, 125.20, 87.90, 118.70, 114.60, 64.40, 50.60, 72.0, 48.2, 54.4, 134.4, 109.4)
Prcp.1m.Resi <- c(9.0, 58.0, 58.0, 126, 118, 116, 71.0, 71.0, 43.0, 47.0, 74.2,
                  97.4, 78.4, 74.5, 154.5, 146.6, 243.6, 345.60, 266.40, 382.70, 277.70, 234.70, 155.80, 69.0, 32.0, 0.3, 80.3, 80.3)
Prcp.1m.Cord <- c(5.9, 5.9, 5.9, 4.0, 4.0, 4.0, 4.0, 0.0, 0.0, 0.0, 34.0,
                  56.0, 125.0, 148.0, 123.0, 112.0, 71.4, 191.90, 173.90, 170.60, 185.20, 117.70, 89.70, 98.10, 72.1, 84.1, 100.1, 91.4)

# ------------------------------------------------------------------------------
# Armado de tabla de datos operativa de meteo y bhoa
meteo.request <- ConsumirDatosEstacion(url.consulta = 'https://api.crc-sas.org/ws-api',
                                       usuario = 'gdiaz', clave = 'EoNGmeDYdr',
                                       fecha.inicial = '2019-01-01',
                                       fecha.final = '2025-02-08',
                                       id.estacion = station.number)

if (station.number == 87121)
  {
   filename.bhoa  <- "87121 - Tucuman - bhoa.csv"
   data <- BASE.meteo.2(path.data = path.ppal,
                        bhoa.file = paste0("operativo/", filename.bhoa),
                        meteo.data = meteo.request)
   LOC.INDEC.number <- "90084010"
   PRCP.1m.station <- Prcp.1m.Tucu}

if (station.number == 87155)
  {filename.bhoa  <- "87155 - Resistencia - bhoa.csv"
   data <- BASE.meteo.2(path.data = path.ppal,
                        bhoa.file = paste0("operativo/", filename.bhoa),
                        meteo.data = meteo.request)
   LOC.INDEC.number <- "22140060"
   PRCP.1m.station <- Prcp.1m.Resi}

if (station.number == 87344)
  {filename.bhoa  <- "87344 - CordobaAero - bhoa.csv"
   data <- BASE.meteo.2(path.data = path.ppal,
                        bhoa.file = paste0("operativo/", filename.bhoa),
                        meteo.data = meteo.request)
   LOC.INDEC.number <- "14014010"
   PRCP.1m.station <- Prcp.1m.Cord}

colnames(data) <- c('Fecha', 'Tmin', 'Prcp', 'HR2', 'ETP', 'ETR', 'ALM')

data <- data %>% mutate(Tmin.count = rollapply(Tmin, width = 7, FUN = function(x) sum(x < 18), align = "right", fill = NA))

data.operativo <- data[which(data$Fecha >= "2024-07-28" & data$Fecha <= "2025-02-08"),]
data.operativo$Semana <- sort(c(rep(31, 7), rep(32, 7), rep(33, 7),
                                rep(34, 7), rep(35, 7), rep(36, 7), rep(37, 7),
                                rep(38, 7), rep(39, 7), rep(40, 7), rep(41, 7),
                                rep(42, 7), rep(43, 7), rep(44, 7), rep(45, 7),
                                rep(46, 7), rep(47, 7), rep(48, 7), rep(49, 7),
                                rep(50, 7), rep(51, 7), rep(52, 7), rep(1, 7),
                                rep(2, 7), rep(3, 7), rep(4, 7), rep(5, 7),
                                rep(6, 7)))
data.operativo[data.operativo == -99.9] <- NA
# ------------------------------------------------------------------------------

# Levanta datos de casos de ola anterior
cases.23.24 <- read_excel(paste0(path.ppal, "casos/Casos Temporada 23_24.xlsx"), sheet = 1)
cases.24.25 <- read_excel(paste0(path.ppal, "casos/Casos Temporada 24_25 a SE 06.xlsx"), sheet = 1)
cases.23.24[which(is.na(cases.23.24$Autóctono)), "Autóctono"] <- 0
cases.24.25[which(is.na(cases.24.25$Autóctono)), "Autóctono"] <- 0
cases <- bind_rows(cases.23.24, cases.24.25) %>% arrange(ID_LOC_INDEC_RESIDENCIA2)

cases.station <- cases[which(cases$ID_LOC_INDEC_RESIDENCIA2 == LOC.INDEC.number),]

cases.last.wave <- cases.station[, c("ANIO_SEPI_MIN", "Autóctono")]
colnames(cases.last.wave) <- c("Semana", "Casos")

cases.last.wave.complete <- cases.last.wave

inicio.ola.anterior <- which(cases.last.wave$Semana == "23/31")
final.ola.anterior <- which(cases.last.wave$Semana == "24/06")

inicio.ola.actual <- which(cases.station$ANIO_SEPI_MIN == "24/31")
final.ola.actual <- which(cases.station$ANIO_SEPI_MIN == "25/06")

cases.last.wave <- cases.last.wave[c(inicio.ola.anterior:final.ola.anterior), ]
cases.station <- cases.station[c(inicio.ola.actual:final.ola.actual), ]
# ------------------------------------------------------------------------------


prcp.station.semanal <- data.operativo %>% 
  group_by(Semana) %>%
  summarise(Suma = sum(Prcp, na.rm = TRUE))

tmin.station.semanal <- data.operativo %>% 
  group_by(Semana) %>%
  summarise(Media = mean(Tmin))

HR2.station.semanal <- data.operativo %>% 
  group_by(Semana) %>%
  summarise(Media = mean(HR2, na.rm = TRUE))

almc.station.semanal <- data.operativo %>% 
  group_by(Semana) %>%
  summarise(Media = mean(ALM, na.rm = TRUE))

etr.station.semanal <- data.operativo %>% 
  group_by(Semana) %>%
  summarise(Media = mean(ETR, na.rm = TRUE))

etp.station.semanal <- data.operativo %>% 
  group_by(Semana) %>%
  summarise(Media = mean(ETP, na.rm = TRUE))

tmin.station.count.semanal <- data.operativo %>% 
  group_by(Semana) %>%
  summarise(Media = mean(Tmin.count, na.rm = TRUE))

tabla <- data.frame(Dates = seq.Date(as.Date("2024-07-28"), as.Date("2025-02-08"), 7),
                    Semana = cases.station$ANIO_SEPI_MIN,
                    Casos.lag = c(NA, NA, cases.station$Autóctono[1:(nrow(cases.station)-2)]),
                    Casos.anterior = cases.last.wave$Casos,
                    Prcp.lag2 = prcp.station.semanal$Suma,
                    Prcp.1m.lag2 = PRCP.1m.station,
                    Tmin.lag2 = tmin.station.semanal$Media,
                    HR2.lag2 = HR2.station.semanal$Media,
                    Almc.lag2 = almc.station.semanal$Media,
                    ETR.lag2 = etr.station.semanal$Media,
                    ETP.lag2 = etp.station.semanal$Media,
                    Tmin.Count.lag2 = tmin.station.count.semanal$Media
                    )

tabla <- tabla %>% mutate(Prcp.2s.lag2 = rollapply(Prcp.lag2, width = 2, FUN = sum, align = "right", fill = NA))
tabla <- tabla[-c(1,2),]


# CARGA DE MODELOS ENTRENADOS / VERIFICADOS
mg.model <- readRDS(paste0("./models/viejos/", station.number, "_multiple_lineal_model"))
rf.model <- readRDS(paste0("./models/viejos/", station.number, "_random_forest_model"))
svm.model <- readRDS(paste0("./models/viejos/", station.number, "_svm_model"))
# ------------------------------------------------------------------------------


# ARMADO DE LA TABLA DE DATOS A GRAFICAR
data.complete <- as_tibble(c("24/35", "24/36", "24/37", "24/38",
                             "24/39", "24/40", "24/41", "24/42", "24/43", "24/44",
                             "24/45", "24/46", "24/47", "24/48", "24/49", "24/50",
                             "24/51", "24/52", "25/01", "25/02", "25/03", "25/04",
                             "25/05", "25/06", "25/07", "25/08"))
data.complete$multiple_linear <- predict(mg.model, tabla)
data.complete$random_forest <- predict(rf.model, tabla)
data.complete$support_vm <- predict(svm.model, tabla)
colnames(data.complete)[1] <- c("Semana")

data.complete[data.complete < 0] <- 0

data.complete <- merge(data.complete, cases.last.wave.complete, by = "Semana", all = TRUE)

data.complete <- data.complete[50:82, ]

data.for.print <- data.complete
data.for.print <- data.for.print[, -c(5, 6)]
data.for.print <- melt(data.for.print, id = "Semana")
# ------------------------------------------------------------------------------

fig <- ggplot() +
  
  geom_bar(data = data.for.print, aes(x = Semana, y = value, fill = variable), colour="black", position = "dodge", stat = "identity") +
  geom_line(data = data.complete, aes(x = Semana, y = Casos, group = 1), colour="#431901", size = 1.3, stat = "identity") +
  geom_point(data = data.complete, aes(x = Semana, y = Casos, group = 1), colour="#431901", stat = "identity") +
  #geom_line(data = data.complete, aes(x = Semana, y = Probables, group = 1), colour="#ffd700", size = 1.3, stat = "identity") +
  #geom_point(data = data.complete, aes(x = Semana, y = Probables, group = 1), colour="#ffd700", stat = "identity") +
  
  scale_fill_manual(values = c("multiple_linear" = "#D55E00", "random_forest" = "#0072B2", "support_vm" = "#009E73")) +
  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 16)) +
  theme(legend.text = element_text(lineheight = 13)) + 
  scale_y_continuous(limits = c(0, 5000)) +
  
  labs(x = "Semana", y = "Casos", fill = "Modelos")

print(fig)
