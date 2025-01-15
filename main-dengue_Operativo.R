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
source(paste0(path.ppal, "fcts_DENGUE.R"))

station.number <- 87155
Prcp.1m.Tucu <- c(0.0, 3.1, 3.1, 7.8, 7.8, 4.8, 4.7, 1.7, 11.0, 11.0, 101, 107,
                  99.0, 125, 109.1, 67.9, 69.2, 125.20, 87.90, 118.70, 114.60, 64.40, 50.60, 72.0)
Prcp.1m.Resi <- c(9.0, 58.0, 58.0, 126, 118, 116, 71.0, 71.0, 43.0, 47.0, 74.2,
                  97.4, 78.4, 74.5, 154.5, 146.6, 243.6, 345.60, 266.40, 382.70, 277.70, 234.70, 155.80, 69.0)

if (station.number == 87121)
  {filename.meteo <- "87121 - Tucuman - meteo.csv"
   filename.bhoa  <- "87121 - Tucuman - bhoa.csv"
   LOC.INDEC.number <- "90084010"
   PRCP.1m.station <- Prcp.1m.Tucu}

if (station.number == 87155)
  {filename.meteo <- "87155 - Resistencia - meteo.csv"
   filename.bhoa  <- "87155 - Resistencia - bhoa.csv"
   LOC.INDEC.number <- "22140060"
   PRCP.1m.station <- Prcp.1m.Resi}


# levanta datos meteo y bhoa operativos
data <- BASE.meteo(path.data = path.ppal,
                   meteo.file = paste0("operativo/", filename.meteo),
                   id.int = station.number,
                   bhoa.file = paste0("operativo/", filename.bhoa))

colnames(data) <- c('Fecha', 'Estacion', 'Tmin', 'Prcp', 'HR2', 'ETP', 'ETR', 'ALM')

data <- data %>% mutate(Tmin.count = rollapply(Tmin, width = 7, FUN = function(x) sum(x < 10), align = "right", fill = NA))

data.operativo <- data[which(data$Fecha >= "2024-07-28" & data$Fecha <= "2025-01-11"),]
data.operativo$Semana <- sort(c(rep(31, 7), rep(32, 7), rep(33, 7),
                                rep(34, 7), rep(35, 7), rep(36, 7), rep(37, 7),
                                rep(38, 7), rep(39, 7), rep(40, 7), rep(41, 7),
                                rep(42, 7), rep(43, 7), rep(44, 7), rep(45, 7),
                                rep(46, 7), rep(47, 7), rep(48, 7), rep(49, 7),
                                rep(50, 7), rep(51, 7), rep(52, 7), rep(1, 7),
                                rep(2, 7)))
# ------------------------------------------------------------------------------

# Levanta datos de casos de ola anterior
cases.23.24 <- read_excel(paste0(path.ppal, "casos/Casos Temporada 23_24.xlsx"), sheet = 1)
cases.24.25 <- read_excel(paste0(path.ppal, "casos/Casos Temporada 24_25 a SE 02.xlsx"), sheet = 1)
cases.23.24[which(is.na(cases.23.24$Autóctono)), "Autóctono"] <- 0
cases.24.25[which(is.na(cases.24.25$Autóctono)), "Autóctono"] <- 0
cases <- bind_rows(cases.23.24, cases.24.25) %>% arrange(ID_LOC_INDEC_RESIDENCIA2)

cases.station <- cases[which(cases$ID_LOC_INDEC_RESIDENCIA2 == LOC.INDEC.number),]

cases.last.wave <- cases.station[, c("ANIO_SEPI_MIN", "Autóctono")]
colnames(cases.last.wave) <- c("Semana", "Casos")

cases.last.wave.complete <- cases.last.wave

inicio.ola.anterior <- which(cases.last.wave$Semana == "23/31")
final.ola.anterior <- which(cases.last.wave$Semana == "24/02")

inicio.ola.actual <- which(cases.station$ANIO_SEPI_MIN == "24/31")
final.ola.actual <- which(cases.station$ANIO_SEPI_MIN == "25/02")

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
  summarise(Media = mean(ALM))

etr.station.semanal <- data.operativo %>% 
  group_by(Semana) %>%
  summarise(Media = mean(ETR))

etp.station.semanal <- data.operativo %>% 
  group_by(Semana) %>%
  summarise(Media = mean(ETP))

tmin.station.count.semanal <- data.operativo %>% 
  group_by(Semana) %>%
  summarise(Media = mean(Tmin.count, na.rm = TRUE))


tabla <- data.frame(Dates = seq.Date(as.Date("2024-07-28"), as.Date("2025-01-11"), 7),
                    Semana = cases.station$ANIO_SEPI_MIN,
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

# CARGA DE MODELOS ENTRENADOS / VERIFICADOS
mg.model <- readRDS(paste0("./models/", station.number, "_multiple_lineal_model"))
rf.model <- readRDS(paste0("./models/", station.number, "_random_forest_model"))
svm.model <- readRDS(paste0("./models/", station.number, "_svm_model"))
# ------------------------------------------------------------------------------


# ARMADO DE LA TABLA DE DATOS A GRAFICAR
data.complete <- as_tibble(c("24/33", "24/34", "24/35", "24/36", "24/37", "24/38",
                             "24/39", "24/40", "24/41", "24/42", "24/43", "24/44",
                             "24/45", "24/46", "24/47", "24/48", "24/49", "24/50",
                             "24/51", "24/52", "25/01", "25/02", "25/03", "25/04"))
data.complete$multiple_linear <- predict(mg.model, tabla)
data.complete$random_forest <- predict(rf.model, tabla)
data.complete$support_vm <- predict(svm.model, tabla)
colnames(data.complete)[1] <- c("Semana")

data.complete[data.complete < 0] <- 0

data.complete <- merge(data.complete, cases.last.wave.complete, by = "Semana", all = TRUE)

data.complete <- data.complete[50:78, ]

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
  
  labs(x = "Semana", y = "Casos", fill = "Modelos")

print(fig)
