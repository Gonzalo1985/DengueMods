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

# levanta datos meteo y bhoa operativos
data <- BASE.meteo(path.data = path.ppal,
                   meteo.file = "operativo/87121 - Tucuman - meteo.csv",
                   id.int = 87121,
                   bhoa.file = "operativo/87121 - Tucuman - bhoa.csv")

colnames(data) <- c('Fecha', 'Estacion', 'Tmin', 'Prcp', 'HR2', 'ETP', 'ETR', 'ALM')

data <- data %>% mutate(Tmin.count = rollapply(Tmin, width = 7, FUN = function(x) sum(x < 10), align = "right", fill = NA))

data.operativo <- data[which(data$Fecha >= "2024-08-18" & data$Fecha <= "2024-10-19"),]
data.operativo$Semana <- sort(c(rep(34, 7), rep(35, 7), rep(36, 7), rep(37, 7),
                                rep(38, 7), rep(39, 7), rep(40, 7), rep(41, 7),
                                rep(42, 7)))
# ------------------------------------------------------------------------------

# Levanta datos de casos de ola anterior
cases <- read_excel(paste0(path.ppal, "casos/Casos Temporada 23_24.xlsx"), sheet = 1)
cases[which(is.na(cases$Autóctono)), "Autóctono"] <- 0
cases.station <- cases[which(cases$ID_LOC_INDEC_RESIDENCIA2 == "90084010"),]  # TUCUMAN
#cases.station <- cases[which(cases$ID_LOC_INDEC_RESIDENCIA2 == "22140060"),]  # RESISTENCIA

cases.last.wave <- cases.station[, c("ANIO_SEPI_MIN", "Autóctono")]
colnames(cases.last.wave) <- c("Semana", "Casos")
missing.SE <- data.frame(Semana = c("24/31", "24/32", "24/33", "24/34", "24/35"),
                         Casos = NA)
cases.last.wave <- rbind(cases.last.wave, missing.SE)

inicio.ola.2 <- which(cases.station$ANIO_SEPI_MIN == "23/34")
final.ola.2 <- which(cases.station$ANIO_SEPI_MIN == "23/42")

cases.station <- cases.station[c(inicio.ola.2:final.ola.2), ]
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

Prcp.1m.Tucu <- c(7.8, 7.8, 4.8, 4.7, 1.7, 11.0, 11.0, 101, 107)
Prcp.1m.Resi <- c(126, 118, 116, 71, 71, 43, 47, 74.2, 97.4)

tabla <- data.frame(Dates = as.POSIXct(seq.Date(as.Date("2024-08-24"), as.Date("2024-10-19"), 7)),
                    Semana = cases.station$ANIO_SEPI_MIN,
                    Casos.anterior = cases.station$Autóctono,
                    Prcp.lag2 = prcp.station.semanal$Suma,
                    Prcp.1m.lag2 = Prcp.1m.Tucu,
                    Tmin.lag2 = tmin.station.semanal$Media,
                    HR2.lag2 = HR2.station.semanal$Media,
                    Almc.lag2 = almc.station.semanal$Media,
                    ETR.lag2 = etr.station.semanal$Media,
                    ETP.lag2 = etp.station.semanal$Media,
                    Tmin.Count.lag2 = tmin.station.count.semanal$Media
                    )

mg.model <- readRDS("./models/87121_multiple_lineal_model")
rf.model <- readRDS("./models/87121_random_forest_model")
svm.model <- readRDS("./models/87121_svm_model")


data.complete <- as_tibble(c("24/36", "24/37", "24/38", "24/39", "24/40", "24/41", "24/42", "24/43", "24/44"))
data.complete$multiple_linear <- predict(mg.model, tabla)
data.complete$random_forest <- predict(rf.model, tabla)
data.complete$support_vm <- predict(svm.model, tabla)
colnames(data.complete)[1] <- c("Semana")

data.complete[data.complete < 0] <- 0

data.complete <- merge(data.complete, cases.last.wave, by = "Semana", all = TRUE)

#data.complete <- data.complete[53:65, ]

data.for.print <- melt(data.complete, id = "Semana")

fig <- ggplot() +
  #geom_point(data = data.for.print, aes(x = Semana, y = observation, group = 1, col = "Casos")) +
  
  geom_bar(data = data.for.print, aes(x = Semana, y = value, fill = variable), colour="black", position = "dodge", stat = "identity") +
  
  scale_fill_manual(values = c("multiple_linear" = "#D55E00", "random_forest" = "#0072B2", "support_vm" = "#009E73")) +
  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 16)) +
  theme(legend.text = element_text(size = 14)) + 
  
  labs(x = "Semana", y = "Casos", fill = "Modelos")

print(fig)
