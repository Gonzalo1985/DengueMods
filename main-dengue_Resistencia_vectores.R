rm(list = ls())

library("readxl")
library("vroom")
library("dplyr")
library("aws.wrfsmn")
library("ggplot2")
library("zoo")
library("lubridate")
library("randomForest")
library("e1071")
library("caret")
library("hydroGOF")

path.ppal <- "./"
source("./fcts_datatable-models.R")
source("./fcts_query-API-CRC.R")

# ------------------------------------------------------------------------------
# PREPARACION DE BASE METEOROLOGICA
meteo.request <- ConsumirDatosEstacion(url.consulta = 'https://api.crc-sas.org/ws-api',
                                       usuario = 'gdiaz', clave = 'EoNGmeDYdr',
                                       fecha.inicial = '2019-01-01',
                                       fecha.final = '2025-01-11',
                                       id.estacion = 87155)

filename.bhoa  <- "87155 - Resistencia - bhoa.csv"
data <- BASE.meteo.2(path.data = path.ppal,
                     bhoa.file = paste0("operativo/", filename.bhoa),
                     meteo.data = meteo.request)

colnames(data) <- c('Fecha', 'Tmin', 'Prcp', 'HR2', 'ETP', 'ETR', 'ALM')

# 7 dias consecutivos por debajo de un umbral
data <- data %>% mutate(Tmin.count = as.integer(rollapply(Tmin, width = 7, FUN = function(x) all(x < 18), align = "right", fill = NA)))

data.training <- data[which(data$Fecha >= "2023-11-05" & data$Fecha <= "2024-11-16"),]
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# PREPARACION DE BASE DE SALUD: DENGUE
cases.old <- read_excel(paste0(path.ppal, "casos/Casos Temporada 23_24_conTOTAL.xlsx"), sheet = 1)
cases.old[which(is.na(cases.old$Autóctono)), "Autóctono"] <- 0
cases.old[which(is.na(cases.old$TOTAL)), "TOTAL"] <- 0

cases <- read_excel(paste0(path.ppal, "casos/Casos Temporada 24_25 a SE 46.xlsx"), sheet = 1)
cases[which(is.na(cases$Autóctono)), "Autóctono"] <- 0
cases[which(is.na(cases$TOTAL)), "TOTAL"] <- 0
cases <- cases[,-12]

cases.station.old <- cases.old[which(cases.old$ID_LOC_INDEC_RESIDENCIA2 == "22140060"),]  # RESISTENCIA
cases.station <- cases[which(cases$ID_LOC_INDEC_RESIDENCIA2 == "22140060"),]  # RESISTENCIA

inicio.ola.1 <- which(cases.station.old$ANIO_SEPI_MIN == "23/45")
final.ola.1 <- which(cases.station.old$ANIO_SEPI_MIN == "24/30")

inicio.ola.2 <- which(cases.station$ANIO_SEPI_MIN == "24/31")
final.ola.2 <- which(cases.station$ANIO_SEPI_MIN == "24/46")

cases.training <- cases.station.old[c(inicio.ola.1:final.ola.1), ]
cases.training <- rbind(cases.training, cases.station[c(inicio.ola.2:final.ola.2), ])
# ------------------------------------------------------------------------------

data.training$Semana <- sort(rep(1:54,7))

prcp.station.semanal <- data.training %>% 
  group_by(Semana) %>%
  summarise(Suma = sum(Prcp, na.rm = TRUE))

tmin.station.semanal <- data.training %>% 
  group_by(Semana) %>%
  summarise(Media = mean(Tmin))

HR2.station.semanal <- data.training %>% 
  group_by(Semana) %>%
  summarise(Media = mean(HR2))

almc.station.semanal <- data.training %>% 
  group_by(Semana) %>%
  summarise(Media = mean(ALM))

etr.station.semanal <- data.training %>% 
  group_by(Semana) %>%
  summarise(Media = mean(ETR))

etp.station.semanal <- data.training %>% 
  group_by(Semana) %>%
  summarise(Media = mean(ETP))

tmin.station.count.semanal <- data.training %>% 
  group_by(Semana) %>%
  summarise(Media = sum(Tmin.count))

tabla.training <- data.frame(Dates = as.POSIXct(seq.Date(as.Date("2023-01-01"), as.Date("2023-02-23"), 1)),
                             Semana = cases.training$ANIO_SEPI_MIN,
                             Casos = cases.training$Autóctono,
                             Casos.lag = c(NA, NA, cases.training$TOTAL[1:52]),
                             Prcp = prcp.station.semanal$Suma,
                             Tmin = tmin.station.semanal$Media,
                             Tmin.Count = tmin.station.count.semanal$Media,
                             HR2 = HR2.station.semanal$Media,
                             Almc = almc.station.semanal$Media,
                             ETR = etr.station.semanal$Media,
                             ETP = etp.station.semanal$Media,
                             Prcp.lag1 = c(NA, prcp.station.semanal$Suma[1:53]),
                             Tmin.lag1 = c(NA, tmin.station.semanal$Media[1:53]),
                             Tmin.Count.lag1 = c(NA, tmin.station.count.semanal$Media[1:53]),
                             HR2.lag1 = c(NA, HR2.station.semanal$Media[1:53]),
                             Almc.lag1 = c(NA, almc.station.semanal$Media[1:53]),
                             ETR.lag1 = c(NA, etr.station.semanal$Media[1:53]),
                             ETP.lag1 = c(NA, etp.station.semanal$Media[1:53]),
                             Prcp.lag2 = c(NA, NA, prcp.station.semanal$Suma[1:52]),
                             Tmin.lag2 = c(NA, NA, tmin.station.semanal$Media[1:52]),
                             Tmin.Count.lag2 = c(NA, NA, tmin.station.count.semanal$Media[1:52]),
                             HR2.lag2 = c(NA, NA, HR2.station.semanal$Media[1:52]),
                             Almc.lag2 = c(NA, NA, almc.station.semanal$Media[1:52]),
                             ETR.lag2 = c(NA, NA, etr.station.semanal$Media[1:52]),
                             ETP.lag2 = c(NA, NA, etp.station.semanal$Media[1:52])
)


#  VECTORES POR SEMANA
vect <- read.table("./vectores/Indice Inm Posit Resistencia.txt")
colnames(vect) <- c("Semana", "IIP")
vect <- vect[-c(55, 56, 57),]

tabla.training$IIP <- vect$IIP
tabla.training$IIP.lag2 <- c(NA, NA, vect$IIP[1:52])
# ------------------------------------------------------------------------------


aux.1m.lag0 <- tabla.training %>% mutate(Prcp.1m = rollapply(Prcp, width = 4, FUN = sum, align = "right", fill = NA))
aux.1m.lag1 <- tabla.training %>% mutate(Prcp.1m.lag1 = rollapply(Prcp.lag1, width = 4, FUN = sum, align = "right", fill = NA))
aux.1m.lag2 <- tabla.training %>% mutate(Prcp.1m.lag2 = rollapply(Prcp.lag2, width = 4, FUN = sum, align = "right", fill = NA))

aux.2s.lag0 <- tabla.training %>% mutate(Prcp.2s = rollapply(Prcp, width = 2, FUN = sum, align = "right", fill = NA))
aux.2s.lag1 <- tabla.training %>% mutate(Prcp.2s.lag1 = rollapply(Prcp.lag1, width = 2, FUN = sum, align = "right", fill = NA))
aux.2s.lag2 <- tabla.training %>% mutate(Prcp.2s.lag2 = rollapply(Prcp.lag2, width = 2, FUN = sum, align = "right", fill = NA))


tabla.training <- cbind(tabla.training,
                        cbind(c(aux.1m.lag0$Prcp.1m),
                              c(aux.1m.lag1$Prcp.1m.lag1),
                              c(aux.1m.lag2$Prcp.1m.lag2))
)

tabla.training <- cbind(tabla.training,
                        cbind(c(aux.2s.lag0$Prcp.2s),
                              c(aux.2s.lag1$Prcp.2s.lag1),
                              c(aux.2s.lag2$Prcp.2s.lag2))
)

# elimina primeras filas con NA por lagueado de variables
tabla.training <- tabla.training[-c(1, 2, 3, 4, 5), ]


colnames(tabla.training)[28] <- c("Prcp.1m")
colnames(tabla.training)[29] <- c("Prcp.1m.lag1")
colnames(tabla.training)[30] <- c("Prcp.1m.lag2")

colnames(tabla.training)[31] <- c("Prcp.2s")
colnames(tabla.training)[32] <- c("Prcp.2s.lag1")
colnames(tabla.training)[33] <- c("Prcp.2s.lag2")

# ------------------------------------------------------------------------------
tabla.FINAL <- tabla.training

# CORRIDA DE MODELOS
var.predictors <- c("Almc.lag2", "Casos.lag", "IIP.lag2")

var.predictors.rf <- c("Almc.lag2", "Casos.lag", "IIP.lag2")

var.predictors.svm <- c("Almc.lag2", "Casos.lag", "IIP.lag2")

form.string.rf <- as.formula(paste("Casos ~ ", paste(var.predictors.rf, collapse= "+")))
form.string.svm <- as.formula(paste("Casos ~ ", paste(var.predictors.svm, collapse= "+")))

# ------------------------------------------------------------------------------
# MODELO DE REGRESION LINEAL MULTIPLE
mg <- multiple.guidance(input.data = tabla.FINAL, predictand = "Casos",
                        predictors = var.predictors)

mg.evaluation <- mg.evaluation(input.data = tabla.FINAL,
                               predictand = "Casos",
                               predictors = var.predictors,
                               var.model = "Casos", lmodel = mg)

saveRDS(object = mg, file = "./models/87155_multiple_lineal_model")


# ------------------------------------------------------------------------------
# MODELO RANDOM FOREST
set.seed(123)
model.rf <- OPTI.methods(data = tabla.FINAL, formula = form.string.rf, method1 = "cv", model = "rf") #tabla.training

set.seed(123)
rf.training <- predict(model.rf, tabla.training)

set.seed(123)
rf.verification <- predict(model.rf, tabla.FINAL)

saveRDS(object = model.rf, file = "./models/87155_random_forest_model")

# ------------------------------------------------------------------------------
# MODELO SVM
set.seed(123)
model.svm <- OPTI.methods(data = tabla.FINAL, formula = form.string.svm, method1 = "cv", model = "svmLinear")

set.seed(123)
svm.training <- predict(model.svm, tabla.training)

set.seed(123)
svm.verification <- predict(model.svm, tabla.FINAL)

saveRDS(object = model.svm, file = "./models/87155_svm_model")


# ------------------------------------------------------------------------------
# PLOT
data.for.print <- as_tibble(mg.evaluation[[1]])
data.for.print$Dates <- as.POSIXct(data.for.print$Dates)
data.for.print$observation <- as.numeric(data.for.print$observation)
data.for.print$guidance <- as.numeric(data.for.print$guidance)

data.for.print[data.for.print < 0] <- 0
#data.for.print$randomForest <- as.numeric(rf.training)
data.for.print$randomForest.ver <- as.numeric(rf.verification)

#data.for.print$supportVM <- as.numeric(svm.training)
data.for.print$supportVM.ver <- as.numeric(svm.verification)

# valores menores a cero de los modelos forzados a cero
data.for.print[data.for.print < 0] <- 0

data.for.print$Semana <- tabla.FINAL$Semana

fig <- ggplot() +
  geom_point(data = data.for.print, aes(x = Semana, y = observation, group = 1, col = "Casos")) +
  
  geom_line(data = data.for.print, aes(x = Semana, y = guidance, group = 1, col = "Modelo Multiple Lineal")) +
  
  geom_line(data = data.for.print, aes(x = Semana, y = randomForest.ver, group = 1, col = "Random Forest")) +
  
  #geom_ribbon(aes(x = index(data.for.print), 
#                  ymin = data.for.print$randomForest.ver - min(model.rf$results$RMSE), 
#                  ymax = data.for.print$randomForest.ver + min(model.rf$results$RMSE), colour = "banda RMSE rf"), alpha = 0.2) +
  
  geom_line(data = data.for.print, aes(x = Semana, y = supportVM.ver, group = 1, col = "Support VM")) +
#  geom_ribbon(aes(x = index(data.for.print), 
#                  ymin = data.for.print$supportVM.ver - min(model.svm$results$RMSE), 
#                  ymax = data.for.print$supportVM.ver + min(model.svm$results$RMSE), colour = "banda RMSE svm"), alpha = 0.2) +
  
#  scale_y_continuous(limits = c(0, 12500), breaks = seq(0, 12500, by = 2500)) +
  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

print(fig)
#print(KGE(data.for.print$supportVM.ver, data.for.print$observation, out.type = "full", method = "2021"))
#print(NSE(data.for.print$supportVM.ver, data.for.print$observation))
#print(rmse(data.for.print$supportVM.ver, data.for.print$observation))

