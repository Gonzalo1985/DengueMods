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

path.ppal <- "./"
source("./fcts_DENGUE.R")


vect <- read.csv("./vectores/vectores_Tucuman_23-24.csv")

vect.mean <- vect %>%
  group_by(Semana) %>%
  summarise(IIP = mean(Indice.de.Inmuebles.Positivos),
            IB = mean(Indice.de.Breteau),
            IRP = mean(Indice.de.Recipientes.Positivos))

#vect.training <- data.frame(IIP = rep(0, 4), IB = rep(0, 4), IRP = rep(0, 4))
#vect.training <- rbind(vect.training, vect.mean[,2:4])

# ------------------------------------------------------------------------------
# PREPARACION DE BASE METEOROLOGICA
data <- BASE.meteo(path.data = path.ppal,
                   meteo.file = "meteo/Base_Tmin_Prcp_completo.csv",
                   id.int = 87121,
                   bhoa.file = "operativo/87121 - Tucuman - bhoa.csv")

colnames(data) <- c('Fecha', 'Estacion', 'Tmin', 'Prcp', 'HR2', 'ETP', 'ETR', 'ALM')

data.training.2019.2020 <- data[which(data$Fecha >= "2019-07-28" & data$Fecha <= "2020-07-25"),]
data.training.2022.2023 <- data[which(data$Fecha >= "2021-07-31" & data$Fecha <= "2022-07-29"),]
data.verification       <- data[which(data$Fecha >= "2023-07-30" & data$Fecha <= "2024-07-27"),]

data.training <- rbind(data.training.2019.2020, data.training.2022.2023)
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# PREPARACION DE BASE DE SALUD: DENGUE
cases <- read_excel(paste0(path.ppal, "casos/Casos Temporada 23_24.xlsx"), sheet = 1)
cases[which(is.na(cases$Autóctono)), "Autóctono"] <- 0

cases.old <- read_excel(paste0(path.ppal, "casos/Casos Temporada 19_20 a 22_23.xlsx"), sheet = 1)
cases.old[which(is.na(cases.old$Autóctono)), "Autóctono"] <- 0

#cases.station <- cases[which(cases$ID_LOC_INDEC_RESIDENCIA2 == "82084270"),]  # ROSARIO
#cases.station <- cases[which(cases$ID_LOC_INDEC_RESIDENCIA2 == "02002010"),]  # CABA
cases.station.old <- cases.old[which(cases.old$ID_LOC_INDEC_RESIDENCIA2 == "90084010"),]  # TUCUMAN
cases.station <- cases[which(cases$ID_LOC_INDEC_RESIDENCIA2 == "90084010"),]  # TUCUMAN
#cases.station.old <- cases.old[which(cases.old$ID_LOC_INDEC_RESIDENCIA2 == "22140060"),]  # RESISTENCIA
#cases.station <- cases[which(cases$ID_LOC_INDEC_RESIDENCIA2 == "22140060"),]  # RESISTENCIA
#cases.station.old <- cases.old[which(cases.old$ID_LOC_INDEC_RESIDENCIA2 == "10049030"),]  # CATAMARCA
#cases.station <- cases[which(cases$ID_LOC_INDEC_RESIDENCIA2 == "10049030"),]  # CATAMARCA

inicio.ola.1 <- which(cases.station.old$ANIO_SEPI_MIN == "19/31")
final.ola.1 <- which(cases.station.old$ANIO_SEPI_MIN == "20/30")

inicio.ola.2 <- which(cases.station.old$ANIO_SEPI_MIN == "22/31")
final.ola.2 <- which(cases.station.old$ANIO_SEPI_MIN == "23/30")

cases.station.old <- cases.station.old[c(inicio.ola.1:final.ola.1, inicio.ola.2:final.ola.2), ]
# ------------------------------------------------------------------------------


data.training$Semana <- sort(rep(1:104,7))

data.verification$Semana <- c(sort(rep(31:52,7)), sort(rep(1:30,7)))

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


tabla.training <- data.frame(Dates = as.POSIXct(seq.Date(as.Date("2023-01-01"), as.Date("2023-04-14"), 1)),
                             Semana = cases.station.old$ANIO_SEPI_MIN,
                             Casos = cases.station.old$Autóctono,
                             Prcp = prcp.station.semanal$Suma,
                             Tmin = tmin.station.semanal$Media,
                             HR2 = HR2.station.semanal$Media,
                             Almc = almc.station.semanal$Media,
                             ETR = etr.station.semanal$Media,
                             ETP = etp.station.semanal$Media,
                             Prcp.lag1 = c(NA, prcp.station.semanal$Suma[1:51], NA, prcp.station.semanal$Suma[53:103]),
                             Tmin.lag1 = c(NA, tmin.station.semanal$Media[1:51], NA, tmin.station.semanal$Media[53:103]),
                             HR2.lag1 = c(NA, HR2.station.semanal$Media[1:51], NA, HR2.station.semanal$Media[53:103]),
                             Almc.lag1 = c(NA, almc.station.semanal$Media[1:51], NA, almc.station.semanal$Media[53:103]),
                             ETR.lag1 = c(NA, etr.station.semanal$Media[1:51], NA, etr.station.semanal$Media[53:103]),
                             ETP.lag1 = c(NA, etp.station.semanal$Media[1:51], NA, etp.station.semanal$Media[53:103]),
                             Prcp.lag2 = c(NA, NA, prcp.station.semanal$Suma[1:50], NA, NA, prcp.station.semanal$Suma[53:102]),
                             Tmin.lag2 = c(NA, NA, tmin.station.semanal$Media[1:50], NA, NA, tmin.station.semanal$Media[53:102]),
                             HR2.lag2 = c(NA, NA, HR2.station.semanal$Media[1:50], NA, NA, HR2.station.semanal$Media[53:102]),
                             Almc.lag2 = c(NA, NA, almc.station.semanal$Media[1:50], NA, NA, almc.station.semanal$Media[53:102]),
                             ETR.lag2 = c(NA, NA, etr.station.semanal$Media[1:50], NA, NA, etr.station.semanal$Media[53:102]),
                             ETP.lag2 = c(NA, NA, etp.station.semanal$Media[1:50], NA, NA, etp.station.semanal$Media[53:102])
)

Casos.anterior <- tabla.training[1:52, "Casos"]
tabla.training <- tabla.training[53:104,]

aux.1m.lag0 <- tabla.training[1:52,] %>% mutate(Prcp.1m = rollapply(Prcp, width = 4, FUN = sum, align = "right", fill = NA))
aux.1m.lag1 <- tabla.training[1:52,] %>% mutate(Prcp.1m.lag1 = rollapply(Prcp.lag1, width = 4, FUN = sum, align = "right", fill = NA))
aux.1m.lag2 <- tabla.training[1:52,] %>% mutate(Prcp.1m.lag2 = rollapply(Prcp.lag2, width = 4, FUN = sum, align = "right", fill = NA))

aux.2s.lag0 <- tabla.training[1:52,] %>% mutate(Prcp.2s = rollapply(Prcp, width = 2, FUN = sum, align = "right", fill = NA))
aux.2s.lag1 <- tabla.training[1:52,] %>% mutate(Prcp.2s.lag1 = rollapply(Prcp.lag1, width = 2, FUN = sum, align = "right", fill = NA))
aux.2s.lag2 <- tabla.training[1:52,] %>% mutate(Prcp.2s.lag2 = rollapply(Prcp.lag2, width = 2, FUN = sum, align = "right", fill = NA))


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


tabla.training <- cbind(tabla.training, Casos.anterior)
tabla.training <- tabla.training[-c(1, 2, 3, 4, 5), ]


colnames(tabla.training)[22] <- c("Prcp.1m")
colnames(tabla.training)[23] <- c("Prcp.1m.lag1")
colnames(tabla.training)[24] <- c("Prcp.1m.lag2")

colnames(tabla.training)[25] <- c("Prcp.2s")
colnames(tabla.training)[26] <- c("Prcp.2s.lag1")
colnames(tabla.training)[27] <- c("Prcp.2s.lag2")

tabla.training.with.VECT <- merge(tabla.training, vect.mean, by = "Semana", all = TRUE)
tabla.training.with.VECT <- tabla.training.with.VECT[1:47,]
tabla.training.with.VECT[is.na(tabla.training.with.VECT)] <- 0

# ------------------------------------------------------------------------------

prcp.station.semanal <- data.verification %>% 
  group_by(Semana) %>%
  summarise(Suma = sum(Prcp, na.rm = TRUE))

tmin.station.semanal <- data.verification %>% 
  group_by(Semana) %>%
  summarise(Media = mean(Tmin))

HR2.station.semanal <- data.verification %>% 
  group_by(Semana) %>%
  summarise(Media = mean(HR2))

almc.station.semanal <- data.verification %>% 
  group_by(Semana) %>%
  summarise(Media = mean(ALM))

etr.station.semanal <- data.verification %>% 
  group_by(Semana) %>%
  summarise(Media = mean(ETR))

etp.station.semanal <- data.verification %>% 
  group_by(Semana) %>%
  summarise(Media = mean(ETP))


cases.station$prcp.semanal <- c(prcp.station.semanal$Suma[31:52],
                                prcp.station.semanal$Suma[1:30])

cases.station$tmin.semanal <- c(tmin.station.semanal$Media[31:52],
                                tmin.station.semanal$Media[1:30])

cases.station$hr2.semanal <- c(HR2.station.semanal$Media[31:52],
                               HR2.station.semanal$Media[1:30])

cases.station$almc.semanal <- c(almc.station.semanal$Media[31:52],
                                almc.station.semanal$Media[1:30])

cases.station$etr.semanal <- c(etr.station.semanal$Media[31:52],
                               etr.station.semanal$Media[1:30])

cases.station$etp.semanal <- c(etp.station.semanal$Media[31:52],
                               etp.station.semanal$Media[1:30])


tabla.verification <- data.frame(Dates = as.POSIXct(seq.Date(as.Date("2023-07-30"), as.Date("2024-07-27"), 7)),
                                  Semana = unique(cases$ANIO_SEPI_MIN),
                                  Casos = cases.station$Autóctono,
                                  Prcp = cases.station$prcp.semanal,
                                  Tmin = cases.station$tmin.semanal,
                                  HR2 = cases.station$hr2.semanal,
                                  Almc = cases.station$almc.semanal,
                                  ETR = cases.station$etr.semanal,
                                  ETP = cases.station$etp.semanal,
                                  Prcp.lag1 = c(NA, cases.station$prcp.semanal[1:51]),
                                  Tmin.lag1 = c(NA, cases.station$tmin.semanal[1:51]),
                                  HR2.lag1 = c(NA, cases.station$hr2.semanal[1:51]),
                                  Almc.lag1 = c(NA, cases.station$almc.semanal[1:51]),
                                  ETR.lag1 = c(NA, cases.station$etr.semanal[1:51]),
                                  ETP.lag1 = c(NA, cases.station$etp.semanal[1:51]),
                                  Prcp.lag2 = c(NA, NA, cases.station$prcp.semanal[1:50]),
                                  Tmin.lag2 = c(NA, NA, cases.station$tmin.semanal[1:50]),
                                  HR2.lag2 = c(NA, NA, cases.station$hr2.semanal[1:50]),
                                  Almc.lag2 = c(NA, NA, cases.station$almc.semanal[1:50]),
                                  ETR.lag2 = c(NA, NA, cases.station$etr.semanal[1:50]),
                                  ETP.lag2 = c(NA, NA, cases.station$etp.semanal[1:50])
                                 )

aux.1m.lag0 <- tabla.verification %>% mutate(Prcp.1m = rollapply(Prcp, width = 4, FUN = sum, align = "right", fill = NA))
aux.1m.lag1 <- tabla.verification %>% mutate(Prcp.1m.lag1 = rollapply(Prcp.lag1, width = 4, FUN = sum, align = "right", fill = NA))
aux.1m.lag2 <- tabla.verification %>% mutate(Prcp.1m.lag2 = rollapply(Prcp.lag2, width = 4, FUN = sum, align = "right", fill = NA))

aux.2s.lag0 <- tabla.verification %>% mutate(Prcp.2s = rollapply(Prcp, width = 2, FUN = sum, align = "right", fill = NA))
aux.2s.lag1 <- tabla.verification %>% mutate(Prcp.2s.lag1 = rollapply(Prcp.lag1, width = 2, FUN = sum, align = "right", fill = NA))
aux.2s.lag2 <- tabla.verification %>% mutate(Prcp.2s.lag2 = rollapply(Prcp.lag2, width = 2, FUN = sum, align = "right", fill = NA))

tabla.verification <- cbind(tabla.verification,
                            cbind(aux.1m.lag0$Prcp.1m,
                                  aux.1m.lag1$Prcp.1m.lag1,
                                  aux.1m.lag2$Prcp.1m.lag2)
)

tabla.verification <- cbind(tabla.verification,
                            cbind(aux.2s.lag0$Prcp.2s,
                                  aux.2s.lag1$Prcp.2s.lag1,
                                  aux.2s.lag2$Prcp.2s.lag2)
)

tabla.verification <- tabla.verification[-c(1, 2, 3, 4, 5), ]

colnames(tabla.verification)[22] <- c("Prcp.1m")
colnames(tabla.verification)[23] <- c("Prcp.1m.lag1")
colnames(tabla.verification)[24] <- c("Prcp.1m.lag2")

colnames(tabla.verification)[25] <- c("Prcp.2s")
colnames(tabla.verification)[26] <- c("Prcp.2s.lag1")
colnames(tabla.verification)[27] <- c("Prcp.2s.lag2")

tabla.verification <- cbind(tabla.verification, tabla.training$Casos)
#tabla.verification <- cbind(tabla.verification, tabla.training$Casos.anterior[1:38])
colnames(tabla.verification)[28] <- c("Casos.anterior")

tabla.verification.with.VECT <- merge(tabla.verification, vect.mean, by = "Semana", all = TRUE)
tabla.verification.with.VECT <- tabla.verification.with.VECT[19:65,]
tabla.verification.with.VECT[is.na(tabla.verification.with.VECT)] <- 0

# ------------------------------------------------------------------------------



# CORRIDA DE MODELOS
var.predictors <- c("Almc.lag2", "ETP.lag2", "Casos.anterior",
                    "Prcp.lag2", "Prcp.1m.lag2", "IIP", "IB", "IRP")

var.predictors.rf <- c("Almc.lag2", "ETP.lag2", "Casos.anterior",
                       "Prcp.lag2", "Prcp.1m.lag2", "IIP", "IB", "IRP")

var.predictors.svm <- c("Almc.lag2", "ETP.lag2", "Casos.anterior",
                        "Prcp.lag2", "Prcp.1m.lag2", "IIP", "IB", "IRP")


form.string.rf <- as.formula(paste("Casos ~ ", paste(var.predictors.rf, collapse= "+")))
form.string.svm <- as.formula(paste("Casos ~ ", paste(var.predictors.svm, collapse= "+")))

# ------------------------------------------------------------------------------
# MODELO DE REGRESION LINEAL MULTIPLE
mg <- multiple.guidance(input.data = tabla.training.with.VECT[,-1], predictand = "Casos",
                        predictors = var.predictors)

mg.evaluation <- mg.evaluation(input.data = tabla.verification.with.VECT[,-1],
                               predictand = "Casos",
                               predictors = var.predictors,
                               var.model = "Casos", lmodel = mg)

#saveRDS(object = mg, file = "./models/multiple_lineal_model")


# ------------------------------------------------------------------------------
# MODELO RANDOM FOREST
set.seed(123)
model.rf <- OPTI.methods(data = tabla.training.with.VECT, formula = form.string.rf, method1 = "cv", model = "rf")

set.seed(123)
rf.training <- predict(model.rf, tabla.training.with.VECT)

set.seed(123)
rf.verification <- predict(model.rf, tabla.verification.with.VECT)

#saveRDS(object = model.rf, file = "./models/random_forest_model")

# ------------------------------------------------------------------------------
# MODELO SVM
set.seed(123)
model.svm <- OPTI.methods(data = tabla.training.with.VECT, formula = form.string.svm, method1 = "cv", model = "svmLinear")

set.seed(123)
svm.training <- predict(model.svm, tabla.training.with.VECT)

set.seed(123)
svm.verification <- predict(model.svm, tabla.verification.with.VECT)

#saveRDS(object = model.svm, file = "./models/svm_model")


# ------------------------------------------------------------------------------
# PLOT
data.for.print <- as_tibble(mg.evaluation[[1]])
data.for.print$Dates <- as.POSIXct(data.for.print$Dates)
data.for.print$observation <- as.numeric(data.for.print$observation)
data.for.print$guidance <- as.numeric(data.for.print$guidance)

data.for.print[data.for.print < 0] <- 0
data.for.print$randomForest <- as.numeric(rf.training)
data.for.print$randomForest.ver <- as.numeric(rf.verification)

data.for.print$supportVM <- as.numeric(svm.training)
data.for.print$supportVM.ver <- as.numeric(svm.verification)

# valores menores a cero de los modelos forzados a cero
data.for.print[data.for.print < 0] <- 0

data.for.print$Semana <- tabla.verification$Semana

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




