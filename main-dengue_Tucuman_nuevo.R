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

# ------------------------------------------------------------------------------
# PREPARACION DE BASE METEOROLOGICA
data <- BASE.meteo(path.data = path.ppal,
                   meteo.file = "meteo/Base_Tmin_Prcp_completo.txt",
                   id.int = 87121,
                   bhoa.file = "bhoa/bhoa_Tucuman.csv")

colnames(data) <- c('Fecha', 'Estacion', 'Tmin', 'Prcp', 'HR2', 'ETP', 'ETR', 'ALM')

# 4 dias consecutivos por debajo de un umbral
data <- data %>% mutate(Tmin.count.4d = as.integer(rollapply(Tmin, width = 4, FUN = function(x) all(x < 18), align = "right", fill = NA)))

# 7 dias consecutivos por debajo de un umbral
data <- data %>% mutate(Tmin.count.7d = as.integer(rollapply(Tmin, width = 7, FUN = function(x) all(x < 18), align = "right", fill = NA)))

data.training.2022.2023 <- data[which(data$Fecha >= "2022-07-31" & data$Fecha <= "2023-07-29"),]
data.training.2023.2024 <- data[which(data$Fecha >= "2023-07-30" & data$Fecha <= "2024-07-27"),]
#data.verification       <- data[which(data$Fecha >= "2023-07-30" & data$Fecha <= "2024-07-27"),]

data.training <- rbind(data.training.2022.2023, data.training.2023.2024)
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# PREPARACION DE BASE DE SALUD: DENGUE
cases.old <- read_excel(paste0(path.ppal, "casos/Casos Temporada 19_20 a 22_23.xlsx"), sheet = 1)
cases.old[which(is.na(cases.old$Autóctono)), "Autóctono"] <- 0
cases.old[which(is.na(cases.old$TOTAL)), "TOTAL"] <- 0

cases <- read_excel(paste0(path.ppal, "casos/Casos Temporada 23_24 hasta SE21.xlsx"), sheet = 1)
cases[which(is.na(cases$Autóctono)), "Autóctono"] <- 0
cases[which(is.na(cases$TOTAL)), "TOTAL"] <- 0

cases.station.old <- cases.old[which(cases.old$ID_LOC_INDEC_RESIDENCIA2 == "90084010"),]  # TUCUMAN
cases.station <- cases[which(cases$ID_LOC_INDEC_RESIDENCIA2 == "90084010"),]  # TUCUMAN

inicio.ola.1 <- which(cases.station.old$ANIO_SEPI_MIN == "22/31")
final.ola.1 <- which(cases.station.old$ANIO_SEPI_MIN == "23/30")

inicio.ola.2 <- which(cases.station$ANIO_SEPI_MIN == "23/31")
final.ola.2 <- which(cases.station$ANIO_SEPI_MIN == "24/21")

cases.training <- cases.station.old[c(inicio.ola.1:final.ola.1), ]
cases.training <- rbind(cases.training, cases.station[c(inicio.ola.2:final.ola.2), ])
# ------------------------------------------------------------------------------


data.training$Semana <- sort(rep(1:104,7))

#data.verification$Semana <- c(sort(rep(31:52,7)), sort(rep(1:30,7)))

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

tmin.station.count.4d <- data.training %>% 
  group_by(Semana) %>%
  summarise(Suma = sum(Tmin.count.4d))

tmin.station.count.7d <- data.training %>% 
  group_by(Semana) %>%
  summarise(Suma = sum(Tmin.count.7d))

tabla.training <- data.frame(Dates = as.POSIXct(seq.Date(as.Date("2023-01-01"), as.Date("2023-04-05"), 1)),
                             Semana = cases.training$ANIO_SEPI_MIN,
                             Casos = cases.training$Autóctono,
                             Casos.lag = c(NA, NA, cases.training$TOTAL[1:93]),
                             Prcp = prcp.station.semanal$Suma[1:95],
                             Tmin = tmin.station.semanal$Media[1:95],
                             Tmin.Count.4d = tmin.station.count.4d$Suma[1:95],
                             Tmin.Count.7d = tmin.station.count.7d$Suma[1:95],
                             HR2 = HR2.station.semanal$Media[1:95],
                             Almc = almc.station.semanal$Media[1:95],
                             ETR = etr.station.semanal$Media[1:95],
                             ETP = etp.station.semanal$Media[1:95],
                             Prcp.lag1 = c(NA, prcp.station.semanal$Suma[1:94]),
                             Tmin.lag1 = c(NA, tmin.station.semanal$Media[1:94]),
                             Tmin.Count.4d.lag1 = c(NA, tmin.station.count.4d$Suma[1:94]),
                             Tmin.Count.7d.lag1 = c(NA, tmin.station.count.7d$Suma[1:94]),
                             HR2.lag1 = c(NA, HR2.station.semanal$Media[1:94]),
                             Almc.lag1 = c(NA, almc.station.semanal$Media[1:94]),
                             ETR.lag1 = c(NA, etr.station.semanal$Media[1:94]),
                             ETP.lag1 = c(NA, etp.station.semanal$Media[1:94]),
                             Prcp.lag2 = c(NA, NA, prcp.station.semanal$Suma[1:93]),
                             Tmin.lag2 = c(NA, NA, tmin.station.semanal$Media[1:93]),
                             Tmin.Count.4d.lag2 = c(NA, NA, tmin.station.count.4d$Suma[1:93]),
                             Tmin.Count.7d.lag2 = c(NA, NA, tmin.station.count.7d$Suma[1:93]),
                             HR2.lag2 = c(NA, NA, HR2.station.semanal$Media[1:93]),
                             Almc.lag2 = c(NA, NA, almc.station.semanal$Media[1:93]),
                             ETR.lag2 = c(NA, NA, etr.station.semanal$Media[1:93]),
                             ETP.lag2 = c(NA, NA, etp.station.semanal$Media[1:93])
)

#Casos.anterior <- tabla.training[1:52, "Casos"]
#tabla.training <- tabla.training[53:104,]

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


#tabla.training <- cbind(tabla.training, Casos.anterior)
tabla.training <- tabla.training[-c(1, 2, 3, 4, 5), ]


colnames(tabla.training)[29] <- c("Prcp.1m")
colnames(tabla.training)[30] <- c("Prcp.1m.lag1")
colnames(tabla.training)[31] <- c("Prcp.1m.lag2")

colnames(tabla.training)[32] <- c("Prcp.2s")
colnames(tabla.training)[33] <- c("Prcp.2s.lag1")
colnames(tabla.training)[34] <- c("Prcp.2s.lag2")


# ------------------------------------------------------------------------------

tabla.FINAL <- tabla.training

# CORRIDA DE MODELOS
var.predictors <- c("Almc.lag2", "Tmin.Count.7d.lag2", "Tmin.Count.4d.lag2", "Prcp.1m.lag2", "Casos.lag")

var.predictors.rf <- c("Almc.lag2", "Tmin.Count.7d.lag2", "Tmin.Count.4d.lag2", "Prcp.2s.lag2", "Casos.lag")

var.predictors.svm <- c("Almc.lag2", "Tmin.Count.7d.lag2", "Tmin.Count.4d.lag2", "Prcp.1m.lag2", "Casos.lag")

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

saveRDS(object = mg, file = "./models/87121_multiple_lineal_model_7d+4d")


# ------------------------------------------------------------------------------
# MODELO RANDOM FOREST
set.seed(123)
model.rf <- OPTI.methods(data = tabla.FINAL, formula = form.string.rf, method1 = "cv", model = "rf") #tabla.training

set.seed(123)
rf.training <- predict(model.rf, tabla.training)

set.seed(123)
rf.verification <- predict(model.rf, tabla.FINAL)

saveRDS(object = model.rf, file = "./models/87121_random_forest_model_7d+4d")

# ------------------------------------------------------------------------------
# MODELO SVM
set.seed(123)
model.svm <- OPTI.methods(data = tabla.FINAL, formula = form.string.svm, method1 = "cv", model = "svmLinear")

set.seed(123)
svm.training <- predict(model.svm, tabla.training)

set.seed(123)
svm.verification <- predict(model.svm, tabla.FINAL)

saveRDS(object = model.svm, file = "./models/87121_svm_model_7d+4d")


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
print(rmse(data.for.print$guidance, data.for.print$observation))
print(rmse(data.for.print$randomForest.ver, data.for.print$observation))
print(rmse(data.for.print$supportVM.ver, data.for.print$observation))

