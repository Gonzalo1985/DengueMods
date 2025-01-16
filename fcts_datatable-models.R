# ------------------------------------------------------------------------------
# Funcion que abre los archivos de datos meteorologicos
BASE.meteo <- function(path.data = path.data, meteo.file = meteo.file, id.int = id.int, bhoa.file = bhoa.file)
{
  file1 <- vroom(paste0(path.data, meteo.file), col_types = "nDnnn")
  file1 <- file1[which(file1$omm_id == id.int), ]
  file2 <- read.csv(paste0(path.data, bhoa.file), row.names = NULL)
  file2 <- file2[,-5]
  colnames(file2) <- c("Fecha", "ETP.mm", "ETR.mm", "ALM.mm")
  file2$Fecha <- as.Date(file2$Fecha)
  file <- merge(file1, file2, by = "Fecha")
  return(file)
}


BASE.meteo.2 <- function(path.data = path.data, bhoa.file = bhoa.file, meteo.data = meteo.data)
{
  file1 <- read.csv(paste0(path.data, bhoa.file), row.names = NULL)
  file1 <- file1[,-5]
  colnames(file1) <- c("Fecha", "ETP.mm", "ETR.mm", "ALM.mm")
  file1$Fecha <- as.Date(file1$Fecha)
  meteo.data$Fecha <- as.Date(meteo.data$Fecha)
  final.data <- merge(meteo.data, file1, by = "Fecha")
  return(final.data)
}


# ------------------------------------------------------------------------------
# Funcion que optimiza los modelos de Machine Learning
OPTI.methods <- function(data = data, formula = formula, method1 = method1, model = model)
  
{
  
  tune_grid <- expand.grid(
    mtry = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20)
  )
  
  myTimeControl <- trainControl(method = method1,
                                number = 100,
                                #initialWindow = 30,
                                verboseIter = TRUE,
                                savePredictions = "all")
  
  model <- train(formula,
                 data = data,
                 method = model,
                 trControl = myTimeControl,
                 tunegrid = tune_grid,
                 metric = 'RMSE',
                 importance = TRUE)
  
  return(model)
}

