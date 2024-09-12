# ------------------------------------------------------------------------------
# Funcion que abre los archivos de datos meteorologicos
BASE.meteo <- function(path.data = path.data, meteo.file = meteo.file, id.int = id.int, bhoa.file = bhoa.file)
{
  file1 <- vroom(paste0(path.data, meteo.file), col_types = "nDnnn")
  file1 <- file1[which(file1$Estacion == id.int), ]
  file2 <- vroom(paste0(path.data, bhoa.file))
  #file2$Fecha <- ymd(paste0(file2$ANUAL,
  #                          sprintf("%02d", file2$MES),
  #                          sprintf("%02d", file2$DIA)))
  #file2 <- file2[, -c(1, 2, 3, 7)]
  file <- merge(file1, file2, by = "Fecha")
  return(file)
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

