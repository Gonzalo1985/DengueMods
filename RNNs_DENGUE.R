#library(reticulate)
#use_condaenv("r-reticulate", required = TRUE)

library("keras3")


model <- keras_model_sequential() %>%
  # Añadir una capa LSTM con las características desfasadas como entrada
  layer_dense(units = 50, input_shape = ncol(X_train), activation = 'relu') %>%
  # Añadir la capa de salida
  layer_dense(units = 1)

model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam()
)

X_train <- as.matrix(tabla.training[,var.predictors])
Y_train <- tabla.training$Casos

X_test <- as.matrix(tabla.verification[,var.predictors])
Y_test <- tabla.verification$Casos

history <- model %>% fit(
  X_train, Y_train,
  epochs = 300,
  batch_size = 16,
  validation_split = 0.2,
  verbose = 1
)

predictions <- model %>% predict(as.matrix(X_train))


# Convertir predicciones a un dataframe
predictions_df <- data.frame(
  time = seq(1,47,1),
  actual = Y_train,
  predicted = as.vector(predictions)
)


ggplot(predictions_df, aes(x = time)) +
  geom_line(aes(y = actual, color = "Real"), size = 1) +
  geom_line(aes(y = predicted, color = "Predicción"), size = 1, linetype = "dashed") +
  labs(title = "Datos Reales vs. Predicciones",
       x = "Tiempo",
       y = "Valor",
       color = "Tipo") +
  theme_minimal()