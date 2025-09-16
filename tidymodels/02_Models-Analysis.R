rm(list = ls())
library('tidymodels')
library("corrplot")



training <- read.csv("./tidymodels/Cordoba_training_Casos-lag-2semanas.csv")
verification <- read.csv("./tidymodels/Cordoba_verification.csv")

# solo se queda con los lag2
training <- training[, -seq(4,19,1)]
training <- training[, -c(12,13,15,16)]


# Exploring data
training.longer <- training %>%
  select(Semana, Casos, 3:13) %>%
  pivot_longer(4:13, names_to = "variables")

training.longer %>%
  ggplot(aes(value, Casos, color = variables)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~ variables, scales = "free_x")

corrplot(cor(training[,3:13]), 
         method = "circle",     # Círculos
         type = "full",         # Mostrar matriz completa
         addCoef.col = "black", # Agregar valores en negro
         number.cex = 0.7,      # Tamaño del texto de coeficientes
         tl.col = "black",      # Color de etiquetas
         tl.cex = 0.8)          # Tamaño del texto de etiquetas
# ------------------------------------------------------------------------------

# Data spliting
set.seed(123)
training <- training %>%
  mutate(Casos.log = ifelse(Casos == 0, 0, log10(Casos)))

data.split <- initial_split(training, strata = Casos, prop = 0.8)
training.data <- training(data.split)
testing.data <- testing(data.split)
# ------------------------------------------------------------------------------

dengue.folds <- vfold_cv(training.data, v = 40, strata = Casos)

# ------------------------------------------------------------------------------
# Building model ----
data.recipe <- 
  recipe(formula = Casos.log ~
           Semana + Casos.lag +
           Tmin.Count.4d.lag2 + 
           Tmin.Count.7d.lag2 +
           Almc.lag2, data = training.data) %>%
  update_role(Semana, new_role = "ID")

rf.mod <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = tune()) %>% 
  set_mode("regression") %>% 
  set_engine("randomForest")

glmn.mod <- 
  linear_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet")

rf.workflow <- 
  workflow() %>% 
  add_recipe(data.recipe) %>% 
  add_model(rf.mod)

glmn.workflow <- 
  workflow() %>% 
  add_recipe(data.recipe) %>% 
  add_model(glmn.mod)
# ------------------------------------------------------------------------------

set.seed(123)
rf.tuning <-
  tune_grid(rf.workflow,
            resamples = dengue.folds,
            grid = 50, control = control_grid(save_workflow = TRUE))

glmn.tuning <-
  tune_grid(glmn.workflow,
            resamples = dengue.folds,
            grid = 50, control = control_grid(save_workflow = TRUE))

rf.fit <- fit_best(rf.tuning, verbose = TRUE)
glmn.fit <- fit_best(glmn.tuning, verbose = TRUE)

#training.pred <- predict(rf.fit, training.data)
#testing.pred <- predict(rf.fit, testing.data)

## Explore results
show_best(rf.tuning, metric = "rmse")
autoplot(rf.tuning)

rf.fit.final <- rf.workflow %>%
  finalize_workflow(select_best(rf.tuning))

glmn.fit.final <- glmn.workflow %>%
  finalize_workflow(select_best(glmn.tuning))

# ¿Qué resuelve last_fit con respecto al testing?
rf.fit <- last_fit(rf.fit.final, data.split)
glmn.fit <- last_fit(glmn.fit.final, data.split)

collect_predictions(dengue.fit) %>%
  ggplot(aes(Casos.log, .pred)) +
  geom_abline(lty = 2, color = "gray50") +
  geom_point(alpha = 0.5, color = "midnightblue") +
  coord_fixed()

 fig <- ggplot() +
   geom_line(data = out, aes(x = SE, y = Obs, group = 1, colour = "Casos"), size = 0.8, linetype = "dashed") +
   geom_point(data = out, aes(x = SE, y = Obs, group = 1, colour = "Casos")) +
   geom_line(data = out, aes(x = SE, y = .pred, group = 2, colour = "Random Forest"), size = 1) +
   geom_point(data = out, aes(x = SE, y = .pred, group = 2, colour = "Random Forest")) +
   geom_line(data = out, aes(x = SE, y = .pred.1, group = 3, colour = "GLMN"), size = 1) +
   geom_point(data = out, aes(x = SE, y = .pred.1, group = 3, colour = "GLMN")) +
   labs(y = "Casos Dengue") +
   theme_bw() +
   theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14)) +
   scale_colour_manual(values = c("Casos" = "#431901", 
                                  "Random Forest" = "#3b7861", 
                                  "GLMN" = "#5564eb"))


