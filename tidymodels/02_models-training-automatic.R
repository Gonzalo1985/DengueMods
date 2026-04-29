rm(list = ls())
library('tidymodels')
library("corrplot")

training <- read.csv("./output.csv")

# Exploring data
training.longer <- training %>%
  select(Semana.Obs.Epidemio, Autóctono, c(3,5:13)) %>%
  pivot_longer(c(3:12), names_to = "variables")

training.longer %>%
  ggplot(aes(value, Autóctono, color = variables)) +
  geom_point() +
  #scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~ variables, scales = "free_x")

colores_suaves <- colorRampPalette(c("#FF9999", "#FFFFFF", "#9999FF"))

corrplot(cor(training[,2:13], use = 'complete.obs', method = "kendall"), 
         method = "circle",        # Círculos
         type = "full",            # Mostrar matriz completa
         addCoef.col = "black",    # Agregar valores en negro
         number.cex = 0.7,         # Tamaño del texto de coeficientes
         tl.col = "black",         # Color de etiquetas
         tl.cex = 0.8,             # Tamaño del texto de etiquetas
         col = colores_suaves(200) # Aplicar paleta de colores suave
        )          
# ------------------------------------------------------------------------------

# Data spliting
set.seed(123)
training <- training %>%
  mutate(Autóctono.log = ifelse(Autóctono == 0, 0, log10(Autóctono)))

data.split <- initial_split(training, strata = Autóctono, prop = 0.8)
training.data <- training(data.split)
testing.data <- testing(data.split)
# ------------------------------------------------------------------------------

dengue.folds <- vfold_cv(training.data, v = 40, strata = Autóctono)

# ------------------------------------------------------------------------------
# Building model ----
data.recipe <- 
  recipe(formula = Autóctono.log ~
           Semana.Obs.Epidemio + Total +
           ETP + ETR + ALM +
           prcp + prcp.1m +
           tmin + hr +
           tmin.count.4d + 
           tmin.count.7d,
         data = training.data) %>%
  step_naomit(all_predictors(), all_outcomes()) %>%
  update_role(Semana.Obs.Epidemio, new_role = "ID")

rf.mod <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = tune()) %>% 
  set_mode("regression") %>% 
  set_engine("randomForest")

glmn.mod <- 
  linear_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet")

svm.mod <- 
  svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_mode("regression") %>%
  set_engine("kernlab")

rf.workflow <- 
  workflow() %>% 
  add_recipe(data.recipe) %>% 
  add_model(rf.mod)

glmn.workflow <- 
  workflow() %>% 
  add_recipe(data.recipe) %>% 
  add_model(glmn.mod)

svm.workflow <- 
  workflow() %>% 
  add_recipe(data.recipe) %>% 
  add_model(svm.mod)
# ------------------------------------------------------------------------------

set.seed(123)
rf.tuning <-
  tune_grid(rf.workflow,
            resamples = dengue.folds,
            grid = 50, control = control_grid(save_workflow = TRUE))
saveRDS(rf.tuning, "PATAGONIA_rf_tuning.rds")

glmn.tuning <-
  tune_grid(glmn.workflow,
            resamples = dengue.folds,
            grid = 50, control = control_grid(save_workflow = TRUE))
saveRDS(glmn.tuning, "PATAGONIA_glmn_tuning.rds")

svm.tuning <-
  tune_grid(svm.workflow,
            resamples = dengue.folds,
            grid = 50, control = control_grid(save_workflow = TRUE))
saveRDS(svm.tuning, "PATAGONIA_svm_tuning.rds")

rf.fit <- fit_best(rf.tuning, verbose = TRUE)
glmn.fit <- fit_best(glmn.tuning, verbose = TRUE)
svm.fit <- fit_best(svm.tuning, verbose = TRUE)

## Explore results
show_best(rf.tuning, metric = "rmse")
autoplot(rf.tuning)




