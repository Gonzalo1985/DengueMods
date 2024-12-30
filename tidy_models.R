library("tidymodels")

rf.model <- rand_forest(trees = 10000, min_n = 5) %>%
  set_engine("ranger", verbose = TRUE) %>%
  set_mode("regression")

rf.wflow <- workflow() %>%
  add_model(rf.model) %>%
  add_variables(outcome = Casos, predictors = c(Almc.lag2, Tmin.Count.lag2, Prcp.2s.lag2, HR2.lag2))

rf.fit <- fit(rf.wflow, tabla.FINAL)