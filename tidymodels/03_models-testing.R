rm(list = ls())
library('tidymodels')
library("corrplot")


training <- data.training.region

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

# ------------------------------------------------------------------------------
rf.tuning <- readRDS("./tidiymodels/tuning/CENTRO_rf_tuning.rds")
svm.tuning <- readRDS("./tidiymodels/tuning/CENTRO_svm_tuning.rds")
glmn.tuning <- readRDS("./tidiymodels/tuning/CENTRO_glmn_tuning.rds")

rf.fit <- fit_best(rf.tuning, verbose = TRUE)
glmn.fit <- fit_best(glmn.tuning, verbose = TRUE)
svm.fit <- fit_best(svm.tuning, verbose = TRUE)

testing.pred <- predict(rf.fit, testing.data)

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
   geom_line(data = out, aes(x = SE, y = Obs.Autoc, group = 1, colour = "Casos"), size = 0.8, linetype = "dashed") +
   geom_point(data = out, aes(x = SE, y = Obs.Autoc, group = 1, colour = "Casos")) +
   geom_line(data = out, aes(x = SE, y = .pred, group = 2, colour = "Random Forest"), size = 1) +
   geom_point(data = out, aes(x = SE, y = .pred, group = 2, colour = "Random Forest")) +
   geom_line(data = out, aes(x = SE, y = .pred.1, group = 3, colour = "GLMN"), size = 1) +
   geom_point(data = out, aes(x = SE, y = .pred.1, group = 3, colour = "GLMN")) +
   geom_line(data = out, aes(x = SE, y = .pred.2, group = 3, colour = "SVM"), size = 1) +
   geom_point(data = out, aes(x = SE, y = .pred.2, group = 3, colour = "SVM")) +
   labs(y = "Casos Dengue") +
   theme_bw() +
   theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14)) +
   coord_cartesian(ylim = c(0, 250)) +
   scale_colour_manual(values = c("Casos" = "#431901", 
                                  "Random Forest" = "#3b7861", 
                                  "GLMN" = "#5564eb",
                                  "SVM" = "#B87E14"))


