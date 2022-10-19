pacman::p_load(tidyverse, tidymodels, discrim, klaR, kknn)
set.seed(42)
  
## Cargamos datos ----
partidos <- read_rds("data/partidos_futbol.rds") %>% 
  left_join(data.frame(torneo = unique(partidos$torneo), 
                       categoria = factor(c("fem","fem", "fem", "fem", 
                                     "juv", 
                                     "reg", "reg", "reg", 
                                     "loc", "loc", "loc", "loc", "loc", "loc", "loc", "loc", 
                                     "fem", 
                                     "juv", "juv", "juv", "juv", 
                                     "reg", "reg", "reg", 
                                     "loc", "loc", "loc"))))



partidos %>% head()

partidos %>%
  summarize(across(.cols = everything(), ~sum(is.na(.))))

partidos %>%
  filter(is.na(formationUsed ))

## quitamos filas con NA
partidos_cleaned <- partidos %>%
  filter(!is.na(formationUsed))


# Dividimos data ----

partido_split <- initial_split(partidos_cleaned, strata = c("fasepartido"))

train <- training(partido_split)

test <- testing(partido_split)

# validacion cruzada
  
cv_folds <- vfold_cv(
  data = train, 
  v = 5) 

cv_folds


# Especificamos modelo ----

nb_model <- naive_Bayes() %>% 
  set_mode("classification") %>% 
  set_engine("klaR")

# receta
partidos_rec <- 
  recipe(categoria ~ . , data = train) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  update_role(torneo, equipo, id_partido, partido,new_role = "ID")


# Generamos workflow ----

partidos_wf <- workflow() %>% 
  add_recipe(partidos_rec) %>% 
  add_model(nb_model)

# Ajustamos el modelo ----

nb_fit <- 
  partidos_wf %>% 
  fit_resamples(resamples = cv_folds)

collect_metrics(nb_fit)

# predecimos con data de prueba

nb_final <- 
  partidos_wf %>% 
  last_fit(split = partido_split)

collect_metrics(nb_final)

nb_test_pred <- 
  bind_cols(test,
  nb_final %>% 
    collect_predictions() %>% 
    dplyr::select(starts_with(".pred_"))
  )

table("predicted class" = nb_test_pred$.pred_class,
      "observed class" = nb_test_pred$categoria)

nb_test_pred %>% 
  roc_curve(
    truth = categoria,
    .pred_fem, .pred_juv, .pred_loc, .pred_reg
  ) %>% 
  autoplot()

# ***************************
# Empaquetamos en una funcion ----
# **************************

evaluar_modelo <- function(model){
  partidos_wf <- workflow() %>% 
    add_recipe(partidos_rec) %>% 
    add_model(model)
  
  nb_fit <- partidos_wf %>% 
    fit_resamples(
      resamples = cv_folds
    )
  print(collect_metrics(nb_fit))
  
  nb_final <- partidos_wf %>% 
    last_fit(split = partido_split)
  
  print(collect_metrics(nb_final))
  
  nb_test_pred <- bind_cols(
    test,
    nb_final %>% collect_predictions() %>% dplyr::select(starts_with(".pred_"))
  )
  
  print(table("predicted class" = nb_test_pred$.pred_class,
        "observed class" = nb_test_pred$categoria))
  
  nb_test_pred %>% 
    roc_curve(
      truth = categoria,
      .pred_fem, .pred_juv, .pred_loc, .pred_reg
    ) %>% 
    autoplot()
}

# probamos la funcion con dos tipos de modelos
evaluar_modelo(nb_model)

knn_model <- nearest_neighbor() %>% 
  set_mode("classification")

evaluar_modelo(knn_model)


