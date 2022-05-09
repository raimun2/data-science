pacman::p_load(tidyverse, tidymodels, palmerpenguins, discrim, klaR, kknn)
set.seed(42)
  
## cargamos data ----
penguins %>%
  head()

penguins %>%
  summarize(across(.cols = everything(), ~sum(is.na(.))))

penguins %>%
  filter(is.na(sex))

## quitamos filas con NA
penguins_cleaned <- penguins %>%
  filter(!is.na(bill_depth_mm))


# Dividimos data ----

penguin_split <- initial_split(penguins_cleaned, strata = "species")

train <- training(penguin_split)

test <- testing(penguin_split)

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
penguins_rec <- recipe(
  species ~ . ,
  data = train
) %>%
  step_impute_knn( # para imputar datos faltantes con knn
    sex,
    neighbors = 3
  ) %>% 
  update_role(
    year, island,
    new_role = "ID"
  )


# generamos workflow ----

penguins_wf <- workflow() %>% 
  add_recipe(penguins_rec) %>% 
  add_model(nb_model)

# Ajustamos el modelo ----

nb_fit <- penguins_wf %>% 
  fit_resamples(
    resamples = cv_folds
  )
collect_metrics(nb_fit)

# predecimos con data de prueba

nb_final <- penguins_wf %>% 
  last_fit(split = penguin_split)

collect_metrics(nb_final)

nb_test_pred <- bind_cols(
  test,
  nb_final %>% collect_predictions() %>% dplyr::select(starts_with(".pred_"))
)

table("predicted class" = nb_test_pred$.pred_class,
      "observed class" = nb_test_pred$species)

nb_test_pred %>% 
  roc_curve(
    truth = species,
    .pred_Adelie, .pred_Chinstrap, .pred_Gentoo
  ) %>% 
  autoplot()

# ***************************
# Empaquetamos en una funcion ----
# **************************

evaluar_modelo <- function(model){
  penguins_wf <- workflow() %>% 
    add_recipe(penguins_rec) %>% 
    add_model(model)
  
  nb_fit <- penguins_wf %>% 
    fit_resamples(
      resamples = cv_folds
    )
  print(collect_metrics(nb_fit))
  
  nb_final <- penguins_wf %>% 
    last_fit(split = penguin_split)
  
  print(collect_metrics(nb_final))
  
  nb_test_pred <- bind_cols(
    test,
    nb_final %>% collect_predictions() %>% dplyr::select(starts_with(".pred_"))
  )
  
  print(table("predicted class" = nb_test_pred$.pred_class,
        "observed class" = nb_test_pred$species))
  
  nb_test_pred %>% 
    roc_curve(
      truth = species,
      .pred_Adelie, .pred_Chinstrap, .pred_Gentoo
    ) %>% 
    autoplot()
}

# probamos la funcion con dos tipos de modelos
evaluar_modelo(nb_model)

knn_model <- nearest_neighbor() %>% 
  set_mode("classification")

evaluar_modelo(knn_model)


