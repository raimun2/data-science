# cargamos librerias
pacman::p_load(tidyverse, tidymodels, discrim, naivebayes)
set.seed(4)

  ## Cargamos datos ----
partidos <- read_rds("data/partidos_futbol.rds") %>% 
  left_join(data.frame(torneo = unique(.$torneo), 
                       categoria = factor(c("fem","fem", "fem", "fem", 
                                            "juv", 
                                            "reg", "reg", "reg", 
                                            "loc", "loc", "loc", "loc", "loc", "loc", "loc", "loc", 
                                            "fem", 
                                            "juv", "juv", "juv", "juv", 
                                            "reg", "reg", "reg", 
                                            "loc", "loc", "loc"))))

# receta
partidos_rec <- 
  recipe(categoria ~ . , data = partidos) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  update_role(torneo, equipo, id_partido, partido, new_role = "ID")



# division datos
data_split <- initial_split(partidos, prop = 3/4, strata = fasepartido)
train_data <- training(data_split)
test_data  <- testing(data_split)

cv_folds <- 
  vfold_cv(data = train_data, v = 5) 


# definimos modelo de arbol con 5 niveles de profundidad y min 10 nodos por hoja
modelo0 <-
  decision_tree() %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

modelo0


# definimos funcion fitea para ajustar el model
fitea_cv <- function(mod){
  
  modelo_fit <- 
    workflow() %>% 
    add_model(mod) %>% 
    add_recipe(partidos_rec) %>% 
    fit_resamples(resamples = cv_folds)
  
  return(collect_metrics(modelo_fit))
}

# usamos funcion fitea con modelo de arboles
fitea_cv(modelo0)

# probamos con otros modelos


modelo1 <-
  decision_tree(min_n = 3) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

modelo1
fitea_cv(modelo1)

modelo2 <-
  decision_tree(tree_depth = 8) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

modelo2
fitea_cv(modelo2)

modelo3 <-
  decision_tree(tree_depth = 5, min_n = 10) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

modelo3


fitea_cv(modelo3)


# modelo naive bayes
modelo_nb <-
  naive_Bayes(smoothness = .8) %>%
  set_engine("naivebayes")

fitea_cv(modelo_nb)
