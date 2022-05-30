# cargamos librerias
pacman::p_load(tidyverse, tidymodels, discrim, naivebayes, nycflights13)
set.seed(42)

# data de vuelos a NYC
data <- 
  flights %>% 
  mutate(
    arr_delay = ifelse(arr_delay >= 30, "late", "on_time"),
    arr_delay = factor(arr_delay),
    date = lubridate::as_date(time_hour)
  ) %>% 
  inner_join(weather, by = c("origin", "time_hour")) %>% 
  dplyr::select(dep_time, flight, origin, dest, air_time, distance, 
                carrier, date, arr_delay, time_hour) %>% 
  na.omit() %>% 
  mutate_if(is.character, as.factor)  %>% 
  sample_n(10000)

# receta
receta <- 
  recipe(arr_delay ~ ., data = data) %>% 
  update_role(flight, time_hour, new_role = "ID") %>% 
  step_date(date, features = c("dow", "month")) %>%               
  step_holiday(date, 
               holidays = timeDate::listHolidays("US"), 
               keep_original_cols = FALSE) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors())


# division datos
data_split <- initial_split(data, prop = 3/4)
train_data <- training(data_split)
test_data  <- testing(data_split)

# definimos modelo de arbol con 5 niveles de profundidad y min 10 nodos por hoja
modelo <-
  decision_tree(tree_depth = 5, min_n = 10) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

modelo

# definimos funcion fitea para ajustar el model
fitea <- function(mod){
  
  modelo_fit <- 
    workflow() %>% 
    add_model(mod) %>% 
    add_recipe(receta) %>% 
    fit(data = train_data)
  
  model_pred <- 
    predict(modelo_fit, test_data, type = "prob") %>% 
    bind_cols(test_data) 
  
  return(model_pred %>% 
           roc_auc(truth = arr_delay, .pred_late))
}

# usamos funcion fitea con modelo de arboles
fitea(modelo)

# probamos con otros modelos
# modelo regresion logistica
modelo_rl <- 
  logistic_reg() %>% 
  set_engine("glm")

fitea(modelo_rl)

# modelo naive bayes
modelo_nb <-
  naive_Bayes(smoothness = .8) %>%
  set_engine("naivebayes")

fitea(modelo_nb)

# modelo knn
modelo_knn <-
  nearest_neighbor(neighbors = 5) %>% 
  set_engine("kknn") %>% 
  set_mode("classification")

fitea(modelo_knn)

# probamos con diferentes parametros del arbol
# profundidad entre 3 y 10
# minimo de nodos entre 5 y 20
# generamos todas las combinaciones
params <- expand.grid(tree_depth = 1:5, 
                      min_n = 5:10)


# usamos map para iterar sobre las 88 opciones y generar resultados
# almacenamos resultados en vector res
res <- map2_dfr(params$tree_depth, 
                params$min_n, 
                function(x,y) 
                  fitea(decision_tree(tree_depth = x, min_n = y) %>% 
                          set_engine("rpart") %>% 
                          set_mode("classification")))

# acoplamos res con los parametros para obtener el dataframe definitivo
res <- cbind(res, params)
