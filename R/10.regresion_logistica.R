pacman::p_load(tidymodels, tidyverse, nycflights13)
set.seed(42)
options(scipen = 999)

# cargar y limpiar datos ----
flight_data <- 
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
flights_rec <- 
  recipe(arr_delay ~ ., data = flight_data) %>% 
  update_role(flight, time_hour, new_role = "ID") %>% 
  step_date(date, features = c("dow", "month")) %>%               
  step_holiday(date, 
               holidays = timeDate::listHolidays("US"), 
               keep_original_cols = FALSE) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors())

# Especificamos modelo
model <- logistic_reg() %>% 
  set_mode("classification") %>% 
  set_engine("glm")

# Dividimos data ----
fligth_split <- initial_split(flight_data, strata = "arr_delay")
train <- training(fligth_split)
test <- testing(fligth_split)

# vemos resultados de una iteracion del modelo
parametros <- model %>% 
  fit(arr_delay~., data=test) %>% 
  tidy() 

view(parametros)

# validacion cruzada
cv_folds <- vfold_cv(data = train, v = 5) 

# creamos funcion para evaluar un modelo con CV
evaluar_modelo <- function(model, recipe, data_split){
  model_wf <- workflow() %>% 
    add_recipe(recipe) %>% 
    add_model(model)
  
  nb_fit <- model_wf %>% 
    fit_resamples(resamples = cv_folds)
  
  print(collect_metrics(nb_fit))
  
  nb_final <- model_wf %>% 
    last_fit(split = data_split)
  
  print(collect_metrics(nb_final))
  
  nb_test_pred <- bind_cols(
    test,
    nb_final %>% collect_predictions() %>% dplyr::select(starts_with(".pred_"))
  )
  
  print(table("predicted class" = nb_test_pred$.pred_class,
              "observed class" = nb_test_pred$arr_delay))

}


evaluar_modelo(model, flights_rec, fligth_split)


# probamos la funcion con otros tipos de modelos
nb_model <- naive_Bayes() %>% 
  set_mode("classification") %>% 
  set_engine("klaR")

evaluar_modelo(nb_model, flights_rec, fligth_split)

knn_model <- nearest_neighbor() %>% 
  set_mode("classification")

evaluar_modelo(knn_model, flights_rec, fligth_split)

