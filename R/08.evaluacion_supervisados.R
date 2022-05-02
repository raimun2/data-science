pacman::p_load(tidymodels, tidyverse, nycflights13)
set.seed(42)

# cargar y limpiar datos ----
flight_data <- 
  flights %>% 
  mutate(
    # discretiza arr_delay y lo hace factor
    arr_delay = ifelse(arr_delay >= 30, "late", "on_time"),
    arr_delay = factor(arr_delay),
    # obtengo solo la fecha a partir de fecha con hora
    date = lubridate::as_date(time_hour)
  ) %>% 
  # combino con data de clima
  inner_join(weather, by = c("origin", "time_hour")) %>% 
  # especifico las columnas de interes
  select(dep_time, flight, origin, dest, air_time, distance, 
         carrier, date, arr_delay, time_hour) %>% 
  # excluyo datos faltates
  na.omit() %>% 
  # transformo characteres en factores
  mutate_if(is.character, as.factor)  %>% 
  # tomo una muestra de tamaño 10.000 para poder ejecutar los modelos
  sample_n(10000)

## inspeccion de los datos 
flight_data %>% 
  count(arr_delay) %>% 
  mutate(prop = n/sum(n))

glimpse(flight_data)

-----------------------------------
  
# especificar receta ----
arr_delay ~ .

# receta simple
flights_rec <- 
  recipe(arr_delay ~ ., data = flight_data) 

# receta ignorando algunas variables
flights_rec <- 
  recipe(arr_delay ~ ., data = flight_data) %>% 
  update_role(flight, time_hour, new_role = "ID") 

# visualizar
summary(flights_rec)

flight_data %>% 
  distinct(date) %>% 
  mutate(numeric_date = as.numeric(date)) 

# modificando fechas
flights_rec <- 
  recipe(arr_delay ~ ., data = flight_data) %>% 
  update_role(flight, time_hour, new_role = "ID") %>% 
  step_date(date, features = c("dow", "month")) %>%               
  step_holiday(date, 
               holidays = timeDate::listHolidays("US"), 
               keep_original_cols = FALSE)

# convertir predictores nominales a dummys
flights_rec <- 
  recipe(arr_delay ~ ., data = flight_data) %>% 
  update_role(flight, time_hour, new_role = "ID") %>% 
  step_date(date, features = c("dow", "month")) %>%               
  step_holiday(date, 
               holidays = timeDate::listHolidays("US"), 
               keep_original_cols = FALSE) %>% 
  step_dummy(all_nominal_predictors())

# quitar variables sin varianza
flights_rec <- 
  recipe(arr_delay ~ ., data = flight_data) %>% 
  update_role(flight, time_hour, new_role = "ID") %>% 
  step_date(date, features = c("dow", "month")) %>%               
  step_holiday(date, 
               holidays = timeDate::listHolidays("US"), 
               keep_original_cols = FALSE) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors())

# especificar modelo ----

nearest_neighbor()

nearest_neighbor() %>% 
  set_mode("classification")

knn_mod <- nearest_neighbor() %>% 
  set_mode("classification")

# genera workflow ----

flights_wflow <- 
  workflow() %>% 
  add_model(knn_mod) %>% 
  add_recipe(flights_rec)

flights_wflow

# split data ----

data_split <- initial_split(flight_data, prop = 3/4)

train_data <- training(data_split)
test_data  <- testing(data_split)


train_data %>% 
  count(arr_delay) %>% 
  mutate(prop = n/sum(n))

data_split_strat <- initial_split(flight_data, prop = 3/4, strata = arr_delay)
train_data <- training(data_split_strat)
test_data  <- testing(data_split_strat)

train_data %>% 
  count(arr_delay) %>% 
  mutate(prop = n/sum(n))

# ajuste de modelos  ----

# ajuste con data entrenamiento
flights_fit <- 
  flights_wflow %>% 
  fit(data = train_data)

# prediccion con data test
predict(flights_fit, test_data)

# amplificacion data test con predicciones 
flights_aug <- 
  augment(flights_fit, test_data)

flights_aug %>%
  select(arr_delay, time_hour, flight, .pred_class, .pred_on_time)

flights_aug %>% 
  count(arr_delay) %>% 
  mutate(prop = n/sum(n))

# calcula metricas ----

metrics(flights_aug, arr_delay, .pred_class)

flights_aug %>% 
  roc_curve(truth = arr_delay, .pred_late) %>% 
  autoplot()

precision(flights_aug, arr_delay, .pred_class)

precision(flights_aug, arr_delay, .pred_class, estimator = "micro")


flights_aug %>%               
  accuracy(truth = arr_delay, .pred_class)

flights_aug %>% 
  roc_auc(truth = arr_delay, .pred_late)

flights_aug %>%                
  roc_auc(truth = arr_delay, .pred_on_time)

# validacion cruzada ----

folds <- vfold_cv(train_data, v = 2)
folds


flights_resample <- 
  flights_wflow %>% 
  fit_resamples(folds)

flights_resample

collect_metrics(flights_resample)


