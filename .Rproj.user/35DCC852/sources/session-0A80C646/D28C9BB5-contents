pacman::p_load(tidyverse, umap, tidymodels, kknn)
set.seed(42)

# cargar y limpiar datos ----
data_raw <- read_rds("data/partidos_futbol.rds") %>% 
  mutate(local=factor(local)) %>% 
  drop_na()


# cargo la data y aplico los mismos tratamientos que en el caso de DBScan
data_raw %>% 
  count(local) %>% 
  mutate(prop = n/sum(n)) 

## inspeccion de los datos 
glimpse(data_raw)

-----------------------------------
  
# especificar receta ----
local ~ .

# receta simple
receta <- 
  recipe(local ~ ., data = data_raw) 

# receta ignorando algunas variables
receta <- 
  recipe(local ~ ., data = data_raw) %>% 
  update_role(id_partido, torneo, equipo, partido,fasepartido,  new_role = "ID") 

# visualizar
summary(receta)

# convertir predictores nominales a dummys
receta <- 
  recipe(local ~ ., data = data_raw) %>% 
  update_role(id_partido, equipo, partido,  new_role = "ID") %>% 
  step_dummy(all_nominal_predictors())

# quitar variables sin varianza
receta <- 
  recipe(local ~ ., data = data_raw) %>% 
  update_role(id_partido, equipo, partido,  new_role = "ID") %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors())

# especificar modelo ----

nearest_neighbor()

nearest_neighbor() %>% 
  set_mode("classification")

knn_mod <- nearest_neighbor() %>% 
  set_mode("classification")

# genera workflow ----

flujo <- 
  workflow() %>% 
  add_model(knn_mod) %>% 
  add_recipe(receta)

flujo

# split data ----

data_split <- initial_split(data_raw, prop = 3/4)

train_data <- training(data_split)
test_data  <- testing(data_split)

train_data %>% 
  count(local) %>% 
  mutate(prop = n/sum(n))

data_split_strat <- initial_split(data_raw, prop = 3/4, strata = local)
train_data <- training(data_split_strat)
test_data  <- testing(data_split_strat)

train_data %>% 
  count(local) %>% 
  mutate(prop = n/sum(n))

# ajuste de modelos  ----

# ajuste con data entrenamiento
ajuste <- 
  flujo %>% 
  fit(data = train_data)

# prediccion con data test
predict(ajuste, test_data)

# amplificacion data test con predicciones 
amplificacion <- augment(ajuste, test_data)

amplificacion %>%
  select(local, equipo, torneo, .pred_class, .pred_TRUE, .pred_FALSE)

amplificacion %>% 
  count(.pred_class) %>% 
  mutate(prop = n/sum(n))

# calcula metricas ----

metrics(amplificacion, local, .pred_class)

amplificacion %>% 
  roc_curve(truth = local, .pred_FALSE) %>% 
  autoplot()


amplificacion %>%               
  accuracy(truth = local, .pred_class)

amplificacion %>% 
  roc_auc(truth = local, .pred_TRUE)

amplificacion %>%                
  roc_auc(truth = local, .pred_FALSE)

# validacion cruzada ----

folds <- vfold_cv(train_data, v = 20)
folds

resample <- 
  flujo %>% 
  fit_resamples(folds)

resample

collect_metrics(resample)


