# cargamos librerias
pacman::p_load(tidyverse, tidymodels, discrim)
set.seed(42)

# leemos data de churn (rotacion)
data <- read_csv("data/Churn_Modelling.csv") %>% 
  mutate(is_female = ifelse(Gender == "Female",1,0),
         Exited = as.factor(Exited)) %>% 
  select(-RowNumber, -Surname, -Geography, -Gender, -CustomerId) %>% 
  relocate(Exited)

# echamos un vistazo
data %>% glimpse()

# dividimos data entrenamiento prueba
data_split <- initial_split(data, prop = 3/4)
train_data <- training(data_split)
test_data  <- testing(data_split)

nrow(test_data)
train_data %>% nrow()

# generamos la receta
receta <- 
  recipe(Exited ~ ., data = train_data) 

receta

#definimos el modelo con 1 grado polinomial
modelo <- svm_poly(degree = 1) %>% 
  set_engine("kernlab") %>% 
  set_mode("classification") %>% 
  translate()


modelo

# definimos funcion fitea igual que otras oportunidades
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
           roc_auc(truth = Exited, .pred_0))
}

# ajustamos el modelo
fitea(modelo)

# generamos otra funcion para fitear en diferentes grados polinomiales
fitea_polySVM <- function(grado){
  
  mod <- svm_poly(degree = grado) %>% 
    set_engine("kernlab") %>% 
    set_mode("classification") %>% 
    translate()
  
  modelo_fit <- 
    workflow() %>% 
    add_model(mod) %>% 
    add_recipe(receta) %>% 
    fit(data = train_data)
  
  model_pred <- 
    predict(modelo_fit, test_data, type = "prob") %>% 
    bind_cols(test_data) 
  
  return(model_pred %>% 
           roc_auc(truth = Exited, .pred_0))
}

# testeamos polinomios
fitea_polySVM(1)
fitea_polySVM(2)
fitea_polySVM(3)
