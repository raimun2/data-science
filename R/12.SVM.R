# cargamos librerias
pacman::p_load(tidyverse, tidymodels, discrim)
set.seed(42)

# leemos data de churn (rotacion)

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



# echamos un vistazo
partidos %>% glimpse()

# dividimos data entrenamiento prueba
data_split <- initial_split(partidos, prop = 3/4)
train_data <- training(data_split)
test_data  <- testing(data_split)

nrow(test_data)
train_data %>% nrow()

# generamos la receta
partidos_rec <- 
  recipe(categoria ~ goalsConceded + accuratePass, data = partidos)

partidos_rec

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
    add_recipe(partidos_rec) %>% 
    fit(data = train_data)
  
  model_pred <- 
    predict(modelo_fit, test_data, type = "prob") %>% 
    bind_cols(test_data) 
  
  roc <- (model_pred %>% 
           roc_auc(truth = categoria,
                     .pred_fem, .pred_juv, .pred_loc, .pred_reg))
  
  return(roc)
}

# ajustamos el modelo
fitea(modelo)

# generamos otra funcion para fitear en diferentes grados polinomiales
fitea_polySVM <- function(grado){
  
  mod <- svm_poly(degree = grado) %>% 
    set_engine("kernlab") %>% 
    set_mode("classification") %>% 
    translate()
  
  fitea(mod)
  
}

# testeamos polinomios
fitea_polySVM(1)
fitea_polySVM(2)
fitea_polySVM(3)
