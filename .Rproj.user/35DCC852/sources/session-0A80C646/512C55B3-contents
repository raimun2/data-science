# cargo librerias
pacman::p_load(tidyverse, umap, tidymodels, kknn)
set.seed(42)

# define funciones de tidymodels por defecto
tidymodels_prefer()

# modelo prediccion
# cargamos la data

data_raw <- read_rds("data/partidos_futbol.rds")


# cargo la data y aplico los mismos tratamientos que en el caso de DBScan
data.num <- data_raw %>% 
  dplyr::select(where(is.numeric), -formationUsed) %>% 
  drop_na() 


# identificamos el numero de filas
n <- nrow(data.num)

# seleccionamos variables relevantes

# separo datos de entrenamiento y de prueba
data_train <- data.num[1:(n - 7), ]
data_test <- data.num[(n - 6):n, ]

# construyo el modelo
knn_reg_spec <-
  nearest_neighbor(neighbors = 5, weight_func = "triangular") %>%
  set_mode("regression") %>%
  set_engine("kknn")

# veo el modelo
knn_reg_spec

# ajusto parametros del modelo
knn_reg_fit <- knn_reg_spec %>% fit(goals ~ ., data = data.num)
# veo prediccion
knn_reg_fit

# predigo valores para conjunto de prueba
predict(knn_reg_fit, data_test)



## clasificacion

# cargo la data

(categorias <- data.frame(torneo = unique(data_raw$torneo), 
                          categoria = c("fem","fem", "fem", "fem", 
                                        "juv", 
                                        "reg", "reg", "reg", 
                                        "loc", "loc", "loc", "loc", "loc", "loc", "loc", "loc", 
                                        "fem", 
                                        "juv", "juv", "juv", "juv", 
                                        "reg", "reg", "reg", 
                                        "loc", "loc", "loc")))

data.cat <- data_raw %>% left_join(categorias)

data.umap <- data.num %>% 
  umap() %>% 
  .$layout %>% 
  as.data.frame() %>% 
  mutate(categoria = factor(data.cat$categoria))


# separo datos de entrenamiento y de prueba
data_train <- data.umap[-(1:10), ]
data_test  <- data.umap[  1:10 , ]

knn_cls_spec <-
  nearest_neighbor(neighbors = 11, weight_func = "triangular") %>%
  set_mode("classification") %>%
  set_engine("kknn")
knn_cls_spec

knn_cls_fit <- knn_cls_spec %>% fit(categoria ~ ., data = data.umap)
knn_cls_fit

bind_cols(
  predict(knn_cls_fit, data_test),
  predict(knn_cls_fit, data_test, type = "prob")
)





