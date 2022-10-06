# cargo librerias
pacman::p_load(tidymodels, kknn)

# define funciones de tidymodels por defecto
tidymodels_prefer()

# modelo prediccion
# cargamos la data
data(Chicago)

# identificamos el numero de filas
n <- nrow(Chicago)

# seleccionamos variables relevantes
Chicago <- Chicago %>% select(ridership, Clark_Lake, Quincy_Wells)

# separo datos de entrenamiento y de prueba
Chicago_train <- Chicago[1:(n - 7), ]
Chicago_test <- Chicago[(n - 6):n, ]

# construyo el modelo
knn_reg_spec <-
  nearest_neighbor(neighbors = 5, weight_func = "triangular") %>%
  set_mode("regression") %>%
  set_engine("kknn")

# veo el modelo
knn_reg_spec

# ajusto parametros del modelo
knn_reg_fit <- knn_reg_spec %>% fit(ridership ~ ., data = Chicago_train)
# veo prediccion
knn_reg_fit

# predigo valores para conjunto de prueba
predict(knn_reg_fit, Chicago_test)

## clasificacion

# cargo la data
data(two_class_dat)

# separo datos de entrenamiento y de prueba
data_train <- two_class_dat[-(1:10), ]
data_test  <- two_class_dat[  1:10 , ]

knn_cls_spec <-
  nearest_neighbor(neighbors = 11, weight_func = "triangular") %>%
  set_mode("classification") %>%
  set_engine("kknn")
knn_cls_spec

knn_cls_fit <- knn_cls_spec %>% fit(Class ~ ., data = data_train)
knn_cls_fit

bind_cols(
  predict(knn_cls_fit, data_test),
  predict(knn_cls_fit, data_test, type = "prob")
)





