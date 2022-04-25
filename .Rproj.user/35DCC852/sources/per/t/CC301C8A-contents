# Regression Example (kknn)


library(tidymodels)
tidymodels_prefer()
data(Chicago)

n <- nrow(Chicago)
Chicago <- Chicago %>% select(ridership, Clark_Lake, Quincy_Wells)

Chicago_train <- Chicago[1:(n - 7), ]
Chicago_test <- Chicago[(n - 6):n, ]

knn_reg_spec <-
  nearest_neighbor(neighbors = 5, weight_func = "triangular") %>%
  # This model can be used for classification or regression, so set mode
  set_mode("regression") %>%
  set_engine("kknn")
knn_reg_spec


knn_reg_fit <- knn_reg_spec %>% fit(ridership ~ ., data = Chicago_train)
knn_reg_fit

predict(knn_reg_fit, Chicago_test)

# Classification Example (kknn)


library(tidymodels)
tidymodels_prefer()
data(two_class_dat)

data_train <- two_class_dat[-(1:10), ]
data_test  <- two_class_dat[  1:10 , ]

knn_cls_spec <-
  nearest_neighbor(neighbors = 11, weight_func = "triangular") %>%
  # This model can be used for classification or regression, so set mode
  set_mode("classification") %>%
  set_engine("kknn")
knn_cls_spec

knn_cls_fit <- knn_cls_spec %>% fit(Class ~ ., data = data_train)
knn_cls_fit

bind_cols(
  predict(knn_cls_fit, data_test),
  predict(knn_cls_fit, data_test, type = "prob")
)





