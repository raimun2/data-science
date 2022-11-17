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


# dividimos data entrenamiento prueba
data_split <- initial_split(partidos, prop = 3/4)
train_data <- training(data_split)
test_data  <- testing(data_split)


# probamos un arbol normal
library(rpart)
arbol1 <- rpart(categoria ~ goalsConceded + accuratePass + saves +totalPass, data=train_data, method = "class")
output1 <- test_data %>% cbind(predict(arbol1, test_data, type="prob"))

output %>% 
  roc_auc(truth = categoria, fem, juv, loc, reg)

# random forest
library(randomForest)
forest <- randomForest(categoria ~ goalsConceded + accuratePass + saves +totalPass,
                       data=train_data, importance=TRUE)
output2 <- test_data %>% cbind(predict(forest, newdata = test_data, type="prob"))

output2 %>% 
  roc_auc(truth = categoria, fem, juv, loc, reg)

# adaboost

library(adabag)

adaboost <- boosting(categoria ~ goalsConceded + accuratePass + saves + totalPass, 
                   data=train_data, boos=TRUE, mfinal=100, coeflearn='Breiman')

pred <- predict(adaboost, test_data, type="prob")

colnames(pred$prob) = colnames(pred$confusion)

output3 <- test_data %>% cbind(pred$prob)

output3 %>% 
  roc_auc(truth = categoria, fem, juv, loc, reg)
