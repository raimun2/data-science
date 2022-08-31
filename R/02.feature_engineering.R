pacman::p_load(tidyverse, proxy)
set.seed(42)

data_raw <- read_rds("data/partidos_futbol.rds")

data_raw %>% glimpse()

data_raw <- data_raw %>% mutate(formationUsed = factor(formationUsed))

data_num <- data_raw %>% 
  mutate(equipo = as.numeric(factor(equipo))) %>% 
  select(where(is.numeric)) 

# creamos un subconjunto para que sea mas liviano
data <-
  data_num %>% 
  select(equipo, accuratePass, wonTackle, goalsConceded,
         ontargetScoringAtt, totalScoringAtt, saves) %>% 
  sample_n(1000)


# Comparación entre atributtos ----

## los metodos de similaridad y distancias disponibles en el paquete proxy
summary(pr_DB)


## Correlaciones

cor(data$ontargetScoringAtt, data$accuratePass, method = "pearson")

cor(data$ontargetScoringAtt, data$accuratePass, method = "spearman")

cor(data$ontargetScoringAtt, data$accuratePass, method = "kendall")

## Similitudes

simil(list(data$ontargetScoringAtt,data$accuratePass), method = "cosine")

simil(list(data$ontargetScoringAtt,data$accuratePass), method = "Jaccard")

simil(list(data$ontargetScoringAtt,data$accuratePass), method = "simple matching")

simil(list(data$ontargetScoringAtt,data$accuratePass), method = "Kulczynski1")

## Distancias

dist(list(data$ontargetScoringAtt,data$accuratePass), method = "Euclidean") 

dist(list(data$ontargetScoringAtt,data$accuratePass), method = "Manhattan") 

dist(list(data$ontargetScoringAtt,data$accuratePass), method = "supremum") 


# Feature Selection -----

## Fuerza bruta

library(stuart) 

results <- bruteforce(data, list(ra = names(data)), 3,
                      cores = 1)  # numero de nucleos en la maquina


summary(results) 

## Metodos de Optimizacion 

library(FSinR)   

# definimos primero el algoritmo de busqueda

searcher <- searchAlgorithm('hillClimbing')
searcher <- searchAlgorithm('geneticAlgorithm')
searcher <- searchAlgorithm('tabu', list(tamTabuList = 4, iter = 5, intensification=2, iterIntensification=5, diversification=1, iterDiversification=5, verbose=FALSE) )
searcher <- searchAlgorithm('antColony')
searcher <- searchAlgorithm('sequentialForwardSelection')

# luego definimos el criterio de evaluacion

evaluator <- filterEvaluator('determinationCoefficient')
evaluator <- filterEvaluator("IEConsistency")
evaluator <- filterEvaluator('chiSquared')
evaluator <- filterEvaluator('MDLC') 
evaluator <- wrapperEvaluator("xgbLinear")
evaluator <- wrapperEvaluator("svmLinearWeights")
evaluator <- wrapperEvaluator("mlpWeightDecay")
evaluator <- wrapperEvaluator("lm")
evaluator <- wrapperEvaluator("knn")

# Utilizamos una combinacion de estos para encontrar la combinacion de atributos que maximiza la funcion de evaluacion
results <- featureSelection(data, 'equipo', searcher, evaluator)

results$bestFeatures

## Búsqueda directa

directSearcher <- directSearchAlgorithm('selectKBest', list(k=3))

results <- directFeatureSelection(data, 'equipo', directSearcher, evaluator)

results$bestFeatures


# Reduccion de Dimensiones -----

data2 <- data %>% 
  dplyr::select(-equipo) %>% 
  unique()


## Principal Component Analysis
PCA <- prcomp(data2)

barplot(PCA$sdev) ## graficamos el aporte de varianza de cada componente principal

features_PCA <- 
  predict(PCA) %>% 
  as.data.frame() 


# Multidimensional scaling 
d <- dist(data2) # distancias euclidianas entre entidades
data.MDS <- cmdscale(d, eig=TRUE, k = 2) # k es el numero de dimensiones de salida

features_MDS <- 
  data.MDS$points %>% 
  as.data.frame() 

# nonparametric Multi Dinemsional Scaling
library(MASS)
data.nMDS <- isoMDS(d, k=2) 

features_nMDS <- 
  data.nMDS$points %>% 
  as.data.frame() 

# t-distributed Stochastic Neighbor Embedding
library(Rtsne)
data.tsne <- Rtsne(data2, dims = 3, perplexity=30, max_iter = 500)

features_tsne <- 
  data.tsne$Y %>% 
  as.data.frame() 

# uniform manifold and projection
library(umap)
data.umap <- umap(data2)

features_umap <- data.umap$layout %>% as.data.frame()

# visualizamos

ggplot(features_PCA, aes(PC1,PC2)) + geom_point()

ggplot(features_MDS, aes(V1,V2)) + geom_point()

ggplot(features_nMDS, aes(V1,V2)) + geom_point()

ggplot(features_tsne, aes(V1,V2)) + geom_point()

ggplot(features_umap, aes(V1,V2)) + geom_point()


# Ejercicio de visualizacion -----

library(datasauRus)

stats <- datasaurus_dozen %>% 
  group_by(dataset) %>% 
  summarize(
    mean_x    = mean(x),
    mean_y    = mean(y),
    std_dev_x = sd(x),
    std_dev_y = sd(y),
    corr_pears  = cor(x, y, method = "pearson"),
    corr_spear  = cor(x, y, method = "spearman"),
    corr_kendall  = cor(x, y, method = "kendall"),
    simil_cos = simil(list(x,y), method = "cosine") %>% as.numeric(),   # funcion simil en la libreria proxy
    simil_jac = simil(list(x,y), method = "Jaccard") %>% as.numeric(),   # funcion simil en la libreria proxy
    simil_sm = simil(list(x,y), method = "simple matching") %>% as.numeric(),   # funcion simil en la libreria proxy
    simil_kul = simil(list(x,y), method = "Kulczynski1") %>% as.numeric(),   # funcion simil en la libreria proxy
    dist_euc = dist(list(x,y), method = "Euclidean") %>% as.numeric(),   
    dist_manh = dist(list(x,y), method = "Manhattan") %>% as.numeric(),  
    dist_sup = dist(list(x,y), method = "supremum") %>% as.numeric(),   
    median_x    = median(x),
    median_y    = median(y),
    CV_x = sd(x) / mean(x),
    CV_y = sd(y) / mean(y),
    max_x = max(x),
    max_y = max(y)
  )

stats 

# visualizamos las diferencias
ggplot(datasaurus_dozen, aes(x=x, y=y, colour=dataset)) +
  geom_point() +                                        
  theme_void() +                                        
  theme(legend.position = "none") +                     
  facet_wrap(~dataset, ncol=3)

# otras formas de visualizar las distribuciones 

ggplot(datasaurus_dozen, aes(x=x,colour=dataset))+
  geom_histogram(binwidth = 2)+                         
  theme_void()+
  theme(legend.position = "none")+
  facet_wrap(~dataset, ncol=3)

ggplot(datasaurus_dozen, aes(x=x,colour=dataset))+
  geom_density()+
  theme_void()+
  theme(legend.position = "none")+
  facet_wrap(~dataset, ncol=3)

ggplot(datasaurus_dozen, aes(x=x, colour=dataset))+
  geom_boxplot()+
  theme_void()+
  theme(legend.position = "none")+
  facet_wrap(~dataset, ncol=3)

ggplot(datasaurus_dozen, aes(x=x, y=y, colour=dataset))+
  geom_violin()+
  theme_void()+
  theme(legend.position = "none")+
  facet_wrap(~dataset, ncol=3)

ggplot(datasaurus_dozen, aes(x=x, y=dataset, colour=dataset)) +
  geom_point() +
  geom_boxplot() +
  theme_void() +
  theme(legend.position = "none")
