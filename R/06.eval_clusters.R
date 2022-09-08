pacman::p_load(tidyverse, umap, factoextra, flexclust, cluster)
set.seed(42)

data_raw <- read_rds("data/partidos_futbol.rds")
source("R/clusteriza.R")


# cargo la data y aplico los mismos tratamientos que en el caso de DBScan
data.umap <- data_raw %>% 
  dplyr::select(where(is.numeric), -formationUsed) %>% 
  drop_na() %>% 
  umap() %>% 
  .$layout %>% 
  as.data.frame() %>% 
  sample_n(1000)

ggplot(data.umap, aes(x=V1, y=V2))+
  geom_point()


# HOPKINS

#Calculamos el estadistico  de hopkin statistic 
res <- get_clust_tendency(data.umap, n = 30, graph = FALSE)

res

data_rand <- apply(data.umap, 2, function(x) runif(1000,min(x),max(x))) %>% as_tibble()

#hopking  random data
res_r <- get_clust_tendency(data_rand, n = 30, graph = FALSE)
res_r


ggplot(data_rand, aes(x=V1, y=V2))+
  geom_point()


#Generamos un modelo de clustering sencillo para probar
model <- kmeans(data.umap, 3)

#ordenamos observaciones basandonos en cluster

mat_dis <- 
  data.umap %>% 
  arrange(model$cluster) %>% 
  dist() %>% 
  as.matrix() 

image(mat_dis)

inspeccion_visual(data.umap, model$cluster)


# analisis de correlacion

# construimos similaridad ideal 1 con los puntos del mismo cluster, 0 eoc

ideal_mat <- 
  matrix(0, 
         nrow = nrow(data.umap), 
         ncol = nrow(data.umap))

ideal_mat[which(model$cluster==1),which(model$cluster==1)] <- 1
ideal_mat[which(model$cluster==2),which(model$cluster==2)] <- 1
ideal_mat[which(model$cluster==3),which(model$cluster==3)] <- 1

# construimos matriz de similaridad inicial
mat_simil <- 1 / (1 + mat_dis)

# calculamos la correlacion 
cor(ideal_mat[upper.tri(ideal_mat)],
    mat_simil[upper.tri(mat_simil)])

clus_cor(data.umap, model$cluster)


#Cohesion

withinCluster <- numeric(3)

for (i in 1:3){
  tempData <- data.umap[which(model$cluster==i),]
  withinCluster[i]  <- sum(dist2(tempData,colMeans(tempData))^2)
}

sum(withinCluster) # cohesion
model$tot.withinss

cohesiones <- clus_cohes(data.umap, model$cluster)

sum(cohesiones)

#Separacion

meanData <- colMeans(data.umap)
SSB <- numeric(3)
for (i in 1:3){
  tempData <- data.umap[which(model$cluster==i),]
  SSB[i] <- nrow(tempData)*sum((meanData-colMeans(tempData))^2)
}

(separation <- sum(SSB))

model$betweenss

separaciones <- clus_sep(data.umap, model$cluster)
sum(separaciones)

#distancia de todos los puntos al centro

(totalSum <- sum(dist2(data.umap, meanData)^2))

model$totss

sum(separaciones + cohesiones)

# Coeficiente de silueta

coefSil <- silhouette(model$cluster,dist(data.umap))

summary(coefSil)

# visualizamos las siluetas
fviz_silhouette(coefSil) + 
  coord_flip()

# buqueda de k en kmeans en funcion de silueta
coefSil <- numeric(30)
for (k in 2:30){
  modelk <- kmeans(data.umap, k,5)
  temp <- silhouette(modelk$cluster,dist(data.umap))
  coefSil[k] <- mean(temp[,3])
}

tempDF <- data.frame(CS=coefSil,
                     K=c(1:30))

ggplot(tempDF, aes(x=K,y=CS)) + 
  geom_line() + 
  scale_x_continuous(breaks=c(1:30))


## 

evaluacion_cluster <- function(data, clusters){
  coefSil <- silhouette(clusters,dist(data))
  cohesiones <- clus_cohes(data, clusters)
  separaciones <- clus_sep(data, clusters)
  correlacion <- clus_cor(data, clusters)
  
  
  
  return(list(correlacion, coefSil, cohesiones, separaciones))
}


i <- 1
k <- 10

modelo_ik <- clusteriza(data.umap, models[i], k = k)

eval <- evaluacion_cluster(data.umap, modelo_ik$clusters)


