# cargo librerias
pacman::p_load(tidyverse, Rtsne, mclust, e1071, cluster, flexclust, factoextra)
set.seed(42)

# cargo la data y aplico los mismos tratamientos que en el caso de DBScan
data_tsne  <- read.csv("data/video_games_sales.csv") %>% 
  mutate(User_Score = as.numeric(User_Score)) %>%
  filter(!(is.na(Critic_Score) | is.na(User_Score))) %>%
  select(Critic_Score, User_Score, User_Count, Global_Sales) %>% 
  unique() %>% 
  Rtsne() %>% 
  .$Y %>% 
  as.data.frame()

# exploramos graficamente la data
ggplot(data_tsne) +
  geom_point(aes(V1,V2))

# antes de clusterizar, evaluamos el grado de clusterizacion natural de los datos de acuerdo
# al estadistico de hopkins, que esta implementado en la libreria factoextra. 

#Calcula el hopkins statistic 
(res <- get_clust_tendency(data_tsne, n = 30, graph = FALSE))

## Evaluacion de clusters

# ejecutamos un modelo para probar su clusterizacion
modelo_kmeans <- kmeans(data_tsne, 13)

# almacenamos los clusters en un vector que contiene el numero de cada cluster y el indice
clusters <- modelo_kmeans$cluster

# inspeccion visual de matriz de distancias

# calculamos las distancias de los datos
distancias <- dist(data_tsne) %>% as.matrix()

# generamos indices con la ubicacion de los clusters ordenados
clusters_i <-  sort(clusters, index.return=TRUE)

#reordeno filas y columnas en base al cluster obtenido
distancias <- distancias[clusters_i$ix, clusters_i$ix]
rownames(distancias) <- c(1:nrow(data_tsne))
colnames(distancias) <- c(1:nrow(data_tsne))

# pero la matriz de distancias es muy grande para graficar
print(object.size(distancias), units = "Mb")

# la extraemos 1 de cada 10 filas y columnas
n <- 10
ids <- (1:floor(nrow(distancias)/n))*n
dist_reducida <- distancias[ids,ids]

# bajo considerablemente el tamaño
print(object.size(dist_reducida), units = "Mb")

# generamos la imagen de la matriz para la inspececion visual
image(dist_reducida)


# Siguente metrica es el indice de correlacion

# primero construyo matriz de correlacion ideal (cada entidad correlaciona 1 con su cluster)

# creo matriz llega de ceros
matriz_ideal <- matrix(0, nrow = nrow(data_tsne), ncol = nrow(data_tsne))
# para cada cluster reemplazo con 1 en aquellas entidades que pertenezcan a el
for(k in unique(clusters_i$x)){
  matriz_ideal[which(clusters_i$x==k), which(clusters_i$x==k)]  <- 1
}

# construyo matriz de disimilitud en base a la distancia ordenada
tempDist2 <- 1/(1+distancias)

# Calcula correlacion entre matriz de disimilitud y matriz optima
cor <- cor(matriz_ideal[upper.tri(matriz_ideal)],tempDist2[upper.tri(tempDist2)])

print(cor)


# a continuacion calculamos el indice de cohesion

# creamos vector vacio para medir la cohesion en cada cluster
withinCluster <- numeric(length(unique(clusters)))
# para cada cluster
for (i in 1:length(withinCluster)){
  # extraigo los puntos pertenecientes al cluster i
  tempData <- data_tsne[which(clusters == i),]
  # calculo la suma de distancias al cuadrado entre cada punto y el centroide
  withinCluster[i] <- sum(dist2(tempData,colMeans(tempData))^2)
}
# calculo la suma total de cohesion para todos los clusters
cohesion <- sum(withinCluster)

#es equivalente a model$tot.withinss en k-means
print(c(cohesion, modelo_kmeans$tot.withinss))

# ahora es turno del coeficiente de separacion

centroide_total <- colMeans(data_tsne)
# creamos vector vacio para almacenar separacion en cada cluster
separation <-  numeric(length(unique(clusters)))
# para cada cluster
for (i in 1:length(separation)){
  # extraigo los puntos pertenecientes al cluster i
  tempData <- data_tsne[which(clusters==i),]
  # calculo la separacion como la distancia promedio entre cada punto de un cluster y el resto
  separation[i] <- nrow(tempData)*sum((centroide_total-colMeans(tempData))^2)
}
(sum(separation))


# Y finalmente aplicamos el coeficiente de silueta, implementado en libreria cluster

# el coeficiente recibe las etiquetas de cluster y la matriz de distancias de la data original
coefSil <- silhouette(clusters, dist(data_tsne))
summary(coefSil)

#visualizamos el codigo de silueta de cada cluster
fviz_silhouette(coefSil) + coord_flip()


# Utilizamos el coeficiente de silueta para encontrar el mejor valor de K

# creamos vector vacio para almacenar siluetas
siluetas <- numeric(20)

for (k in 2:20){
  # ejecutamos kmedias con k centroides
  modelo <- kmeans(data_tsne, centers = k)
  # cramos objeto con la silueta
  temp <- silhouette(modelo$cluster, dist(data_tsne))
  # almacenamos la silueta promedio del modelo
  siluetas[k] <- mean(temp[,3])
}
tempDF <- data.frame(CS=siluetas, K=c(1:20))

# visualizamos
ggplot(tempDF, aes(x=K, y=CS)) + 
  geom_line() +
  scale_x_continuous(breaks=c(1:20)) +
  geom_vline(xintercept = which(tempDF$CS == max(tempDF$CS)), col = "red")


source("R/clusteriza.R")

