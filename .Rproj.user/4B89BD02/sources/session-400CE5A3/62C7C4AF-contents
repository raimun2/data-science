# cargo librerias
pacman::p_load(tidyverse, umap, ggdendro, cluster)
set.seed(42)


data_raw <- read_rds("data/partidos_futbol.rds")

# analisis de clusters naturales ----
data <- data_raw %>% 
  select(where(is.numeric), -formationUsed) %>% 
  drop_na()

model.umap <- umap(data)

data.umap <- 
  model.umap$layout %>% 
  as.data.frame()

# exploramos la data 
data.umap %>% summary()

# exploramos graficamente la data

ggplot(data.umap) +
  geom_point(aes(V1,V2))

# calculamos la distancia euclideana
d <- dist(data.umap)

# analizo graficamente la distribucion de las distancias entre puntos
hist(d)

# hacemos un modelo jerarquico con distancia completa
model_complete <- hclust(d)

# obtenemos una sintesis del modelo
summary(model_complete)

# usamos la funcion dentro de R base para graficar el dendrograma
plot(model_complete)
abline(h = 10, col="red")

# replicamos el grafico con ggplot
ggdendrogram(model_complete, theme_dendro = TRUE) +
  geom_hline(yintercept = 10, col="red") +
  coord_flip()

# analizamos el arbol si lo cortamos en h = 40
groups <- cutree(model_complete, h = 10)  
groups %>% unique() %>% length()


# visualizamos los grupos resultantes
ggplot(data.umap) +
  geom_point(aes(V1,V2, col=factor(groups))) +
  theme(legend.position = "none")

# evaluemos como evoluciona el numero de clusters con h

# creamos un vector vacio para almacenar los resultados
res <- tibble("h" = quantile(d, probs  = (1:99)/100), n = 0, silh = 0)

# recorremos los 100 percentiles y vamos rellenando el vector con la distancia intra cluster
for (i in 1:99){
  groups <- cutree(model_complete, h = res$h[i])
  
  res$silh[i] <- summary(silhouette(groups,dist(data.umap)))$si.summary[3]
  res$n[i] <- groups %>% unique() %>% length()
}  

# visualizamos la silueta vs h
ggplot(res, aes(h, silh)) + 
  geom_point()

# existen muchas otras definiciones para las distancias y para la agregacion


metodos_distancias <- c("mahalanobis", "euclidean", "maximum", "manhattan", "binary", "minkowski")
metodos_agregacion <- c("complete", "average", "median", "centroid", 
                        "single", "ward.D", "ward.D", "mcquitty")


# cx - estima matriz de covariabza si es que no esta presente
dist_dd <- function(data, method = "mahalanobis", cx=NULL){
  if(method == "mahalanobis"){
    if(is.null(cx)) cx <- cov(data)
    out <- lapply(1:nrow(data), function(i) {
      mahalanobis(x = data, 
                  center = do.call("c", data[i, ]),
                  cov = cx)
    })
    return(as.dist(do.call("rbind", out)))
  } else{
    return(dist(data,method))
  }
}


d2 <- dist_dd(data.umap, method = metodos_distancias[1])

model2 <- hclust(d2, method=metodos_agregacion[5]) 

summary(model2)


# ahora hagamos una funcion para probar cualquier combinacion
clusters_jer <- function(data, i_d, i_a, k){
  
  d_i <- dist_dd(data, method = metodos_distancias[i_d])
  
  model_i <- hclust(d_i, method=metodos_agregacion[i_a]) 
  
  # encontramos la minima distancia que cumple el nro de clusters
  groups <- cutree(model_i, k)
  data$cluster <- factor(groups)
  
  ggplot(data) +
    geom_point(aes(V1,V2, col=cluster)) +
    theme(legend.position = "none") +
    ggtitle(paste0(k," clusters con distancia ",
                   metodos_distancias[i_d],
                   "\n y método de agregación ",
                   metodos_agregacion[i_a]))
  
}

# probamos la funcion con distintos valores de i_d, i_a, y k
clusters_jer(data.umap, 1, 8, 10)


