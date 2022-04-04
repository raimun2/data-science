# cargo librerias
pacman::p_load(tidyverse, Rtsne, ggdendro)
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

# exploramos la data 
data_tsne %>% summary()

# exploramos graficamente la data

ggplot(data_tsne) +
  geom_point(aes(V1,V2))

# calculamos la distancia euclideana
d <- dist(data_tsne)

# analizo graficamente la distribucion de las distancias entre puntos
hist(d)

# hacemos un modelo jerarquico con distancia completa
model_complete <- hclust(d, method="complete")

# obtenemos una sintesis del modelo
summary(model_complete)

# usamos la funcion dentro de R base para graficar el dendrograma
plot(model_complete)
abline(h = 40, col="red")

# replicamos el grafico con ggplot
ggdendrogram(model_complete, theme_dendro = TRUE) +
  geom_hline(yintercept = 40, col="red")

# analizamos el arbol si lo cortamos en h = 40
groups <- cutree(model_complete, h = 40)  
groups %>% unique() %>% length()

# asignamos grupos a los datos creando una nueva variable
data_tsne$cluster_complete <- factor(groups)

# visualizamos los grupos resultantes
ggplot(data_tsne) +
  geom_point(aes(V1,V2, col=cluster_complete)) +
  theme(legend.position = "none")

# evaluemos como evoluciona el numero de clusters con h

# creamos un vector vacio para almacenar los resultados
res <- tibble("h" = quantile(d, probs  = (1:100)/100), n = 0)

# recorremos los 100 percentiles y vamos rellenando el vector con la distancia intra cluster
for (i in 1:100){
  groups <- cutree(model_complete, h = res$h[i])
  res$n[i] <- groups %>% unique() %>% length()
}  

# visualizamos el numero de grupos vs h
ggplot(res, aes(h, n)) + 
  geom_point()

# existen muchas otras definiciones para las distancias y para la agregacion


metodos_distancias <- c("euclidean", "maximum", "manhattan", "binary", "minkowski")
metodos_agregacion <- c("complete", "average", "median", "centroid", "single",
                        "ward.D", "ward.D", "mcquitty")

d2 <- dist(data_tsne, method = metodos_distancias[1])

model2 <- hclust(d2, method=metodos_agregacion[5]) 

summary(model2)

# ahora hagamos una funcion para probar cualquier combinacion

clusters_jer <- function(data, i_d, i_a, k){
  
  d_i <- dist(data, method = metodos_distancias[i_d])
  
  model_i <- hclust(d_i, method=metodos_agregacion[i_a]) 
  
  res <- tibble("h" = quantile(d_i, probs  = (1:100)/100), n = 0)
  for (i in 1:100){
    groups <- cutree(model_i, h = res$h[i])
    res$n[i] <- groups %>% unique() %>% length()
  }  
  
  # encontramos la minima distancia que cumple el nro de clusters
  res$dist_k <- abs(res$n - k)
  h_k <- res$h[which(res$dist_k == min(res$dist_k))][1]
  
  groups <- cutree(model_i, h = h_k)
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
clusters_jer(data_tsne, 2, 5, 13)


