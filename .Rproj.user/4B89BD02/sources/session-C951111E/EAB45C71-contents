pacman::p_load(magick, tidyverse, factoextra, umap)
set.seed(42)

data_raw <- read_rds("data/partidos_futbol.rds")


# analisis de clusters naturales ----
data <- data_raw %>% 
  select(where(is.numeric), -formationUsed) %>% 
  drop_na()

get_clust_tendency(data, n = 30, graph = FALSE)

model.umap <- umap(data)

data.umap <- 
  model.umap$layout %>% 
  as.data.frame()

get_clust_tendency(data.umap, n = 30, graph = FALSE)

ggplot(data.umap, aes(V1, V2)) + 
  geom_point()

# modelo de k-means ----

# version basica de kmeans
kmedias <- function(data, k, itermax = 10){
  
  # defino los centroides aleatoriamente seleccionando una muestra de la data recibida
  centroids <- data[sample(1:nrow(data), k),]
  
  # creo 2 variables auxiliares del mismo largo que me permitiran comparar si el algoritmo convergio
  cluster <- 1:nrow(data)
  cluster_iter <- cluster*0
  
  # creo un for para iterar hasta las iteraciones maximas
  for(i in 1:itermax){
    if(!mean(cluster_iter == cluster)==1){ # si el algoritmo aun no converge
      # calculo distancia de puntos con centroides de acuerdo a funcion
      distk <- sqrt(matrix(rowSums(expand.grid(rowSums(centroids*centroids),rowSums(data*data))), # hago calculo de producto punto enntre matrices
                           nrow=nrow(centroids)) - # calculo el tamaño de la matriz resultante
                      2. * as.matrix(centroids) %*% t(as.matrix(data)))
      
      cluster_iter <- cluster # reasigno la variable auxiliar al cluster obtenido en la iteracion anterior
      cluster <- apply(distk, 2, function(x) which(x==min(x))[1]) # identifico el cluster mas cercano a cada punto
      dist_min <- apply(distk, 2, function(x) min(x)) # identifico la distancia minima al cluster mas cercano
      output <- data.frame(dist_min, data, cluster) # construyo salida del modelo juntando la data con sus clusters asignados
      dist_clusts <- aggregate(.~cluster, output, mean) # agrego los datos por clusters obteniendo coordenadas y distancias medias
      centroids <- dist_clusts[,-(1:2)] # redefino los centroides 
    } 
  }
  
  return(list("clusters" = cluster, "centroides" = centroids)) # funcion devuelve una lista con los clusters de cada punto y con los centroides
  
}

# comparacion con algoritmo nativo kmeans ----

tictoc::tic()
modelo1 <- kmedias(data.umap, k = 12)
tictoc::toc()

tictoc::tic()
modelo2 <- kmeans(data.umap, 12)
tictoc::toc()

library(patchwork)
p1 <- ggplot(data.umap, aes(V1, V2, col = factor(modelo1$clusters))) +
  geom_point() +
  theme(legend.position = "bottom",
        legend.title = element_blank())
p2 <- ggplot(data.umap, aes(V1, V2, col = factor(modelo2$cluster))) +
  geom_point() +
  theme(legend.position = "bottom",
        legend.title = element_blank())

p1 + p2


# ejemplo imagen UAI ----
imagen <- image_read("data/uai.jpg")

# genero variable con los datos de valores rgb
imagen_num <- imagen %>% 
  image_data(channels = "rgb") %>% 
  as.numeric()
  
# transformo a color para evaluar numero de colores
colorvector <- rgb(imagen_num[,,1], 
                   imagen_num[,,2], 
                   imagen_num[,,3], 
                   maxColorValue = 1)

colorvector  %>%  unique() %>%  length()

# creo dataframe con columnas rgb
colors_df <- 
  col2rgb(colorvector) %>% 
  t() %>% 
  as_tibble()

# simplifico colores redondeando

# redondeo los colores para llegar a 30 colores
n <- 55
t_colors_df <- floor(colors_df/n)*n

# creo vector de colores transformado
t_colorvector <- rgb(t_colors_df/255)

t_colorvector  %>%  unique() %>%  length()

# creo una copia de la matriz original y la relleno con los valores tranformados
t_imagen_num <- imagen_num
t_imagen_num[,,1] <- t_colors_df$red/255
t_imagen_num[,,2] <- t_colors_df$green/255
t_imagen_num[,,3] <- t_colors_df$blue/255

# dibujo la imagen original y la transformada
par(mfrow=c(2,1), cex=0.7, mai=c(0,0,0,0))
plot(c(100, 150), c(300, 350), type = "n", xlab = "", ylab = "", bty="n", axes=F)
graphics::rasterImage(imagen_num,  100, 300, 150, 350, interpolate = FALSE)
plot(c(100, 150), c(300, 350), type = "n", xlab = "", ylab = "", bty="n", axes=F)
graphics::rasterImage(t_imagen_num,  100, 300, 150, 350, interpolate = FALSE)




# genero un modelo de kmeans con 30 centroides
modelo_kmeans <- kmeans(colors_df, 30, iter.max  = 20)

# extraigo los centroides y genero columna cluster
centroides <- 
  modelo_kmeans$centers %>% 
  as_tibble() %>% 
  mutate(cluster = 1:30)


# extraigo los clusters de cada punto y creo una tabla para almacenarlos
t_colors_df_kmeans <- 
  tibble(cluster = modelo_kmeans$cluster) %>% 
  inner_join(centroides, by = "cluster")



# creo una copia de la imagen original y reemplazo valores por centroides obtenidos
t_imagen_num_kmeans <- imagen_num
t_imagen_num_kmeans[,,1] <- t_colors_df_kmeans$red/255
t_imagen_num_kmeans[,,2] <- t_colors_df_kmeans$green/255
t_imagen_num_kmeans[,,3] <- t_colors_df_kmeans$blue/255

# dibujo la imagen original, y las 2 transformadas
par(mfrow=c(3,1), cex=0.7, mai=c(0,0,0,0))
plot(c(100, 150), c(300, 350), type = "n", xlab = "", ylab = "", bty="n", axes=F)
graphics::rasterImage(imagen_num,  100, 300, 150, 350, interpolate = FALSE)
plot(c(100, 150), c(300, 350), type = "n", xlab = "", ylab = "", bty="n", axes=F)
graphics::rasterImage(t_imagen_num,  100, 300, 150, 350, interpolate = FALSE)
plot(c(100, 150), c(300, 350), type = "n", xlab = "", ylab = "", bty="n", axes=F)
graphics::rasterImage(t_imagen_num_kmeans,  100, 300, 150, 350, interpolate = FALSE)



# corro el modelo con las mismas condiciones del kmeans
modelo_kmedias_artesanal <- kmedias(colors_df, 30, itermax = 20)

centroides_artesa <- 
  modelo_kmedias_artesanal$centroides %>% 
  as_tibble() %>% 
  mutate(clusters = 1:30)



t_colors_df_kmeans_art <- 
  tibble(clusters = modelo_kmedias_artesanal$clusters) %>% 
  inner_join(centroides_artesa, by = "clusters")


t_imagen_num_kmeans_art <- imagen_num
t_imagen_num_kmeans_art[,,1] <- t_colors_df_kmeans_art$red/255
t_imagen_num_kmeans_art[,,2] <- t_colors_df_kmeans_art$green/255
t_imagen_num_kmeans_art[,,3] <- t_colors_df_kmeans_art$blue/255

par(mfrow=c(4,1), cex=0.7, mai=c(0,0,0,0))
plot(c(100, 150), c(300, 350), type = "n", xlab = "", ylab = "", bty="n", axes=F)
graphics::rasterImage(imagen_num,  100, 300, 150, 350, interpolate = FALSE)
plot(c(100, 150), c(300, 350), type = "n", xlab = "", ylab = "", bty="n", axes=F)
graphics::rasterImage(t_imagen_num,  100, 300, 150, 350, interpolate = FALSE)
plot(c(100, 150), c(300, 350), type = "n", xlab = "", ylab = "", bty="n", axes=F)
graphics::rasterImage(t_imagen_num_kmeans,  100, 300, 150, 350, interpolate = FALSE)
plot(c(100, 150), c(300, 350), type = "n", xlab = "", ylab = "", bty="n", axes=F)
graphics::rasterImage(t_imagen_num_kmeans_art,  100, 300, 150, 350, interpolate = FALSE)



