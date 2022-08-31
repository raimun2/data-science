# cargo librerias
pacman::p_load(tidyverse, umap, mclust, e1071)
set.seed(42)

# cargo la data y aplico los mismos tratamientos que en el caso de DBScan
data.umap <- read_rds("data/partidos_futbol.rds") %>% 
  dplyr::select(where(is.numeric), -formationUsed) %>% 
  drop_na() %>% 
  umap() %>% 
  .$layout %>% 
  as.data.frame()


# exploramos la data 
data.umap %>% summary()

# exploramos graficamente la data
ggplot(data.umap) +
  geom_point(aes(V1,V2))

# usamos la libreria mclust y creamos un primer modelo con la forma EII
model_eii <- Mclust(data.umap, G=13, modelNames = "EII")

# vemos el resultado del model
summary(model_eii, parameters = TRUE)

# visualizamos los clusters
ggplot(data.umap) +
  geom_point(aes(V1,V2, color=factor(model_eii$classification))) +
  theme(legend.position = "none")

# vemos que sucede si solo consideramos los puntos con membresias superiores a 50% de un cluster
fuzz_eii <- apply(model_eii$z,1, function(x) ifelse(max(x) > .5, which(x==max(x)),0))

# visualizamos esta regla
ggplot(data.umap[fuzz_eii != 0,]) +
  geom_point(aes(V1,V2, color=factor(fuzz_eii[fuzz_eii != 0]))) +
  geom_point(data=data.umap[fuzz_eii == 0,], aes(V1,V2), alpha = 0.5, size=0.8) +
  theme(legend.position = "none")

# exploramos las diferentes opciones de modelos
mclust.options("emModelNames")

# creamos una funcion para graficar cualquier version del modelo
gmm_func <- function(data, k, model, threshold=NULL){
  # creo un modelo con los parametros
  model_gmm <- Mclust(data, G=k, modelNames = model)
  
  # si el parametro es nulo usar clasificacion, de lo contrario usar la maxima membresia siempre que supere el umbral
  if(is.null(threshold)){
    fuzz <- model_gmm$classification
  } else {
    fuzz <- apply(model_gmm$z,1, function(x) ifelse(max(x) > threshold, which(x==max(x)),0))
  }
  
  # generar grafico
  ggplot(data[fuzz != 0,]) +
    geom_point(aes(V1,V2, color=factor(fuzz[fuzz != 0]))) +
    geom_point(data=data[fuzz == 0,], aes(V1,V2), alpha = 0.5, size=0.8) +
    theme(legend.position = "none") +
    ggtitle(paste0(round(nrow(data[fuzz == 0,])/nrow(data)*100,0),"% de los datos sin cluster"))
  
}

# probamos la funcion con diferentes parametros
gmm_func(data.umap, 13, "VEE", 0.7)

# calculamos un modelo con TODAS las opciones
model_all <- Mclust(data.umap, G=1:30)

# visualizamos el BIC de todas las opciones
plot(model_all, what = "BIC")

# vemos el resultado del modelo optimo
model_all

# visualizamos el modelo optimo
ggplot(data.umap) + 
  aes(x=V1, y=V2, color=factor(model_all$classification)) + 
  geom_point(alpha=0.5) 

# ahora, para usar cmeans, utilizamos la libreria e1071

# la funcion cmeans recibe el parametro de fuzziness, admeas del numero de clusters
modelo_c_means <- cmeans(data.umap, 13, m=3) 

# exploramos las pertenencias de cada punto a cada cluster
modelo_c_means$membership %>% head()

# visualizamos los grupos obtenidos
ggplot(data.umap, aes(V1, V2, color = factor(modelo_c_means$cluster))) + 
  geom_point() +
  theme(legend.position = "none")

# creamos una funcion similar a la de GMM
cmeans_func <- function(data, k, fuzzifier, threshold=NULL){
  model_gmm <-  cmeans(data, k, m=fuzzifier)
  
  if(is.null(threshold)){
    fuzz <- modelo_c_means$cluster
  } else {
    fuzz <- apply(modelo_c_means$membership, 1, function(x) ifelse(max(x) > threshold, which(x==max(x)),0))
  }
  
  ggplot(data[fuzz != 0,]) +
    geom_point(aes(V1,V2, color=factor(fuzz[fuzz != 0]))) +
    geom_point(data=data[fuzz == 0,], aes(V1,V2), alpha = 0.5, size=0.8) +
    theme(legend.position = "none") +
    ggtitle(paste0(round(nrow(data[fuzz == 0,])/nrow(data)*100,0),"% de los datos sin cluster"))
  
}

# probamos la funcion con diferentes parametros
cmeans_func(data.umap, 13, 3, 0.7)


# creamos una funcion para calcular el coeficiente de particion difusa a partir de las pertenencias
FPC <- function(membresias){
  matriz <- membresias%*%t(membresias) # producto matricial
  
  FPC <- sum(matriz*diag(nrow(matriz)))/nrow(matriz)
  
  return(FPC)

}

# evaluamos el FPC del modelo cmeans
FPC(modelo_c_means$membership)

# evaluamos el FPC del modelo GMM
FPC(model_all$z)
