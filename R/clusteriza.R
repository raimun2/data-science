pacman::p_load(mclust, e1071, cluster, flexclust, factoextra, magrittr)

models <- c("kmeans", "hier", "gmm", "cmeans", "dbscan")

print(models)

clusteriza <- function(data, model, k = NULL, linkage = "complete", cov = "EII", minpts = 5,
                       eps = NULL, fuzz = 2){
  if(is.null(eps) & model == "dbscan") {eps <- data %>% cov() %>% diag() %>% sum() %>% sqrt()}
  if(is.null(k) & (model %in% c("kmeans", "cmeans", "gmm"))) {k <- floor(sqrt(nrow(data)))}
  if(model %in% models){
    
    if(model == "kmeans"){
      object   <- kmeans(data, k)
      clusters <- object$cluster
      
    } else if(model == "cmeans"){
      object   <- e1071::cmeans(data, k, m = fuzz)
      clusters <- object$cluster
      
    } else if(model == "gmm"){
      object   <- mclust::Mclust(data, G=k, modelNames = cov)
      clusters <- object$classification
      
    } else if(model == "dbscan"){
      object   <- dbscan::dbscan(data, eps=eps, minPts  = minpts)
      clusters <- object$cluster
      
    } else if(model == "hier"){
      object   <- hclust(dist(data), method = linkage)
      clusters <- cutree(object, k = k)
    }
    
    return(list("model" = object,"clusters" = clusters))
    
  
  } else {
    print("modelo no soportado")
  }
}

## evalucion

clus_cor <- function(data, clusters){
  
  k <- length(unique(clusters))
  
  mat_dis <- 
    data %>% 
    arrange(clusters) %>% 
    dist() %>% 
    as.matrix() 
  
  # construimos matriz de similaridad inicial
  mat_simil <- 1 / (1 + mat_dis)
  
  ideal_mat <- 
    matrix(0, 
           nrow = nrow(data), 
           ncol = nrow(data))
  
  walk(1:k, 
       function(x) {
         ideal_mat[which(clusters==x),which(clusters==x)] <<- 1  
       })
  
  
  # calculamos la correlacion 
  cor <-    cor(ideal_mat[upper.tri(ideal_mat)],
                mat_simil[upper.tri(mat_simil)])
  
  return(cor)
}


inspeccion_visual <- function(data, clusters){
  mat_dis <- 
    data %>% 
    arrange(clusters) %>% 
    dist() %>% 
    as.matrix() 
  
  image(mat_dis)
}


# funcion que evalua cohesion de cada cluster
cohesion <- function (x, data, clusters) {
  tempData <- data[which(clusters==x),]
  coh <- sum(dist2(tempData, colMeans(tempData))^2)
  
  return(coh)
}


# funcion que evalua cohesion del proceso de clustering
clus_cohes <- function(data, clusters){
  k <- length(unique(clusters))
  vec_coh <- map_dbl(unique(clusters), function(x) cohesion(x=x, data=data, clusters=clusters))
  return(vec_coh)
}


# funcion que evalua separacion de cada cluster
separacion <- function (x, data, clusters) {
  meanData <- colMeans(data)
  
  tempData <- data[which(clusters==x),]
  sep <- nrow(tempData)*sum((meanData-colMeans(tempData))^2)
  
  return(sep)
}

# funcion que evalua separacion del proceso de clustering
clus_sep <- function(data, clusters){
  k <- length(unique(clusters))
  

  vec_sep <- map_dbl(unique(clusters), function(x) separacion(x=x, data=data, clusters = clusters))
  return(vec_sep)
}
