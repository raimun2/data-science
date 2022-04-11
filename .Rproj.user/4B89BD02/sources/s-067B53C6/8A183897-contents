pacman::p_load(mclust, e1071, cluster, flexclust, factoextra, magrittr)

models <- c("kmeans", "hier", "gmm", "cmeans", "dbscan")

clusteriza <- function(data, model, k = NULL, linkage = "complete", cov = "EII", minpts = 5,
                       eps = NULL, fuzz = 2, return.model = FALSE){
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
    
    if(return.model){
      return(object)  
    } else {
      return(clusters)
    }
  
  } else {
    print("modelo no soportado")
  }
}

