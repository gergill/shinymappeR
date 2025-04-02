pca_filter <- function(data, n) {
  pca_output = prcomp(data, center = FALSE, scale. = FALSE)
  return(pca_output$x[,n])
}

eccentricity <- function(data) {
  dists = as.matrix(dist(data))
  return(apply(dists, 1, sum))
}

