filter_data <- function(input, data) {
  switch(input,
         "eccentricity" = eccentricity(data),
         "PCA-1" = pca_filter(data),
         "project to x" = data$x,
         "project to y" = data$y,
         data$x)
}

pca_filter <- function(data) {
  pca_output = prcomp(data, center = FALSE, scale. = FALSE)
  return(pca_output$x[,1])
}

eccentricity <- function(data) {
  dists = as.matrix(dist(data))
  return(apply(dists, 1, sum))
}

