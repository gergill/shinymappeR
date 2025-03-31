pca_filter <- function(data, n) {
  pca_output = prcomp(data, center = FALSE, scale. = FALSE)
  return(pca_output$x[,n])
}
