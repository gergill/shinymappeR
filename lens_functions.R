eccentricity <- function(data) {
  dists = as.matrix(dist(data))
  return(apply(dists, 1, sum))
}

