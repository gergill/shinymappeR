generate_circle <- function(num_points, noise) {
  return(data.frame(
    x = sapply(1:num_points, cos) + runif(num_points, 0, noise),
    y = sapply(1:num_points, sin) + runif(num_points, 0, noise)
    )
  )
}

generate_figure_eight <- function(num_points, noise) {
  data.frame(
    x = sapply(1:num_points, function(x) cos(x) / (1 + sin(x)^2)) + runif(num_points, 0, noise),
    y = sapply(1:num_points, function(x) sin(x)*cos(x) / (1 + sin(x)^2)) + runif(num_points, 0, noise)
  )
}

# stolen from sir jacob miller
generate_spiral <- function(n=1000, noise=0.1){
  t <- runif(n, 0, 4 * pi)
  h <- runif(n, 0, 1)

  hole_mask <- (t > 1.5*pi & t < 1.75*pi) & (h>0.25 & h<0.75)
  t <- t[!hole_mask]
  h <- h[!hole_mask]

  x <- t*cos(t) + rnorm(length(t), 0, noise)
  z <- t*sin(t) + rnorm(length(t), 0, noise)

  return(data.frame(x=x, y=z))
}

generate_barbell <- function(n, noise) {
  r1 = sqrt(runif(n*.45, 0, 1))
  a1 = runif(n*.45, 0, 2*pi)
  disk1 = data.frame(x=r1*cos(a1) - 1, y=r1*sin(a1))

  r2 = sqrt(runif(n*.45, 1-noise, 1))
  a2 = runif(n*.45, 0, 2*pi)
  disk2 = data.frame(x=r2*cos(a2) + 1, y=r2*sin(a2))

  line = data.frame(x = runif(n/20, -.5, .5), y = rep(0, n/20))

  return(rbind(disk1, disk2, line))
}
