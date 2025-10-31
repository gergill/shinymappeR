library(R6)

# ---- LensFunction Class -----------------------------------------------------

LensFunction <- R6::R6Class(
  "LensFunction",
  public = list(
    name = NULL,
    description = NULL,
    projection = FALSE, # TRUE if lens defines a projection line
    param_spec = NULL, # parameter specs
    lens_fn = NULL, # f(data, params...) => numeric vector
    projection_fn = NULL, # optional: f(data, params...) => line info

    initialize = function(name, description, projection = FALSE,
                          param_spec = list(), lens_fn, projection_fn = NULL) {
      self$name <- name
      self$description <- description
      self$projection <- projection
      self$param_spec <- param_spec
      self$lens_fn <- lens_fn
      self$projection_fn <- projection_fn
    },
    compute = function(data, params = list()) {
      args <- lapply(names(self$param_spec), function(p) {
        if (!is.null(params[[p]])) {
          params[[p]]
        } else {
          self$param_spec[[p]]$value
        }
      })
      names(args) <- names(self$param_spec)
      do.call(self$lens_fn, c(list(data), args))
    },
    get_ui = function(ns = NS(NULL)) {
      if (length(self$param_spec) == 0) {
        return(NULL)
      }
      lapply(names(self$param_spec), function(p) {
        spec <- self$param_spec[[p]]
        if (spec$type == "slider") {
          sliderInput(
            inputId = ns(p),
            label = spec$label,
            min = spec$min,
            max = spec$max,
            value = spec$value,
            step = spec$step
          )
        } else if (spec$type == "numeric") {
          numericInput(
            inputId = ns(p),
            label = spec$label,
            value = spec$value,
            min = spec$min,
            max = spec$max
          )
        } else if (spec$type == "select") {
          selectInput(
            inputId = ns(p),
            label = spec$label,
            choices = spec$choices,
            selected = spec$value
          )
        }
      })
    }
  )
)

# ---------------------------------------------------------------------------
# ---- Example Lens Functions ----------------------------------------------
# ---------------------------------------------------------------------------

# Utility: eccentricity
eccentricity <- function(data) {
  dists <- as.matrix(dist(data))
  apply(dists, 1, sum)
}

# Projection to X
project_x_lens <- LensFunction$new(
  name = "project to x",
  description = "Projection of data onto the X axis.",
  projection = TRUE,
  lens_fn = function(data) data$x,
  projection_fn = function(data) list(direction = c(1, 0), intercept = 0)
)

# Projection to Y
project_y_lens <- LensFunction$new(
  name = "project to y",
  description = "Projection of data onto the Y axis.",
  projection = TRUE,
  lens_fn = function(data) data$y,
  projection_fn = function(data) list(direction = c(0, 1))
)

# PCA-1 projection
pca1_lens <- LensFunction$new(
  name = "PCA-1",
  description = "Projection onto the first principal component.",
  projection = TRUE,
  lens_fn = function(data) {
    prcomp(data, center = TRUE, scale. = FALSE)$x[, 1]
  },
  projection_fn = function(data) {
    comp <- prcomp(data, center = TRUE, scale. = FALSE)
    list(vector = comp$rotation[, 1], center = colMeans(data))
  }
)

# PCA-2 projection
pca2_lens <- LensFunction$new(
  name = "PCA-2",
  description = "Projection onto the second principal component.",
  projection = TRUE,
  lens_fn = function(data) {
    prcomp(data, center = TRUE, scale. = FALSE)$x[, 2]
  },
  projection_fn = function(data) {
    comp <- prcomp(data, center = TRUE, scale. = FALSE)
    list(vector = comp$rotation[, 2], center = colMeans(data))
  }
)

# Non-projection: Eccentricity
eccentricity_lens <- LensFunction$new(
  name = "eccentricity",
  description = "Sum of distances from each point to all others.",
  projection = FALSE,
  lens_fn = function(data) eccentricity(data)
)

# ---------------------------------------------------------------------------
# ---- Theta Lens -----------------------------------------------------------
# ---------------------------------------------------------------------------

theta_lens <- LensFunction$new(
  name = "theta lens",
  description = paste(
    "Projects points onto the tangent line of a circle",
    "encompassing the dataset, at angle θ ∈ [0, 2π]."
  ),
  projection = TRUE,
  param_spec = list(
    theta = list(
      type = "slider",
      label = "Theta (radians)",
      min = 0,
      max = pi,
      value = 0,
      step = 0.01
    )
  ),
  lens_fn = function(data, theta = 0) {
    # Ensure data is in numeric matrix form
    mat <- as.matrix(data)
    center <- colMeans(mat)
    r <- max(sqrt(rowSums((mat - matrix(center, nrow(mat), 2, byrow = TRUE))^2)))

    # Point on circle
    px <- center[1] + r * cos(theta)
    py <- center[2] + r * sin(theta)
    ptheta <- c(px, py)

    # Tangent direction (unit)
    tangent <- c(-sin(theta), cos(theta))

    # Project each data point onto tangent line
    projections <- (mat[, 1] - ptheta[1]) * tangent[1] +
      (mat[, 2] - ptheta[2]) * tangent[2]
    projections
  },
  projection_fn = function(data, theta = 0) {
    center <- colMeans(data)
    r <- max(sqrt((data$x - center[1])^2 + (data$y - center[2])^2))
    point <- c(center[1] + r * cos(theta), center[2] + r * sin(theta))
    tangent <- c(-sin(theta), cos(theta))
    list(
      vector = tangent,
      point = point,
      center = center,
      radius = r
    )
  }
)

# ---------------------------------------------------------------------------
# ---- Lens Registry and Accessors -----------------------------------------
# ---------------------------------------------------------------------------

lens_registry <- list(
  "project to x" = project_x_lens,
  "project to y" = project_y_lens,
  "PCA-1" = pca1_lens,
  "PCA-2" = pca2_lens,
  "eccentricity" = eccentricity_lens,
  "theta lens" = theta_lens
)

get_lens <- function(name) {
  key <- gsub(" ", "", tolower(name))
  clean <- gsub(" ", "", tolower(names(lens_registry)))
  if (key %in% clean) {
    lens_registry[[names(lens_registry)[which(clean == key)]]]
  } else {
    lens_registry[[1]]
  }
}
