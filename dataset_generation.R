library(R6)

# ---- DatasetGenerator class definition -------------------------------------

DatasetGenerator <- R6::R6Class(
  "DatasetGenerator",
  public = list(
    name = NULL,
    description = NULL,
    param_spec = NULL,   # list of parameter metadata
    generate_fn = NULL,  # function that produces a data.frame(x, y)

    initialize = function(name, description, param_spec, generate_fn) {
      self$name <- name
      self$description <- description
      self$param_spec <- param_spec
      self$generate_fn <- generate_fn
    },

    # Data generation method
    generate = function(params = list()) {
      args <- lapply(names(self$param_spec), function(p) {
        if (!is.null(params[[p]])) params[[p]]
        else self$param_spec[[p]]$value
      })
      names(args) <- names(self$param_spec)
      do.call(self$generate_fn, args)
    },

    # UI representation of the generator's parameters
    get_ui = function(ns = NS(NULL)) {
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

# ------------------------------------------------------------------
# ---- GENERATORS---------------------------------------------------
# ------------------------------------------------------------------

# 1. Circle -------------------------------------------------------------------
circle_generator <- DatasetGenerator$new(
  name = "circle",
  description = "A simple noisy circle in 2D space.",
  param_spec = list(
    num_points = list(
      type = "slider", label = "Number of points",
      min = 100, max = 2000, value = 1000, step = 100
    ),
    noise = list(
      type = "slider", label = "Noise level",
      min = 0, max = 1, value = 0.1, step = 0.01
    )
  ),
  generate_fn = function(num_points, noise) {
    data.frame(
      x = sapply(1:num_points, cos) + runif(num_points, 0, noise),
      y = sapply(1:num_points, sin) + runif(num_points, 0, noise)
    )
  }
)

# 2. Fading Circle -------------------------------------------------------------
fading_circle_generator <- DatasetGenerator$new(
  name = "fading circle",
  description = "A circle with random intensity/strength fading.",
  param_spec = list(
    num_points = list(
      type = "slider", label = "Number of points",
      min = 100, max = 2000, value = 800, step = 100
    ),
    noise = list(
      type = "slider", label = "Noise level",
      min = 0, max = 1, value = 0.15, step = 0.01
    )
  ),
  generate_fn = function(num_points, noise) {
    angles <- rnorm(num_points, mean = 0, sd = 1)
    data.frame(
      x = cos(angles) + runif(num_points, 0, noise),
      y = sin(angles) + runif(num_points, 0, noise)
    )
  }
)

# 3. Figure Eight --------------------------------------------------------------
figure8_generator <- DatasetGenerator$new(
  name = "figure 8",
  description = "A standard 2D figure-eight curve with noise.",
  param_spec = list(
    num_points = list(
      type = "slider", label = "Number of points",
      min = 100, max = 2000, value = 1200, step = 100
    ),
    noise = list(
      type = "slider", label = "Noise level",
      min = 0, max = 1, value = 0.05, step = 0.01
    )
  ),
  generate_fn = function(num_points, noise) {
    data.frame(
      x = sapply(1:num_points, function(x) cos(x) / (1 + sin(x)^2)) +
        runif(num_points, 0, noise),
      y = sapply(1:num_points, function(x) sin(x) * cos(x) / (1 + sin(x)^2)) +
        runif(num_points, 0, noise)
    )
  }
)

# 4. Spiral -------------------------------------------------------------------
spiral_generator <- DatasetGenerator$new(
  name = "spiral",
  description = "A spiral shape with an inner hole and random noise.",
  param_spec = list(
    n = list(
      type = "slider", label = "Number of points",
      min = 100, max = 2000, value = 1500, step = 100
    ),
    noise = list(
      type = "slider", label = "Noise level",
      min = 0, max = 1, value = 0.08, step = 0.01
    )
  ),
  generate_fn = function(n = 1000, noise = 0.1) {
    t <- runif(n, 0, 4 * pi)
    h <- runif(n, 0, 1)

    hole_mask <- (t > 1.5 * pi & t < 1.75 * pi) & (h > 0.25 & h < 0.75)
    t <- t[!hole_mask]
    h <- h[!hole_mask]

    x <- t * cos(t) + rnorm(length(t), 0, noise)
    z <- t * sin(t) + rnorm(length(t), 0, noise)
    data.frame(x = x, y = z)
  }
)

# 5. Barbell ------------------------------------------------------------------
barbell_generator <- DatasetGenerator$new(
  name = "barbell",
  description = "Two noisy disks connected by a bar segment.",
  param_spec = list(
    n = list(
      type = "slider", label = "Number of points",
      min = 100, max = 2000, value = 1000, step = 100
    ),
    noise = list(
      type = "slider", label = "Noise level",
      min = 0, max = 1, value = 0.1, step = 0.01
    )
  ),
  generate_fn = function(n, noise) {
    r1 <- sqrt(runif(n * .45, 0, 1))
    a1 <- runif(n * .45, 0, 2 * pi)
    disk1 <- data.frame(x = r1 * cos(a1) - 2, y = r1 * sin(a1))

    r2 <- sqrt(runif(n * .45, 1 - noise, 1))
    a2 <- runif(n * .45, 0, 2 * pi)
    disk2 <- data.frame(x = r2 * cos(a2) + 2, y = r2 * sin(a2))

    line <- data.frame(x = runif(n / 20, -1, 1), y = rep(0, n / 20))
    rbind(disk1, disk2, line)
  }
)

# ---------------------------------------------------------------
# ---- Registry -------------------------------------------------
# ---------------------------------------------------------------

dataset_registry <- list(
  circle = circle_generator,
  "fading circle" = fading_circle_generator,
  "figure 8" = figure8_generator,
  spiral = spiral_generator,
  barbell = barbell_generator
)

# fetch a generator by name
get_generator <- function(name) {
  key <- tolower(name)
  key <- gsub(" ", "", key)
  matched <- names(dataset_registry)
  clean_names <- gsub(" ", "", tolower(matched))
  if (key %in% clean_names) {
    dataset_registry[[matched[which(clean_names == key)]]]
  } else {
    dataset_registry$circle
  }
}
