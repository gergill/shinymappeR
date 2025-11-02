library(R6)

# ---- DatasetGenerator class definition -------------------------------------

DatasetGenerator <- R6::R6Class(
  "DatasetGenerator",
  public = list(
    name = NULL,
    description = NULL,
    param_spec = NULL, # list of parameter metadata
    generate_fn = NULL, # function that produces a data.frame(x, y)

    initialize = function(name, description, param_spec, generate_fn) {
      self$name <- name
      self$description <- description
      self$param_spec <- param_spec
      self$generate_fn <- generate_fn
    },

    # Data generation method
    generate = function(params = list()) {
      args <- lapply(names(self$param_spec), function(p) {
        if (!is.null(params[[p]])) {
          params[[p]]
        } else {
          self$param_spec[[p]]$value
        }
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

# 6. Multi-Rotating Gaussians --------------------------------------------------
multi_gaussians_generator <- DatasetGenerator$new(
  name = "multi rotating Gaussians",
  description = paste(
    "Places an arbitrary number of Gaussian clusters evenly around a circle,",
    "each optionally with its own standard deviation. The entire configuration",
    "rotates as angle changes."
  ),
  param_spec = list(
    num_points = list(
      type = "slider", label = "Total number of points",
      min = 100, max = 5000, value = 1000, step = 100
    ),
    num_gaussians = list(
      type = "slider", label = "Number of Gaussian clusters",
      min = 2, max = 15, value = 2, step = 1
    ),
    radius = list(
      type = "slider", label = "Radius of circle",
      min = 0.1, max = 5, value = 2, step = 0.1
    ),
    angle = list(
      type = "slider", label = "Global rotation (radians)",
      min = 0, max = 2 * pi, value = 0, step = 0.1
    ),
    sd_base = list(
      type = "slider", label = "Base standard deviation",
      min = 0.01, max = 1, value = 0.1, step = 0.01
    ),
    sd_variation = list(
      type = "slider", label = "Std. deviation variation across clusters",
      min = 0, max = 1, value = 0, step = 0.05
    )
  ),
  generate_fn = function(num_points,
                         num_gaussians,
                         radius,
                         angle,
                         sd_base,
                         sd_variation) {
    set.seed(137)

    # Points per component distributed as evenly
    points_per <- rep(floor(num_points / num_gaussians), num_gaussians)
    remainder <- num_points - sum(points_per)
    if (remainder > 0) {
      points_per[seq_len(remainder)] <- points_per[seq_len(remainder)] + 1
    }

    # evenly spaced cluster means about circle
    base_angles <- seq(0, 2 * pi, length.out = num_gaussians + 1)[-1] + angle

    # each cluster may have slightly different SDs if sd_variation > 0
    if (sd_variation == 0) {
      sds <- rep(sd_base, num_gaussians)
    } else {
      sds <- sd_base * (1 + sd_variation *
        sin(seq(0, 2 * pi, length.out = num_gaussians)))
    }

    # Generate clusters
    cluster_list <- lapply(seq_len(num_gaussians), function(i) {
      n_i <- points_per[i]
      mean_i <- c(radius * cos(base_angles[i]), radius * sin(base_angles[i]))
      data.frame(
        x = rnorm(n_i, mean = mean_i[1], sd = sds[i]),
        y = rnorm(n_i, mean = mean_i[2], sd = sds[i])
      )
    })

    # numeric-only output
    df <- do.call(rbind, cluster_list)
    df <- df[sapply(df, is.numeric)]
    df
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
  barbell = barbell_generator,
  "multi rotating Gaussians" = multi_gaussians_generator
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
