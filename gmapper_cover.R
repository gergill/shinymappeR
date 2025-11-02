library(mclust)
library(nortest)

# Compute Anderson-Darling statistic
ad_test_stat <- function(x) {
  n <- length(x)
  if (n < 8) {
    return(0)
  }
  res <- ad.test(x)
  return(res$statistic * (1 + (4 / n) - (5 / n)^2))
}

make_interval <- function(data, lower, upper, mask = NULL) {
  if (is.null(mask)) mask <- seq_along(data)
  masked <- data[mask]
  new_mask <- which(masked >= lower & masked <= upper)
  new_members <- mask[new_mask]
  return(list(
    ad_score = -ad_test_stat(data[new_members]),
    members = new_members,
    lower = lower,
    upper = upper
  ))
}

split_interval <- function(interval, data, g_overlap) {
  masked_data <- data[interval$members]
  if (length(masked_data) < 8) {
    return(NULL)
  }

  gmm <- tryCatch(Mclust(masked_data, G = 2), error = function(e) NULL)
  if (is.null(gmm)) {
    return(NULL)
  }

  means <- as.numeric(gmm$parameters$mean)
  sigs_raw <- gmm$parameters$variance$sigmasq
  sigs <- as.numeric(sigs_raw)
  if (length(sigs) == 1) sigs <- rep(sigs, length(means))

  order_idx <- order(means)
  means <- means[order_idx]
  sds <- sqrt(sigs)[order_idx]

  cat("Means:", means, "SDs:", sds, "\n")

  if (any(is.na(sds)) || any(is.na(means))) {
    cat("NA detected in GMM parameters, aborting split\n")
    return(NULL)
  }

  m1 <- means[1]
  m2 <- means[2]
  sigma1 <- sds[1]
  sigma2 <- sds[2]

  # min() constraint to ensure left_upper <= m2
  left_upper <- min(
    m1 + (1 + g_overlap) * sigma1 / (sigma1 + sigma2) * (m2 - m1),
    m2
  )

  # max() constraint to ensure right_lower >= m1
  right_lower <- max(
    m2 - (1 + g_overlap) * sigma2 / (sigma1 + sigma2) * (m2 - m1),
    m1
  )

  cat("Left upper:", left_upper, "Right lower:", right_lower, "\n")

  # check if split is valid
  if (left_upper > (interval$upper - 1e-6) ||
    right_lower < (interval$lower + 1e-6)) {
    cat("Rejected due to bounds check\n")
    return(NULL)
  }

  list(
    make_interval(data, interval$lower, left_upper, interval$members),
    make_interval(data, right_lower, interval$upper, interval$members)
  )
}

bfs_gmapper <- function(
    lens, iterations, max_intervals, ad_threshold, g_overlap) {
  # Initialize cover as one interval
  data_min <- min(lens)
  data_max <- max(lens)
  cover <- list(make_interval(lens, data_min, data_max))
  iter <- 0

  while (iter < iterations) {
    if (length(cover) >= max_intervals) break

    iter <- iter + 1
    new_cover <- list()
    splits_occurred <- FALSE

    # iterate through each interval
    for (i in seq_along(cover)) {
      interval <- cover[[i]]

      # if interval passes AD test (looks normal)
      if (-interval$ad_score < ad_threshold) {
        # looks normal, keep as-is
        new_cover <- append(new_cover, list(interval))
      } else {
        # not normal, try to split
        new_intervals <- split_interval(interval, lens, g_overlap)

        if (is.null(new_intervals)) {
          # can't split, keep original
          new_cover <- append(new_cover, list(interval))
        } else {
          # splitted
          new_cover <- append(new_cover, new_intervals)
          splits_occurred <- TRUE
        }
      }

      if (length(new_cover) >= max_intervals) break
    }

    # update cover for next iteration
    cover <- new_cover

    # no splits, done!
    if (!splits_occurred) break
  }

  # ensure leftmost interval extends to data_min
  lower_bounds <- sapply(cover, function(iv) iv$lower)
  leftmost_idx <- which.min(lower_bounds)
  if (cover[[leftmost_idx]]$lower != data_min) {
    cover[[leftmost_idx]]$lower <- data_min
  }

  # ensure rightmost interval extends to data_max
  upper_bounds <- sapply(cover, function(iv) iv$upper)
  rightmost_idx <- which.max(upper_bounds)
  if (cover[[rightmost_idx]]$upper != data_max) {
    cover[[rightmost_idx]]$upper <- data_max
  }

  return(cover)
}

create_gmapper_cover <- function(
    lens, iterations = 20, max_intervals = 10,
    ad_threshold = 0.5, g_overlap = 0.3) {
  cov <- bfs_gmapper(lens, iterations, max_intervals, ad_threshold, g_overlap)
  do.call(rbind, lapply(cov, \(iv) c(iv$lower, iv$upper)))
}
