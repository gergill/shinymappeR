library(mclust)
library(nortest)

# Compute Anderson-Darling statistic
ad_test_stat <- function(x) {
  n <- length(x)
  if (n < 8) return(0)
  res <- ad.test(x)
  return(res$statistic * (1 + (4/n) - (5/n)^2))
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
  meanx <- mean(masked_data)
  stdx <- sd(masked_data)

  m <- sqrt(2 / pi) * stdx
  c1 <- meanx + m
  c2 <- meanx - m

  gmm <- tryCatch(
    Mclust(masked_data, G = 2, initialization = list(subset = c1, hcPairs = c2)),
    error = function(e) NULL
  )
  if (is.null(gmm)) return(NULL)

  means <- sort(gmm$parameters$mean)
  sds <- sqrt(gmm$parameters$variance$sigmasq[order(gmm$parameters$mean)])

  left_mean <- means[1]; right_mean <- means[2]
  left_std <- sds[1]; right_std <- sds[2]

  new_upper <- left_mean + (1 + g_overlap) * left_std / (left_std + right_std) * (right_mean - left_mean)
  new_lower <- right_mean - (1 + g_overlap) * right_std / (left_std + right_std) * (right_mean - left_mean)

  if (new_upper >= interval$upper || new_lower <= interval$lower) return(NULL)

  list(
    make_interval(data, interval$lower, new_upper, interval$members),
    make_interval(data, new_lower, interval$upper, interval$members)
  )
}

bfs_gmapper <- function(lens, iterations, max_intervals, ad_threshold, g_overlap) {
  cover <- list(make_interval(lens, min(lens), max(lens)))
  iter <- 0

  while (TRUE) {
    if (length(cover) >= max_intervals) break
    if (iter > iterations) break

    # pick interval with largest (most negative ad_score)
    idx <- which.min(sapply(cover, \(x) x$ad_score))
    worst <- cover[[idx]]

    if (-worst$ad_score < ad_threshold) break

    iter <- iter + 1
    new_intervals <- split_interval(worst, lens, g_overlap)
    if (is.null(new_intervals)) {
      # can't split
      cover[[idx]]$ad_score <- Inf
      next
    }

    # replace with new intervals
    cover <- append(cover[-idx], new_intervals)
  }

  return(cover)
}

create_gmapper_cover <- function(
  lens, iterations = 20, max_intervals = 10, ad_threshold = 0.5, g_overlap = 0.3
) {
  cov <- bfs_gmapper(lens, iterations, max_intervals, ad_threshold, g_overlap)
  do.call(rbind, lapply(cov, \(iv) c(iv$lower, iv$upper)))
}
