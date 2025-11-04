library(mclust)

gaussian_pdf_cover <- function(
    lens, n_components = 3, pdf_cutoff = 0.01, 
    min_interval_size = 8) {
  
  # fit GMM
  gmm <- tryCatch(
    Mclust(lens, G = n_components),
    error = function(e) {
      cat("GMM fitting failed, using single Gaussian\n")
      Mclust(lens, G = 1)
    }
  )
  
  if (is.null(gmm)) {
    # Fallback: return single interval
    return(matrix(c(min(lens), max(lens)), nrow = 1))
  }
  
  means <- as.numeric(gmm$parameters$mean)
  sigs_raw <- gmm$parameters$variance$sigmasq
  sigs <- as.numeric(sigs_raw)
  if (length(sigs) == 1) sigs <- rep(sigs, length(means))
  
  mixing_probs <- gmm$parameters$pro
  if (is.null(mixing_probs)) {
    mixing_probs <- rep(1 / length(means), length(means))
  }
  
  # Sort components by mean
  order_idx <- order(means)
  means <- means[order_idx]
  sds <- sqrt(sigs)[order_idx]
  mixing_probs <- mixing_probs[order_idx]
  
  cat("Fitted", length(means), "Gaussians\n")
  cat("Means:", means, "\n")
  cat("SDs:", sds, "\n")
  cat("Mixing probabilities:", mixing_probs, "\n")
  
  # intervals for each Gaussian component
  intervals <- list()
  
  for (i in seq_along(means)) {
    mean_i <- means[i]
    sd_i <- sds[i]
    mix_i <- mixing_probs[i]
    
    # Peak density for this component
    peak_density <- mix_i * dnorm(mean_i, mean_i, sd_i)
    
    # Threshold density
    threshold_density <- peak_density * pdf_cutoff
    
    # Find where density equals threshold
    # For a Gaussian: density(x) = mix * (1/sqrt(2*pi*sd^2)) * exp(-0.5*((x-mean)/sd)^2)
    # We want: mix * (1/sqrt(2*pi*sd^2)) * exp(-0.5*((x-mean)/sd)^2) = threshold
    # Solving: exp(-0.5*((x-mean)/sd)^2) = threshold / (mix/(sqrt(2*pi*sd^2)))
    # -0.5*((x-mean)/sd)^2 = log(threshold * sqrt(2*pi*sd^2) / mix)
    
    if (threshold_density <= 0 || threshold_density >= peak_density) {
      # Invalid threshold, skip this component
      next
    }
    
    # Calculate cutoff distance from mean
    ratio <- threshold_density / (mix_i / (sqrt(2 * pi * sd_i^2)))
    if (ratio <= 0 || ratio >= 1) {
      next
    }
    
    z_cutoff <- sqrt(-2 * log(ratio))
    
    lower <- mean_i - z_cutoff * sd_i
    upper <- mean_i + z_cutoff * sd_i
    
    # Count how many points fall in this interval
    n_points <- sum(lens >= lower & lens <= upper)
    
    if (n_points >= min_interval_size) {
      intervals[[length(intervals) + 1]] <- c(lower, upper)
      cat("Component", i, "interval: [", lower, ",", upper, 
          "] with", n_points, "points\n")
    } else {
      cat("Component", i, "has only", n_points, 
          "points, skipping (min required:", min_interval_size, ")\n")
    }
  }
  
  # If no valid intervals, return full range
  if (length(intervals) == 0) {
    cat("No valid intervals found, returning full range\n")
    return(matrix(c(min(lens), max(lens)), nrow = 1))
  }
  
  # create cover matrix
  cover_matrix <- do.call(rbind, intervals)
  
  # sort by lower bound
  cover_matrix <- cover_matrix[order(cover_matrix[, 1]), , drop = FALSE]
  
  return(cover_matrix)
}

create_gaussian_pdf_cover <- function(
    lens, n_components = 3, pdf_cutoff = 0.01,
    min_interval_size = 8) {
  gaussian_pdf_cover(lens, n_components, pdf_cutoff, min_interval_size)
}
