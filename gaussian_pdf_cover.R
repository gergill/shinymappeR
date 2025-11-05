library(mclust)
source("cover_utils.R")

#' Create Gaussian PDF Cover for Mapper/TDA
#'
#' Fits a Gaussian Mixture Model to lens values and creates a cover based on
#' regions where each component's responsibility exceeds a threshold.
#'
#' @param lens Numeric vector of lens values
#' @param n_components Number of Gaussian components to fit
#' @param z_threshold Responsibility threshold (default 0.3). Points where a
#'   component's responsibility exceeds this value are included in that
#'   component's cover element
#' @param min_interval_size Minimum number of points required for an interval
#'   to be included. The default is 1 since we assume no noise and not too sparse.
#'   
#' @return A cover object containing union of all valid
#'   intervals
#'
#' @details
#' The function identifies contiguous regions using run-length encoding:
#' 
#' Example: For a component with sorted lens values and responsibilities:
#'   Lens values:    [0.1, 0.2, 0.5, 0.6, 0.7, 1.2, 1.5, 2.0]
#'   Responsibility: [0.1, 0.2, 0.8, 0.9, 0.7, 0.2, 0.6, 0.1]
#'   Above 0.3?:     [ F ,  F ,  T ,  T ,  T ,  F ,  T ,  F ]
#'                    └─────┘  └─────────┘  └───┘ └─────┘
#'                              ↑ Region 1 ↑  ↑Region 2
#' 
#' This identifies 2 regions:
#'   - Region 1: [0.5, 0.7] (3 points)
#'   - Region 2: [1.5, 1.5] (1 point)
#' 
#' If min_interval_size = 2, only Region 1 survives as a cover element.
create_gaussian_pdf_cover <- function(
    lens, 
    n_components = 3, 
    z_threshold = 0.3, 
    min_interval_size = 1) {
  
  # Fit GMM with specified number of components
  gmm <- tryCatch(
    Mclust(lens, G = n_components),
    error = function(e) {
      cat("GMM fitting failed, using single Gaussian\n")
      Mclust(lens, G = 1)
    }
  )
  
  # Fallback to full range if GMM fails completely
  if (is.null(gmm) || is.null(gmm$z)) {
    return(create_cover(list(matrix(c(min(lens), max(lens)), nrow = 1, ncol = 2))))
  }
  
  # Sort lens values and align responsibilities
  # This ensures we can identify contiguous regions along the lens axis
  ord <- order(lens)
  sorted_lens <- lens[ord]
  sorted_z <- gmm$z[ord, ]  # n_points × n_components matrix
  
  means <- as.numeric(gmm$parameters$mean)
  
  cat("Fitted", ncol(sorted_z), "Gaussians with z-threshold:", z_threshold, "\n")
  cat("Means:", means, "\n\n")
  
  cover_elements <- list()
  
  # Process each Gaussian component
  for (k in seq_len(ncol(sorted_z))) {
    z_k <- sorted_z[, k]  # Responsibility curve for component k
    above_threshold <- z_k > z_threshold  # Boolean mask
    
    if (sum(above_threshold) == 0) {
      cat("Component", k, "has no points above threshold\n")
      next
    }
    
    # Identify contiguous regions using run-length encoding
    # Example: above_threshold = [F, F, T, T, T, F, T, F]
    #   rle gives: values  = [F, T, F, T, F]
    #              lengths = [2, 3, 1, 1, 1]
    # This means: 2 FALSEs, then 3 TRUEs, then 1 FALSE, then 1 TRUE, then 1 FALSE
    rle_result <- rle(above_threshold)
    
    # Calculate start and end indices for each run
    # Continuing example:
    #   run_ends   = cumsum([2, 3, 1, 1, 1]) = [2, 5, 6, 7, 8]
    #   run_starts = [1, 3, 6, 7, 8]
    run_ends <- cumsum(rle_result$lengths)
    run_starts <- c(1, run_ends[-length(run_ends)] + 1)
    
    # Keep only TRUE runs (regions above threshold)
    # Continuing example: which(rle_result$values) = [2, 4]
    # This corresponds to:
    #   - Run 2: indices 3-5 (3 TRUEs)
    #   - Run 4: index 7 (1 TRUE)
    true_runs <- which(rle_result$values)
    
    intervals <- list()
    
    # Convert each TRUE run into an interval using actual lens values
    for (i in true_runs) {
      start_idx <- run_starts[i]
      end_idx <- run_ends[i]
      
      # Map indices to lens values to get interval boundaries
      # Example: indices 3-5 map to lens values [0.5, 0.6, 0.7]
      #          giving interval [0.5, 0.7]
      lower <- sorted_lens[start_idx]
      upper <- sorted_lens[end_idx]
      
      n_points <- end_idx - start_idx + 1
      
      # filter intervals with too few points
      if (n_points >= min_interval_size) {
        intervals[[length(intervals) + 1]] <- c(lower, upper)
      } else {
        cat("Component", k, "interval [", round(lower, 3), ",", 
            round(upper, 3), "] has only", n_points, "points, skipping\n")
      }
    }
    
    if (length(intervals) == 0) {
      cat("Component", k, "has no intervals with sufficient points\n")
      next
    }
    
    # matrix format for cover creation
    interval_matrix <- do.call(rbind, intervals)
    colnames(interval_matrix) <- c("lower", "upper")
    
    cover_elements[[length(cover_elements) + 1]] <- interval_matrix
    
    # get statistics for this component
    total_points <- sum(sapply(seq_len(nrow(interval_matrix)), function(j) {
      sum(sorted_lens >= interval_matrix[j, 1] & 
          sorted_lens <= interval_matrix[j, 2])
    }))
    
    cat(sprintf(
      "Component %d (mean=%.3f): %d interval%s covering %d points\n",
      k, means[k], nrow(interval_matrix),
      if (nrow(interval_matrix) > 1) "s" else "",
      total_points
    ))
    
    for (j in seq_len(nrow(interval_matrix))) {
      n_pts <- sum(
        sorted_lens >= interval_matrix[j, 1] & 
        sorted_lens <= interval_matrix[j, 2]
      )
      cat(sprintf(
        "  Interval %d: [%.3f, %.3f] (%d points)\n",
        j, interval_matrix[j, 1], interval_matrix[j, 2], n_pts
      ))
    }
  }
  
  # Fallback
  if (length(cover_elements) == 0) {
    cat("No valid elements found, returning full range\n")
    return(create_cover(list(
      matrix(c(min(lens), max(lens)), nrow = 1, ncol = 2)
    )))
  }
  
  cat("\nCreated union cover with", length(cover_elements), "elements\n")
  return(create_cover(cover_elements))
}
