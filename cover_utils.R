#' Create a cover element from one or more intervals
#'
#' @param intervals A matrix with 2 columns (lower, upper) or a single numeric vector c(lower, upper)
#' @return A standardized cover element (2-column matrix)
create_cover_element <- function(intervals) {
  if (is.vector(intervals) && length(intervals) == 2) {
    intervals <- matrix(intervals, nrow = 1, ncol = 2)
  }

  if (!is.matrix(intervals) || ncol(intervals) != 2) {
    stop("Intervals must be a 2-column matrix or a vector of length 2")
  }

  colnames(intervals) <- c("lower", "upper")
  intervals
}

#' Create a cover from a list of cover elements
#'
#' @param elements A list where each element is a cover element (matrix of intervals)
#' @return A list-based cover structure
create_cover <- function(elements) {
  if (!is.list(elements)) {
    stop("Elements must be a list")
  }

  cover <- lapply(elements, create_cover_element)
  class(cover) <- c("union_cover", "list")
  cover
}

#' Check if a cover is in the new union format
#'
#' @param cover A cover object
#' @return TRUE if it's a union cover, FALSE otherwise
is_union_cover <- function(cover) {
  inherits(cover, "union_cover")
}

#' Convert old-school cover (matrix) to new union cover
#'
#' @param old_cover A matrix with 2 columns (lower, upper)
#' @return A union cover where each element has one interval
convert_to_union_cover <- function(old_cover) {
  if (is_union_cover(old_cover)) {
    return(old_cover)
  }

  if (is.matrix(old_cover) || is.data.frame(old_cover)) {
    elements <- lapply(seq_len(nrow(old_cover)), function(i) {
      matrix(c(old_cover[i, 1], old_cover[i, 2]), nrow = 1, ncol = 2)
    })
    return(create_cover(elements))
  }

  stop("Cannot convert cover to union format")
}

#' Convert union cover back to matrix format (for backward compatibility)
#'
#' @param union_cover A union cover object
#' @param merge_mode Either "bounds" (merge to bounding interval) or "first" (use first interval)
#' @return A matrix with 2 columns (lower, upper), one row per cover element
convert_union_to_matrix <- function(union_cover, merge_mode = "bounds") {
  if (!is_union_cover(union_cover)) {
    if (is.matrix(union_cover) || is.data.frame(union_cover)) {
      return(as.matrix(union_cover))
    }
    stop("Input must be a union cover or matrix")
  }

  intervals <- lapply(union_cover, function(element) {
    if (nrow(element) == 1) {
      # Single interval - return as is
      c(element[1, 1], element[1, 2])
    } else {
      # Multiple intervals - merge to bounding interval
      if (merge_mode == "bounds") {
        # Take min of all lowers and max of all uppers
        c(min(element[, 1]), max(element[, 2]))
      } else if (merge_mode == "first") {
        # Just take first interval
        c(element[1, 1], element[1, 2])
      } else {
        stop("merge_mode must be 'bounds' or 'first'")
      }
    }
  })

  mat <- do.call(rbind, intervals)
  colnames(mat) <- c("lower", "upper")
  mat
}

#' Create interval checker functions that work with both union covers and matrix covers
#'
#' @param cover Either a union cover object or a matrix with 2 columns
#' @return A list of functions that check if values are in each cover element
create_interval_checkers <- function(cover) {
  if (is_union_cover(cover)) {
    # Union cover - create checkers that handle multiple intervals per element
    lapply(cover, function(element) {
      function(values) {
        sapply(values, function(v) {
          any(apply(element, 1, function(interval) {
            v >= interval[1] && v <= interval[2]
          }))
        })
      }
    })
  } else if (is.matrix(cover) || is.data.frame(cover)) {
    # Matrix cover
    # replicates the original check_in_interval behavior
    apply(cover, 1, function(interval_row) {
      lower <- interval_row[1]
      upper <- interval_row[2]
      function(values) {
        values >= lower & values <= upper
      }
    })
  } else {
    stop("Cover must be either a union_cover object or a matrix/data.frame")
  }
}

#' Check if a point falls within a cover element (union of intervals)
#'
#' @param value A numeric value to test
#' @param element A cover element (matrix of intervals)
#' @return TRUE if value is in any of the intervals, FALSE otherwise
point_in_element <- function(value, element) {
  any(apply(element, 1, function(interval) {
    value >= interval[1] && value <= interval[2]
  }))
}

#' Get all points that fall within a cover element
#'
#' @param values A numeric vector of values
#' @param element A cover element
#' @return Indices of values that fall within the element
get_points_in_element <- function(values, element) {
  which(sapply(values, function(v) point_in_element(v, element)))
}

#' Print method for union covers
#'
#' @param x A union cover
#' @param ... Additional arguments
print.union_cover <- function(x, ...) {
  cat("Union Cover with", length(x), "elements:\n")
  for (i in seq_along(x)) {
    n_intervals <- nrow(x[[i]])
    if (n_intervals == 1) {
      cat(sprintf(
        "  Element %d: [%.3f, %.3f]\n",
        i, x[[i]][1, 1], x[[i]][1, 2]
      ))
    } else {
      cat(sprintf("  Element %d: Union of %d intervals:\n", i, n_intervals))
      for (j in seq_len(n_intervals)) {
        cat(sprintf("    [%.3f, %.3f]\n", x[[i]][j, 1], x[[i]][j, 2]))
      }
    }
  }
}
