source("cover_utils.R")

#' One-Dimensional Mapper with Union Cover Support
#'
#' Run Mapper using a one-dimensional filter, a cover (which can include unions), and a clusterer.
#'
#' @param data A data frame.
#' @param dists A distance matrix associated to the data frame. Can be a `dist` object or `matrix`.
#' @param filtered_data The result of a function applied to the data frame; there should be one filter value per observation.
#' These values need to be named, and the names must match the original data set.
#' @param cover Either a union_cover object or an n x 2 matrix of interval endpoints.
#' @param clusterer A function which accepts a list of distance matrices as input, and returns clustering results.
#'
#' @return A list of two data frames, `nodes` and `edges`, containing the Mapper graph.
create_1D_mapper_object_union <- function(data,
                                          dists,
                                          filtered_data,
                                          cover,
                                          clusterer = global_hierarchical_clusterer("single", dists)) {
  # Create interval checker functions that work with both union and matrix covers
  cover_functions <- create_interval_checkers(cover)

  # Validate that all intervals are properly ordered
  if (is_union_cover(cover)) {
    for (i in seq_along(cover)) {
      element <- cover[[i]]
      if (any(element[, 1] > element[, 2])) {
        stop("Left endpoints in the cover must be less than or equal to right endpoints.")
      }
    }
  } else {
    if (!all(cover[, 1] <= cover[, 2])) {
      stop("Left endpoints in the cover must be less than or equal to right endpoints.")
    }
  }

  # Call the underlying mapper function with our cover functions
  return(create_mapper_object(data, dists, filtered_data, cover_functions, clusterer = clusterer))
}
