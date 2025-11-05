source("cover_utils.R")

create_width_balanced_cover <- function(min_val, max_val,
                                        num_patches = 10,
                                        percent_overlap = 25,
                                        use_union_format = TRUE) {
  stopifnot(
    is.numeric(min_val),
    is.numeric(max_val),
    min_val < max_val,
    num_patches >= 1,
    percent_overlap >= 0 && percent_overlap < 100
  )

  total_range <- max_val - min_val
  base_width <- total_range / num_patches

  overlap_frac <- percent_overlap / 100

  overlap_half <- overlap_frac * base_width / 2

  elements <- lapply(seq_len(num_patches), function(i) {
    left_base <- min_val + (i - 1) * base_width
    right_base <- min_val + i * base_width

    lower <- left_base - overlap_half
    upper <- right_base + overlap_half

    # restrict to [min_val, max_val]
    lower <- max(lower, min_val)
    upper <- min(upper, max_val)

    matrix(c(lower, upper), nrow = 1, ncol = 2)
  })

  if (use_union_format) {
    return(create_cover(elements))
  } else {
    # Backward compatibility: return as matrix
    intervals <- do.call(rbind, elements)
    colnames(intervals) <- c("lower", "upper")
    return(as.data.frame(intervals))
  }
}
