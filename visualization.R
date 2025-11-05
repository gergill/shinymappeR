library(viridis)
library(RColorBrewer)
source("cover_utils.R")

# --- Filtered Data Plot ------------------------------------
plot_filtered_data <- function(df, vals) {
  col <- colorRampPalette(brewer.pal(9, "YlOrRd"))(50)
  col_assign <- col[as.numeric(cut(vals, breaks = 50))]
  plot(df, col = col_assign, pch = 20, asp = 1, axes = FALSE)
}

# --- Mapper Graph Visualization ----------------------------
plot_mapper_graph <- function(mapper_obj) {
  plot(mapper_to_igraph(mapper_obj))
}

# --- Mapper Cover Splits Line Plot -------------------------
plot_mapper_cover_splits <- function(cov, lens_values = NULL, bins = 30) {
  # Convert to union format if needed
  if (!is_union_cover(cov)) {
    cov <- convert_to_union_cover(cov)
  }
  
  if (length(cov) == 0) {
    plot(1, 1,
      type = "n", xlab = "Lens Values", ylab = "Elements",
      main = "No cover elements to display"
    )
    return()
  }

  n_elements <- length(cov)
  cov_colors <- viridis::viridis(n_elements, alpha = 0.7, option = "D")

  # Calculate x-range
  if (!is.null(lens_values)) {
    lens_values <- na.omit(lens_values)
    h <- hist(lens_values, breaks = bins, plot = FALSE)
    x_range <- c(min(h$breaks), max(h$breaks))
  } else {
    all_bounds <- do.call(rbind, cov)
    x_range <- range(all_bounds)
  }

  x_padding <- diff(x_range) * 0.05
  plot(x_range + c(-x_padding, x_padding), c(0, n_elements + 1),
    type = "n", xlab = "Lens Values", ylab = "Element Index",
    main = "Mapper Cover Elements", axes = TRUE
  )

  abline(h = 1:n_elements, col = "lightgray", lty = 3, lwd = 0.5)

  rect_height <- 0.6
  for (i in seq_len(n_elements)) {
    element <- cov[[i]]
    n_intervals <- nrow(element)
    
    for (j in seq_len(n_intervals)) {
      y_bottom <- i - rect_height / 2
      y_top <- i + rect_height / 2
      
      # Different style for union elements
      border_lwd <- if (n_intervals > 1) 2 else 1
      border_lty <- if (n_intervals > 1) 1 else 1

      rect(element[j, 1], y_bottom, element[j, 2], y_top,
        col = cov_colors[i],
        border = "black",
        lwd = border_lwd,
        lty = border_lty
      )
    }

    # Label
    mid_point <- mean(element[, 1:2])
    label_text <- if (n_intervals > 1) {
      paste0("E", i, " (", n_intervals, ")")
    } else {
      paste0("E", i)
    }
    text(mid_point, i,
      labels = label_text,
      cex = 0.8, col = "white", font = 2
    )
  }

  # Range info
  if (!is.null(lens_values)) {
    mtext(
      paste(
        "Lens range: [", round(min(lens_values), 3), ",",
        round(max(lens_values), 3), "]"
      ),
      side = 1, line = 3, cex = 0.8, col = "darkgray"
    )
    all_bounds <- do.call(rbind, cov)
    mtext(
      paste(
        "Cover range: [", round(min(all_bounds), 3), ",",
        round(max(all_bounds), 3), "]"
      ),
      side = 1, line = 4, cex = 0.8, col = "darkblue"
    )
  }
}

# --- Staggered Data + Cover Visualization ------------------
plot_staggered_data <- function(df, cov, lens_obj, input, filtered_vals) {
  # Convert to union format if needed
  if (!is_union_cover(cov)) {
    cov <- convert_to_union_cover(cov)
  }
  
  plot(
    df,
    asp = 1, pch = 20, col = "grey",
    xlab = "", ylab = "",
    xlim = range(df$x), ylim = range(df$y)
  )

  if (lens_obj$projection) {
    n_elements <- length(cov)
    cov_colors <- viridis::viridis(n_elements, alpha = 0.35, option = "D")

    if (input$lens == "project to x") {
      for (i in seq_len(n_elements)) {
        element <- cov[[i]]
        for (j in seq_len(nrow(element))) {
          lty_style <- if (nrow(element) > 1) 1 else (if (i %% 2 == 0) 2 else 1)
          lwd_style <- if (nrow(element) > 1) 2 else 1
          rect(
            element[j, 1], min(df$y), element[j, 2], max(df$y),
            col = cov_colors[i],
            border = "black",
            lty = lty_style,
            lwd = lwd_style
          )
        }
      }
    } else if (input$lens == "project to y") {
      for (i in seq_len(n_elements)) {
        element <- cov[[i]]
        for (j in seq_len(nrow(element))) {
          lty_style <- if (nrow(element) > 1) 1 else (if (i %% 2 == 0) 2 else 1)
          lwd_style <- if (nrow(element) > 1) 2 else 1
          rect(
            min(df$x), element[j, 1], max(df$x), element[j, 2],
            col = cov_colors[i],
            border = "black",
            lty = lty_style,
            lwd = lwd_style
          )
        }
      }
    } else if (input$lens == "theta lens") {
      pinfo <- lens_obj$projection_fn(df, input$theta)
      pvec <- pinfo$vector / sqrt(sum(pinfo$vector^2))
      perp <- c(-pvec[2], pvec[1]) / sqrt(sum(pvec^2))

      for (i in seq_len(n_elements)) {
        element <- cov[[i]]
        for (j in seq_len(nrow(element))) {
          lty_style <- if (nrow(element) > 1) 1 else (if (i %% 2 == 0) 2 else 1)
          lwd_style <- if (nrow(element) > 1) 2 else 1
          
          cut1 <- pinfo$point + element[j, 1] * pvec
          cut2 <- pinfo$point + element[j, 2] * pvec
          rect_coords <- rbind(
            cut1 + 100 * perp,
            cut1 - 100 * perp,
            cut2 - 100 * perp,
            cut2 + 100 * perp
          )
          polygon(
            rect_coords[, 1], rect_coords[, 2],
            col = cov_colors[i],
            border = "black",
            lty = lty_style,
            lwd = lwd_style
          )
        }
      }
    } else if (grepl("PCA", input$lens)) {
      pinfo <- lens_obj$projection_fn(df)
      pvec <- pinfo$vector / sqrt(sum(pinfo$vector^2))
      slope <- pvec[2] / pvec[1]
      abline(a = 0, b = slope, col = "darkgreen", lwd = 3, lty = 3)
      perp <- c(-pvec[2], pvec[1]) / sqrt(sum(pvec^2))

      for (i in seq_len(n_elements)) {
        element <- cov[[i]]
        for (j in seq_len(nrow(element))) {
          lty_style <- if (nrow(element) > 1) 1 else (if (i %% 2 == 0) 2 else 1)
          lwd_style <- if (nrow(element) > 1) 2 else 1
          
          cut1 <- element[j, 1] * pvec
          cut2 <- element[j, 2] * pvec
          rect_coords <- rbind(
            cut1 + 100 * perp,
            cut1 - 100 * perp,
            cut2 - 100 * perp,
            cut2 + 100 * perp
          )
          polygon(
            rect_coords[, 1], rect_coords[, 2],
            col = cov_colors[i],
            border = "black",
            lty = lty_style,
            lwd = lwd_style
          )
        }
      }
    }
  } else {
    val_colors <- colorRampPalette(brewer.pal(9, "YlOrRd"))(50)
    col_assign <- val_colors[as.numeric(cut(filtered_vals, breaks = 50))]
    points(df, col = col_assign, pch = 20)
  }
}

# --- Patch View --------------------------------------------
plot_patch_view <- function(df, mapper_obj, display_patch, method, clusterer_mode) {
  vertices <- mapper_obj[[1]]
  global_dists <- dist(df)
  this_patch <- vertices[vertices$patch == display_patch, ]
  rows <- as.numeric(unlist(strsplit(this_patch$data, ",")))
  datasub <- df[rows, ]

  patch_dists <- dist(datasub)
  patch_dend <- hclust(patch_dists, method)
  global_dend <- hclust(global_dists, method)
  global_cut <- get_longevity_cut_height(global_dend, max(global_dists))
  patch_cut <- if (clusterer_mode == "local") {
    get_longevity_cut_height(patch_dend, max(patch_dists))
  } else {
    global_cut
  }

  par(mfrow = c(1, 2))
  clusters <- cutree(patch_dend, h = patch_cut)
  cols <- brewer.pal(length(unique(clusters)), "Dark2")
  plot(datasub, pch = 20, col = cols[clusters], asp = 1, axes = FALSE)
  plot_dendrogram(
    patch_dend, method,
    if (clusterer_mode == "local") max(patch_dists) else max(global_dists),
    patch_cut, paste("Patch", display_patch),
    paste("Linkage:", method)
  )
}

# --- Global View -------------------------------------------
plot_global_view <- function(df, method) {
  global_dists <- dist(df)
  global_dend <- hclust(global_dists, method)
  global_cut <- get_longevity_cut_height(global_dend, max(global_dists))

  par(mfrow = c(1, 2))
  clusters <- cutree(global_dend, h = global_cut)
  cols <- brewer.pal(length(unique(clusters)), "Dark2")
  plot(df, pch = 20, col = cols[clusters], asp = 1, axes = FALSE)
  plot_dendrogram(
    global_dend, method,
    max(global_dists), global_cut,
    "All Data", paste("Linkage:", method)
  )
}
