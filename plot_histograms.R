library(ggplot2)

# Utility: fit and return mean & sd
.fit_normal <- function(values) {
  if (length(values) < 2 || all(is.na(values))) {
    return(list(mu = NA_real_, sigma = NA_real_))
  }
  mu <- mean(values, na.rm = TRUE)
  sigma <- pmax(sd(values, na.rm = TRUE), 1e-6)
  list(mu = mu, sigma = sigma)
}

# -------------------------------------------------------------------
# GLOBAL histogram of lens values + single normal fit
# -------------------------------------------------------------------
plot_global_histogram <- function(lens_values, G = NULL, bins = 30, emphasize = TRUE) {
  library(ggplot2)
  library(mclust)
  library(dplyr)
  library(patchwork)

  lens_values <- na.omit(lens_values)
  if (length(lens_values) < 2) {
    return(ggplot() + ggtitle("Not enough data for GMM fit."))
  }

  # Fit GMM (auto-select if G = NULL)
  gmm_fit <- Mclust(lens_values, G = G)
  
  # Extract parameters more robustly
  means <- gmm_fit$parameters$mean
  probs <- gmm_fit$parameters$pro
  n_comp <- gmm_fit$G
  
  # Handle variance extraction based on model type
  if (n_comp == 1) {
    sds <- sqrt(gmm_fit$parameters$variance$sigmasq)
  } else {
    # For multicomponent models, variance structure may differ
    if (is.matrix(gmm_fit$parameters$variance$sigmasq)) {
      sds <- sqrt(diag(gmm_fit$parameters$variance$sigmasq))
    } else if (is.vector(gmm_fit$parameters$variance$sigmasq)) {
      sds <- sqrt(gmm_fit$parameters$variance$sigmasq)
    } else {
      # Fallback: extract from model predictions
      sds <- rep(sqrt(gmm_fit$parameters$variance$sigmasq), n_comp)
    }
  }
  
  # Ensure we have the right number of parameters
  if (length(sds) == 1 && n_comp > 1) {
    sds <- rep(sds, n_comp)
  }

  # Histogram data (for density calculation)
  h <- hist(lens_values, breaks = bins, plot = FALSE)
  xmax <- max(h$breaks)
  xmin <- min(h$breaks)
  x_seq <- seq(xmin, xmax, length.out = 400)

  # Each GMM component density
  component_df <- do.call(rbind, lapply(seq_len(n_comp), function(k) {
    data.frame(
      lens = x_seq,
      density = probs[k] * dnorm(x_seq, mean = means[k], sd = sds[k]),
      component = paste0("Component ", k)
    )
  }))

  # Sum to get full mixture
  mixture_density <- component_df %>%
    group_by(lens) %>%
    summarise(density = sum(density), .groups = "drop")

  # Enhanced component visibility for bottom plot
  component_df_display <- component_df
  if (emphasize && n_comp > 1) {
    # Scale components to be more visible in the bottom plot
    mixture_max <- max(mixture_density$density)
    component_df_display <- component_df %>%
      group_by(component) %>%
      mutate(
        # Make sure components are at least 5% of mixture height
        density = pmax(density, 0.05 * mixture_max)
      )
  }

  # --- Top: histogram with mixture overlay ---
  p_hist <- ggplot(data.frame(lens = lens_values), aes(x = lens)) +
    geom_histogram(
      aes(y = after_stat(density)),
      bins = bins,
      fill = "#377eb8", color = "black", alpha = 0.6
    ) +
    geom_line(
      data = mixture_density,
      aes(x = lens, y = density),
      color = "#e41a1c", linewidth = 1.5
    ) +
    theme_minimal(base_size = 14) +
    labs(
      title = sprintf(
        "Global Lens Histogram with GMM Fit (%d components)", n_comp
      ),
      x = "Lens Value", y = "Density"
    ) +
    # Add legend for the mixture line
    annotate("text", 
             x = Inf, y = Inf, 
             label = "— Mixture Model",
             hjust = 1, vjust = 1, size = 3.5, color = "#e41a1c")

  # --- Bottom: GMM components only ---
  p_gmm <- ggplot() +
    #geom_line(
    #  data = mixture_density,
    #  aes(x = lens, y = density),
    #  color = "#e41a1c", linewidth = 1.2, alpha = 0.1
    #) +
    geom_line(
      data = component_df_display,
      aes(x = lens, y = density, color = component),
      linewidth = 1.0, linetype = "dashed"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom") +
    labs(
      title = "Individual Gaussian Components",
      x = "Lens Value", y = "Density",
      color = "Component"
    ) +
    # Add parameter annotations
    annotate("text", 
             x = Inf, y = Inf, 
             label = paste0("μ: ", paste(round(means, 3), collapse = ", "), "\n",
                           "σ: ", paste(round(sds, 3), collapse = ", "), "\n",
                           "w: ", paste(round(probs, 3), collapse = ", ")),
             hjust = 1, vjust = 1, size = 3, alpha = 0.8)

  # Stack the two plots vertically using patchwork
  p_hist / p_gmm + plot_layout(heights = c(2, 1.2))
}

# -------------------------------------------------------------------
# PATCH-level histogram
# -------------------------------------------------------------------

plot_patch_histogram <- function(data, mapperobject, lens_values, display_patch) {
  vertices <- mapperobject[[1]]

  # Find clusters in chosen patch
  if (!"patch" %in% names(vertices)) {
    return(ggplot() +
      ggtitle("No patch information available in mapper object."))
  }

  patch_ids <- which(vertices$patch == display_patch)
  if (length(patch_ids) == 0) {
    return(ggplot() +
      ggtitle(paste("No vertices found for patch", display_patch)))
  }

  # Collect all member points within the patch
  get_pts <- function(row) {
    if ("points_in_vertex" %in% names(vertices)) {
      return(vertices$points_in_vertex[[row]])
    }
    if ("points" %in% names(vertices)) {
      return(vertices$points[[row]])
    }
    if ("data" %in% names(vertices)) {
      return(as.integer(strsplit(vertices$data[row], ",")[[1]]))
    }
    integer(0)
  }

  patch_points <- unique(unlist(lapply(patch_ids, get_pts)))
  if (length(patch_points) == 0) {
    return(ggplot() +
      ggtitle(paste("No points in patch", display_patch)))
  }

  lens_patch <- lens_values[patch_points]
  fit <- .fit_normal(lens_patch)
  mu <- fit$mu
  sigma <- fit$sigma

  x_rng <- range(lens_patch, na.rm = TRUE)
  xpad <- diff(x_rng) * 0.1
  x_seq <- seq(x_rng[1] - xpad, x_rng[2] + xpad, length.out = 200)
  df_curve <- data.frame(lens = x_seq, density = dnorm(x_seq, mean = mu, sd = sigma))
  df_patch <- data.frame(lens = lens_patch)

  ggplot(df_patch, aes(x = lens)) +
    geom_histogram(aes(y = after_stat(density)),
      bins = 25, fill = "#4daf4a", color = "black", alpha = 0.6
    ) +
    geom_line(
      data = df_curve, aes(x = lens, y = density),
      color = "#984ea3", linewidth = 1.3
    ) +
    theme_minimal(base_size = 14) +
    labs(
      title = paste("Patch", display_patch, "Lens Histogram"),
      subtitle = sprintf("μ = %.3f, σ = %.3f", mu, sigma),
      x = "Lens Value", y = "Density"
    )
}

# -------------------------------------------------------------------
# 3) CLUSTER-level histograms
# -------------------------------------------------------------------

plot_cluster_histograms <- function(data, mapperobject, lens_values, display_patch = NULL) {
  vertices <- mapperobject[[1]]

  # Membership extraction
  extract_points <- function(i) {
    if ("points_in_vertex" %in% names(vertices)) {
      return(vertices$points_in_vertex[[i]])
    }
    if ("points" %in% names(vertices)) {
      return(vertices$points[[i]])
    }
    if ("data" %in% names(vertices)) {
      txt <- vertices$data[i]
      if (is.na(txt) || txt == "") {
        return(integer(0))
      }
      return(as.integer(strsplit(txt, ",")[[1]]))
    }
    integer(0)
  }

  cluster_dfs <- lapply(seq_len(nrow(vertices)), function(i) {
    pts <- extract_points(i)
    if (length(pts) > 0) {
      data.frame(cluster = paste0("Cluster ", i), lens = lens_values[pts])
    } else {
      NULL
    }
  })
  cluster_dfs <- do.call(rbind, cluster_dfs)
  if (is.null(cluster_dfs) || nrow(cluster_dfs) == 0) {
    return(ggplot() +
      ggtitle("No cluster data available for histogram."))
  }

  # Restrict to specific patch clusters if requested
  if ("patch" %in% names(vertices) && !is.null(display_patch)) {
    keep_ids <- which(vertices$patch == display_patch)
    keep_labels <- paste0("Cluster ", keep_ids)
    cluster_dfs <- subset(cluster_dfs, cluster %in% keep_labels)
    if (nrow(cluster_dfs) == 0) {
      return(ggplot() +
        ggtitle(paste("No clusters found for patch", display_patch)))
    }
  }

  # Fit per cluster
  norm_params <- aggregate(
    lens ~ cluster,
    data = cluster_dfs,
    FUN = function(x) c(mean = mean(x), sd = sd(x))
  )
  norm_params <- data.frame(
    cluster = norm_params$cluster,
    mu = norm_params$lens[, "mean"],
    sigma = pmax(norm_params$lens[, "sd"], 1e-6)
  )

  # Base histogram
  p <- ggplot(cluster_dfs, aes(x = lens)) +
    geom_histogram(aes(y = after_stat(density), fill = cluster),
      bins = 20, alpha = 0.6, color = "black"
    ) +
    facet_wrap(~cluster, scales = "free", ncol = 2) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",
      panel.border = element_rect(color = "#cccccc", fill = NA),
      strip.text = element_text(face = "bold")
    ) +
    labs(
      title = "Cluster Histograms (Single‑Normal Fits)",
      x = "Lens Value", y = "Density"
    )

  for (i in seq_len(nrow(norm_params))) {
    mu_i <- norm_params$mu[i]
    sd_i <- norm_params$sigma[i]
    cl_i <- norm_params$cluster[i]

    cl_data <- subset(cluster_dfs, cluster == cl_i)
    x_rng <- range(cl_data$lens, na.rm = TRUE)
    xpad <- diff(x_rng) * 0.1
    x_seq <- seq(x_rng[1] - xpad, x_rng[2] + xpad, length.out = 200)
    df_curve <- data.frame(
      lens = x_seq,
      density = dnorm(x_seq, mean = mu_i, sd = sd_i),
      cluster = cl_i
    )

    p <- p + geom_line(
      data = df_curve,
      aes(x = lens, y = density, color = cluster),
      linewidth = 1.1
    )
  }

  p
}
