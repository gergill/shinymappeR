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
# GLOBAL histogram of lens values + GMM fit
# -------------------------------------------------------------------
plot_global_histogram <- function(lens_values, n_components = NULL, bins = 50) {
  library(ggplot2)
  library(mclust)
  library(dplyr)
  library(patchwork)

  lens_values <- na.omit(lens_values)
  if (length(lens_values) < 2) {
    return(list(
      plot = ggplot() +
        ggtitle("Not enough data for GMM fit."),
      xlim = c(NA, NA)
    ))
  }

  G_to_use <- if (is.null(n_components)) NULL else n_components

  # Fit optimal GMM (auto-select number of components)
  gmm_fit_optimal <- Mclust(lens_values, G = G_to_use)

  # Extract parameters for optimal model
  means_optimal <- gmm_fit_optimal$parameters$mean
  probs_optimal <- gmm_fit_optimal$parameters$pro
  n_comp_optimal <- gmm_fit_optimal$G

  # Handle variance extraction for optimal model
  if (n_comp_optimal == 1) {
    sds_optimal <- sqrt(gmm_fit_optimal$parameters$variance$sigmasq)
  } else {
    if (is.matrix(gmm_fit_optimal$parameters$variance$sigmasq)) {
      sds_optimal <- sqrt(diag(gmm_fit_optimal$parameters$variance$sigmasq))
    } else if (is.vector(gmm_fit_optimal$parameters$variance$sigmasq)) {
      sds_optimal <- sqrt(gmm_fit_optimal$parameters$variance$sigmasq)
    } else {
      sds_optimal <- rep(sqrt(gmm_fit_optimal$parameters$variance$sigmasq), n_comp_optimal)
    }
  }

  if (length(sds_optimal) == 1 && n_comp_optimal > 1) {
    sds_optimal <- rep(sds_optimal, n_comp_optimal)
  }

  # Histogram data (for density calculation)
  h <- hist(lens_values, breaks = bins, plot = FALSE)
  xmax <- max(h$breaks)
  xmin <- min(h$breaks)
  x_seq <- seq(xmin, xmax, length.out = 400)

  # store x-lim to match up with other graphs
  xlim <- c(xmin, xmax)

  # --- OPTIMAL MODEL ---
  # Each GMM component density
  component_df_optimal <- do.call(rbind, lapply(seq_len(n_comp_optimal), function(k) {
    data.frame(
      lens = x_seq,
      density = probs_optimal[k] * dnorm(x_seq, mean = means_optimal[k], sd = sds_optimal[k]),
      component = paste0("Component ", k)
    )
  }))

  # Sum to get full mixture
  mixture_density_optimal <- component_df_optimal %>%
    group_by(lens) %>%
    summarise(density = sum(density), .groups = "drop")

  # Calculate percentage contribution at each point
  mixture_total_optimal <- component_df_optimal %>%
    group_by(lens) %>%
    summarize(total_density = sum(density), .groups = "drop")

  component_contrib_optimal <- component_df_optimal %>%
    left_join(mixture_total_optimal, by = "lens") %>%
    mutate(contrib = density / total_density) %>%
    select(lens, contrib, component)

  # --- Top: histogram with optimal GMM mixture overlay ---
  p_hist <- ggplot(data.frame(lens = lens_values), aes(x = lens)) +
    geom_histogram(
      aes(y = after_stat(density)),
      bins = bins,
      fill = "#377eb8", color = "black", alpha = 0.35
    ) +
    geom_line(
      data = mixture_density_optimal,
      aes(x = lens, y = density),
      color = "#010000", linewidth = 1.5, alpha = 0.75
    ) +
    geom_line(
      data = component_df_optimal,
      aes(x = lens, y = density, color = component),
      linewidth = 1.0, linetype = "dashed", alpha = 1.0
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "right") +
    labs(
      title = sprintf(
        "Global Lens Histogram with Optimal GMM Fit (%d components)", n_comp_optimal
      ),
      x = "Lens Value", y = "Density",
      color = "Component"
    ) +
    annotate("text",
      x = Inf, y = Inf,
      label = paste0(
        "— Mixture Model\n",
        "μ: ", paste(round(means_optimal, 3), collapse = ", "), "\n",
        "σ: ", paste(round(sds_optimal, 3), collapse = ", "), "\n",
        "w: ", paste(round(probs_optimal, 3), collapse = ", ")
      ),
      hjust = 1, vjust = 1, size = 3.5, color = "#e41a1c"
    )

  # --- Bottom: Component contributions for n_components model (if specified) ---
  if (!is.null(n_components) && is.numeric(n_components) && n_components > 0) {
    gmm_fit_custom <- Mclust(lens_values, G = n_components)

    means_custom <- gmm_fit_custom$parameters$mean
    probs_custom <- gmm_fit_custom$parameters$pro
    n_comp_custom <- gmm_fit_custom$G

    # Handle variance extraction
    if (n_comp_custom == 1) {
      sds_custom <- sqrt(gmm_fit_custom$parameters$variance$sigmasq)
    } else {
      if (is.matrix(gmm_fit_custom$parameters$variance$sigmasq)) {
        sds_custom <- sqrt(diag(gmm_fit_custom$parameters$variance$sigmasq))
      } else if (is.vector(gmm_fit_custom$parameters$variance$sigmasq)) {
        sds_custom <- sqrt(gmm_fit_custom$parameters$variance$sigmasq)
      } else {
        sds_custom <- rep(sqrt(gmm_fit_custom$parameters$variance$sigmasq), n_comp_custom)
      }
    }

    if (length(sds_custom) == 1 && n_comp_custom > 1) {
      sds_custom <- rep(sds_custom, n_comp_custom)
    }

    # Calculate component contributions for custom model
    component_df_custom <- do.call(rbind, lapply(seq_len(n_comp_custom), function(k) {
      comp_density <- probs_custom[k] * dnorm(x_seq, mean = means_custom[k], sd = sds_custom[k])
      data.frame(
        lens = x_seq,
        density = comp_density,
        component = paste0("Component ", k)
      )
    }))

    mixture_total_custom <- component_df_custom %>%
      group_by(lens) %>%
      summarize(total_density = sum(density), .groups = "drop")

    component_contrib_custom <- component_df_custom %>%
      left_join(mixture_total_custom, by = "lens") %>%
      mutate(contrib = density / total_density) %>%
      select(lens, contrib, component)

    p_contrib_custom <- ggplot() +
      geom_line(
        data = component_contrib_custom,
        aes(x = lens, y = contrib, color = component),
        linewidth = 1.0
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom") +
      scale_y_continuous(labels = scales::percent) +
      labs(
        title = sprintf("Component Contributions - Custom Model (%d components)", n_comp_custom),
        x = "Lens Value", y = "% Contribution",
        color = "Component"
      ) +
      annotate("text",
        x = Inf, y = Inf,
        label = paste0(
          "μ: ", paste(round(means_custom, 3), collapse = ", "), "\n",
          "σ: ", paste(round(sds_custom, 3), collapse = ", "), "\n",
          "w: ", paste(round(probs_custom, 3), collapse = ", ")
        ),
        hjust = 1, vjust = 1, size = 3, alpha = 0.8
      )

    # Extract colors from the custom model ggplot
    component_colors <- ggplot_build(p_contrib_custom)$data[[1]]$colour
    component_colors <- unique(component_colors)

    # Stack all three plots
    combined_plot <- p_hist / p_contrib_custom + plot_layout(heights = c(2, 1))
  } else {
    # Extract colors from the optimal model ggplot
    component_colors <- ggplot_build(p_hist)$data[[3]]$colour
    component_colors <- unique(component_colors)

    # If n_components is NULL, only show optimal model plots
    combined_plot <- p_hist + plot_layout(heights = c(2))
  }

  # return both the plot and xlim
  list(plot = combined_plot, xlim = xlim, colors = component_colors)
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
# CLUSTER-level histograms
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
