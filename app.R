# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)
library(mappeR)
library(RColorBrewer)

source("dataset_generation.R")
source("lens_functions.R")
source("hierarchical_clusterers.R")
source("plot_dendrograms.R")
source("gmapper_cover.R")

# ------------------------------------------------------------------
# USER INTERFACE
# ------------------------------------------------------------------

ui <- navbarPage(
  "1D Mapper",

  # --- DATA AND LENSES TAB -----------------------------------------
  tabPanel(
    "Data and Lenses",
    sidebarLayout(
      sidebarPanel(
        fileInput(
          "upload",
          "Upload CSV dataset:",
          accept = c(".csv", "text/csv", "text/plain")
        ),
        checkboxInput("header", "CSV has header", TRUE),
        helpText(
          "Upload a two-column CSV file (x and y). ",
          "If no file is uploaded, a built-in dataset generator is used."
        ),
        selectInput(
          "data",
          "Example Dataset:",
          choices = names(dataset_registry),
          selected = "circle"
        ),
        uiOutput("dynamic_dataset_ui"),
        selectInput(
          "lens",
          "Lens Function:",
          choices = names(lens_registry),
          selected = "project to x"
        ),
        uiOutput("dynamic_lens_ui")
      ),
      mainPanel(
        plotOutput("filtered_data"),
        plotOutput("mapper")
      )
    )
  ),

  # --- COVERING AND CLUSTERING TAB ----------------------------------
  tabPanel(
    "Covering and Clustering",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "cover_method",
          "Covering Method:",
          choices = c("Width-Balanced", "G‑Mapper"),
          selected = "Width-Balanced"
        ),
        conditionalPanel(
          condition = "input.cover_method == 'Width-Balanced'",
          sliderInput("num_patches", "Number of patches:", 1, 20, 10),
          sliderInput("percent_overlap", "Percent overlap:", 0, 100, 25)
        ),
        conditionalPanel(
          condition = "input.cover_method == 'G‑Mapper'",
          sliderInput("iterations", "Max iterations:", 1, 100, 20),
          sliderInput("ad_threshold", "A–D threshold:", 0.1, 10, 0.5, step = 0.1),
          sliderInput("g_overlap", "Gaussian overlap:", 0.05, 0.9, 0.3, step = 0.05),
          sliderInput("max_intervals", "Maximum number of intervals:", 5, 50, 10)
        ),
        hr(),
        sliderInput("display_patch", "Patch to display:", value = 1, min = 1, max = 2),
        selectInput("method", "Linkage method:",
          choices = c("single", "complete", "average", "mcquitty")
        ),
        selectInput("clusterer", "Cutting height method:",
          choices = c("global", "local")
        )
      ),
      mainPanel(
        textOutput("data_source"),
        plotOutput("staggered_data"),
        plotOutput("patch_view"),
        plotOutput("global_view")
      )
    )
  )
)

# ------------------------------------------------------------------
# SERVER LOGIC
# ------------------------------------------------------------------

server <- function(input, output, session) {
  # --- Dynamic Dataset Parameter UI -----------------------------------------
  observeEvent(input$data, {
    generator <- get_generator(input$data)
    output$dynamic_dataset_ui <- renderUI({
      generator$get_ui()
    })
  })

  # --- Dynamic Lens Parameter UI --------------------------------------------
  observeEvent(input$lens, {
    lens_obj <- get_lens(input$lens)
    output$dynamic_lens_ui <- renderUI({
      lens_obj$get_ui()
    })
  })

  # --- Data Reactive --------------------------------------------------------
  data <- reactive({
    if (!is.null(input$upload)) {
      df <- tryCatch(
        read.csv(input$upload$datapath, header = input$header),
        error = function(e) NULL
      )
      validate(
        need(!is.null(df), "Failed to read CSV."),
        need(ncol(df) == 2, "CSV must contain exactly two numeric columns.")
      )
      df <- df[, 1:2]
      colnames(df) <- c("x", "y")
      rownames(df) <- seq_len(nrow(df))
      return(df)
    }

    gen <- get_generator(input$data)
    params <- lapply(names(gen$param_spec), function(p) input[[p]])
    names(params) <- names(gen$param_spec)
    gen$generate(params)
  })

  output$data_source <- renderText({
    if (!is.null(input$upload)) {
      paste("Using uploaded dataset:", input$upload$name)
    } else {
      paste("Using built-in dataset:", input$data)
    }
  })

  # --- Filtered Data ---------------------------------------------------------
  filtered_data <- reactive({
    df <- data()
    lens_obj <- get_lens(input$lens)
    params <- lapply(names(lens_obj$param_spec), function(p) input[[p]])
    names(params) <- names(lens_obj$param_spec)

    vals <- lens_obj$compute(df, params)
    # 🟢 Ensure names align with the dataset
    names(vals) <- rownames(df)
    vals
  })

  # --- Cover Creation --------------------------------------------------------
  cover <- reactive({
    lens <- filtered_data()
    if (input$cover_method == "Width-Balanced") {
      create_width_balanced_cover(
        min(lens), max(lens),
        input$num_patches, input$percent_overlap
      )
    } else {
      create_gmapper_cover(
        lens,
        iterations = input$iterations,
        max_intervals = input$max_intervals,
        ad_threshold = input$ad_threshold,
        g_overlap = input$g_overlap
      )
    }
  })

  # --- Mapper Object ---------------------------------------------------------
  clusterer <- reactive({
    dists <- dist(data())
    switch(input$clusterer,
      "local" = local_hierarchical_clusterer(input$method),
      "global" = global_hierarchical_clusterer(input$method, dists)
    )
  })

  mapper <- reactive({
    create_1D_mapper_object(
      data(),
      dist(data()),
      filtered_data(),
      cover(),
      clusterer()
    )
  })

  # --- update the "Patch to display" slider when cover changes ----
  observeEvent(list(input$cover_method, input$num_patches, cover()), {
    if (input$cover_method == "Width-Balanced") {
      # For width-balanced covers, patches = num_patches
      updateSliderInput(
        session,
        "display_patch",
        max = input$num_patches,
        value = min(input$display_patch, input$num_patches),
        step = 1
      )
    } else if (input$cover_method %in% c("G-Mapper", "G‑Mapper")) {
      n_cov <- tryCatch(nrow(cover()), error = function(e) 1)
      updateSliderInput(
        session,
        "display_patch",
        max = n_cov,
        value = min(input$display_patch, n_cov),
        step = 1
      )
    }
  })

  # --- Plots -----------------------------------------------------------------

  output$filtered_data <- renderPlot({
    vals <- filtered_data()
    df <- data()
    col <- colorRampPalette(brewer.pal(9, "YlOrRd"))(50)
    col_assign <- col[as.numeric(cut(vals, breaks = 50))]
    plot(df, col = col_assign, pch = 20, asp = 1, axes = FALSE)
  })

  output$mapper <- renderPlot({
    plot(mapper_to_igraph(mapper()))
  })

  output$staggered_data <- renderPlot({
    df <- data()
    cov <- cover()
    lens_obj <- get_lens(input$lens)

    # Base scatter plot
    plot(df,
      asp = 1, pch = 20, col = "grey",
      xlab = "", ylab = "",
      xlim = range(df$x), ylim = range(df$y)
    )

    if (lens_obj$projection) {
      n_cov <- nrow(cov)
      cov_colors <- viridis::viridis(n_cov, alpha = 0.35, option = "D")

      if (input$lens == "project to x") {
        # ---- horizontal rectangles spanning y ----
        for (i in seq_len(n_cov)) {
          rect(
            xleft = cov[i, 1],
            ybottom = min(df$y),
            xright = cov[i, 2],
            ytop = max(df$y),
            col = cov_colors[i],
            border = NA
          )
        }
      } else if (input$lens == "project to y") {
        # ---- vertical rectangles spanning x ----
        for (i in seq_len(n_cov)) {
          rect(
            xleft = min(df$x),
            ybottom = cov[i, 1],
            xright = max(df$x),
            ytop = cov[i, 2],
            col = cov_colors[i],
            border = NA
          )
        }
      } else if (input$lens == "theta lens") {
        pinfo <- lens_obj$projection_fn(df, input$theta)
        pvec <- pinfo$vector / sqrt(sum(pinfo$vector^2))

        # Perpendicular direction = circle radius
        perp <- c(-pvec[2], pvec[1])
        perp <- perp / sqrt(sum(perp^2))


        # Draw cover strips perpendicular to radius (i.e. along tangent)
        for (i in seq_len(n_cov)) {
          # Compute endpoints along tangent direction
          cut1 <- pinfo$point + cov[i, 1] * pvec
          cut2 <- pinfo$point + cov[i, 2] * pvec

          # Expand in radial perpendicular direction for visible strip width
          c1 <- cut1 + 100 * perp
          c2 <- cut1 - 100 * perp
          c3 <- cut2 - 100 * perp
          c4 <- cut2 + 100 * perp

          polygon(
            x = c(c1[1], c2[1], c3[1], c4[1]),
            y = c(c1[2], c2[2], c3[2], c4[2]),
            col = cov_colors[i], border = NA
          )
        }
      } else if (grepl("PCA", input$lens)) {
        # ---- PCA projection lines/cover polygons ----
        pinfo <- lens_obj$projection_fn(df)
        pvec <- pinfo$vector / sqrt(sum(pinfo$vector^2))
        slope <- pvec[2] / pvec[1]

        # Draw main PCA direction
        abline(a = 0, b = slope, col = "darkgreen", lwd = 3, lty = 3)

        # Draw cover tiles perpendicular to PCA vector
        perp <- c(-pvec[2], pvec[1])
        perp <- perp / sqrt(sum(perp^2))

        for (i in seq_len(n_cov)) {
          cut1 <- cov[i, 1] * pvec
          cut2 <- cov[i, 2] * pvec

          c1 <- cut1 + 100 * perp
          c2 <- cut1 - 100 * perp
          c3 <- cut2 - 100 * perp
          c4 <- cut2 + 100 * perp

          polygon(
            x = c(c1[1], c2[1], c3[1], c4[1]),
            y = c(c1[2], c2[2], c3[2], c4[2]),
            col = cov_colors[i], border = NA
          )
        }
      }
    } else {
      # Color points by non-projection lens value
      vals <- filtered_data()
      val_colors <- colorRampPalette(brewer.pal(9, "YlOrRd"))(50)
      col_assign <- val_colors[as.numeric(cut(vals, breaks = 50))]
      points(df, col = col_assign, pch = 20)
    }
  })

  output$patch_view <- renderPlot({
    df <- data()
    mapper_obj <- mapper()
    vertices <- mapper_obj[[1]]
    global_dists <- dist(df)

    this_patch <- vertices[vertices$patch == input$display_patch, ]
    this_patch_data <- this_patch[, "data"]
    this_patch_names <- unlist(strsplit(this_patch_data, ","))
    rows <- as.numeric(this_patch_names)
    datasub <- df[rows, ]

    patch_dists <- dist(datasub)
    patch_dend <- hclust(patch_dists, input$method)
    global_dend <- hclust(global_dists, input$method)
    global_cut <- get_longevity_cut_height(global_dend, max(global_dists))
    patch_cut <- if (input$clusterer == "local") {
      get_longevity_cut_height(patch_dend, max(patch_dists))
    } else {
      global_cut
    }

    par(mfrow = c(1, 2))
    clusters <- cutree(patch_dend, h = patch_cut)
    num_clusts <- length(unique(clusters))
    cols <- brewer.pal(num_clusts, "Dark2")
    col_assign <- sapply(clusters, function(x) cols[x])
    plot(datasub, pch = 20, col = col_assign, asp = 1, axes = FALSE)
    plot_dendrogram(
      patch_dend, input$method,
      if (input$clusterer == "local") max(patch_dists) else max(global_dists),
      patch_cut, paste("Patch", input$display_patch),
      paste("Linkage:", input$method)
    )
  })

  output$global_view <- renderPlot({
    df <- data()
    global_dists <- dist(df)
    global_dend <- hclust(global_dists, input$method)
    global_cut <- get_longevity_cut_height(global_dend, max(global_dists))

    par(mfrow = c(1, 2))
    clusters <- cutree(global_dend, h = global_cut)
    num_clusts <- length(unique(clusters))
    cols <- brewer.pal(num_clusts, "Dark2")
    col_assign <- sapply(clusters, function(x) cols[x])

    plot(df, pch = 20, col = col_assign, asp = 1, axes = FALSE)
    plot_dendrogram(
      global_dend, input$method,
      max(global_dists), global_cut,
      "All Data", paste("Linkage:", input$method)
    )
  })
}

# ------------------------------------------------------------------
# RUN APP
# ------------------------------------------------------------------
shinyApp(ui = ui, server = server)
