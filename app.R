# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(mappeR)
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
          choices = names(dataset_registry), # to add a choice: go to dataset_generation.R
          selected = "circle"
        ),

        # dynamic dataset parameter controls produced from generator
        uiOutput("dynamic_dataset_ui"),

        selectInput(
          "lens",
          "Lens Function:",
          choices = c(
            "project to x",
            "project to y",
            "use eccentricity value",
            "PCA-1",
            "PCA-2"
          )
        )
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

        # Width‑Balanced Parameters
        conditionalPanel(
          condition = "input.cover_method == 'Width-Balanced'",
          sliderInput(
            "num_patches",
            "Number of patches:",
            min = 1, max = 20, value = 10
          ),
          sliderInput(
            "percent_overlap",
            "Percent overlap:",
            min = 0, max = 100, value = 25
          )
        ),

        # G‑Mapper Parameters
        conditionalPanel(
          condition = "input.cover_method == 'G‑Mapper'",
          sliderInput(
            "iterations", "Max iterations:", min = 1, max = 100, value = 20
          ),
          sliderInput(
            "ad_threshold", "A–D threshold:",
            min = 0.1, max = 10, value = 0.5, step = 0.1
          ),
          sliderInput(
            "g_overlap", "Gaussian overlap:",
            min = 0.05, max = 0.9, value = 0.3, step = 0.05
          ),
          sliderInput(
            "max_intervals", "Maximum number of intervals:",
            min = 5, max = 50, value = 10
          )
        ),

        hr(),
        sliderInput(
          "display_patch",
          "Patch to display:",
          value = 1, min = 1, max = 2, step = 1
        ),
        selectInput(
          "method",
          "Linkage method:",
          choices = c("single", "complete", "average", "mcquitty")
        ),
        selectInput(
          "clusterer",
          "Cutting height method:",
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

  # dynamically update patch slider maximum when the cover method changes
  observe({
    if (input$cover_method == "Width-Balanced") {
      updateSliderInput(session, "display_patch", max = input$num_patches)
    } else if (input$cover_method == "G‑Mapper") {
      n_cov <- tryCatch(nrow(cover()), error = function(e) 1)
      updateSliderInput(session, "display_patch", max = n_cov)
    }
  })

  # --- dynamic dataset parameter UI ---------------------------------
  observeEvent(input$data, {
    generator <- get_generator(input$data)
    output$dynamic_dataset_ui <- renderUI({
      generator$get_ui()  # produce list of sliders for dataset params
    })
  })

  # --- DATA REACTIVE ------------------------------------------------

  # TODO: Add a clear CSV btn so people don't have to reload the application
  data <- reactive({
    # Case 1: Uploaded CSV
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

    # Case 2: Generated dataset from DatasetGenerator class
    gen <- get_generator(input$data)

    param_names <- names(gen$param_spec)
    params <- lapply(param_names, function(p) input[[p]])
    names(params) <- param_names

    gen$generate(params)
  })

  # --- DATA SOURCE ---------------------------------------------
  output$data_source <- renderText({
    if (!is.null(input$upload)) {
      paste("Using uploaded dataset:", input$upload$name)
    } else {
      paste("Using built-in dataset:", input$data)
    }
  })

  # --- FILTERED DATA -----------------------------------------
  filtered_data <- reactive({
    data <- data()
    res <- switch(input$lens,
      "project to x" = data$x,
      "project to y" = data$y,
      "use eccentricity value" = eccentricity(data),
      "PCA-1" = prcomp(data, center = FALSE, scale. = FALSE)$x[, 1],
      "PCA-2" = prcomp(data, center = FALSE, scale. = FALSE)$x[, 2]
    )
    names(res) <- row.names(data)
    res
  })

  # --- COVER GENERATION ---------------------------------------------
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

  # --- CLUSTERER ----------------------------------------------------
  clusterer <- reactive({
    data <- data()
    dists <- dist(data)
    switch(input$clusterer,
      "local" = local_hierarchical_clusterer(input$method),
      "global" = global_hierarchical_clusterer(input$method, dists)
    )
  })

  # --- MAPPER CONSTRUCTION ------------------------------------------
  mapper <- reactive({
    create_1D_mapper_object(
      data(),
      dist(data()),
      filtered_data(),
      cover(),
      clusterer()
    )
  })

  # --- PLOTS --------------------------------------------------------
  output$filtered_data <- renderPlot({
    data <- data()
    filtered <- filtered_data()
    col <- color_gradient(50)[as.numeric(cut(filtered, breaks = 50))]
    plot(data, pch = 20, axes = FALSE, col = col, asp = 1,
         xlab = "", ylab = "")
  })

  output$mapper <- renderPlot({
    plot(mapper_to_igraph(mapper()))
  })

  output$staggered_data <- renderPlot({
    data <- data()
    cov <- cover()

    # basic scatter
    plot(
      data,
      xlim = c(min(data$x), max(data$x)),
      pch = 20,
      asp = 1
    )

    if (input$lens == "project to x") {
      rect(cov[, 1], min(data$y), cov[, 2], max(data$y),
        col = color_gradient(input$num_patches, .5)
      )

    } else if (input$lens == "project to y") {
      rect(min(data$x), cov[, 2], max(data$x), cov[, 1],
        col = color_gradient(input$num_patches, .5)
      )

    } else if (input$lens == "PCA-1" || input$lens == "PCA-2") {
      pc_index <- ifelse(input$lens == "PCA-1", 1, 2)
      pca_output <- prcomp(data, center = FALSE, scale. = FALSE)
      pca_vector <- pca_output$rotation[, pc_index]
      slope <- pca_vector[2] / pca_vector[1]

      # draw PCA line
      abline(0, slope, col = "green", lwd = 3, lty = 3)

      # compute perpendicular
      perp_vector <- c(-pca_vector[2], pca_vector[1])
      perp_vector <- perp_vector / sqrt(sum(perp_vector^2))

      # draw cover polygons along PCA direction
      for (i in seq_len(nrow(cov))) {
        cut1 <- cov[i, 1] * pca_vector
        cut2 <- cov[i, 2] * pca_vector

        corner1 <- cut1 + 100 * perp_vector
        corner2 <- cut1 - 100 * perp_vector
        corner3 <- cut2 - 100 * perp_vector
        corner4 <- cut2 + 100 * perp_vector

        polygon(
          x = c(corner1[1], corner2[1], corner3[1], corner4[1]),
          y = c(corner1[2], corner2[2], corner3[2], corner4[2]),
          col = color_gradient(nrow(cov), .5)[i],
          border = NA
        )
      }
    }
  })

  output$patch_view <- renderPlot({
    data <- data()
    mapper_obj <- mapper()
    vertices <- mapper_obj[[1]]
    global_dists <- dist(data)

    this_patch <- vertices[vertices$patch == input$display_patch, ]
    this_patch_data <- this_patch[, "data"]
    this_patch_names <- unlist(strsplit(this_patch_data, ","))
    rows <- as.numeric(this_patch_names)
    datasub <- data[rows, ]

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
      patch_cut,
      paste("Patch", input$display_patch),
      paste("Linkage:", input$method)
    )
  })

  output$global_view <- renderPlot({
    data <- data()
    global_dists <- dist(data)
    global_dend <- hclust(global_dists, input$method)
    global_cut <- get_longevity_cut_height(global_dend, max(global_dists))

    par(mfrow = c(1, 2))
    clusters <- cutree(global_dend, h = global_cut)
    num_clusts <- length(unique(clusters))
    cols <- brewer.pal(num_clusts, "Dark2")
    col_assign <- sapply(clusters, function(x) cols[x])

    plot(data, pch = 20, col = col_assign, asp = 1, axes = FALSE)
    plot_dendrogram(
      global_dend, input$method,
      max(global_dists), global_cut,
      "All Data", paste("Linkage:", input$method)
    )
  })
}

# ------------------------------------------------------------------
# RUN
# ------------------------------------------------------------------
shinyApp(ui = ui, server = server)
