library(shiny)
library(mappeR)
library(RColorBrewer)

source("dataset_generation.R")
source("lens_functions.R")
source("hierarchical_clusterers.R")
source("plot_dendrograms.R")
source("cover_utils.R")
source("gmapper_cover.R")
source("gaussian_pdf_cover.R")
source("width_balanced_cover.R")
source("visualization.R")
source("plot_histograms.R")
source("mapper_wrapper.R")

server <- function(input, output, session) {
  # --- Dataset Parameter UI -----------------------------------------
  observeEvent(input$data, {
    generator <- get_generator(input$data)
    output$dynamic_dataset_ui <- renderUI({
      generator$get_ui()
    })
  })

  # --- Lens Parameter UI --------------------------------------------
  observeEvent(input$lens, {
    lens_obj <- get_lens(input$lens)
    output$dynamic_lens_ui <- renderUI({
      lens_obj$get_ui()
    })
  })

  # --- Reactive: Data -------------------------------------------------------
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
      paste("Uploaded dataset:", input$upload$name)
    } else {
      paste("Example dataset:", input$data)
    }
  })

  # --- Reactive: Filtered (Lens) Data --------------------------------------
  filtered_data <- reactive({
    df <- data()
    lens_obj <- get_lens(input$lens)
    params <- lapply(names(lens_obj$param_spec), function(p) input[[p]])
    names(params) <- names(lens_obj$param_spec)
    vals <- lens_obj$compute(df, params)
    names(vals) <- rownames(df)
    vals
  })

  # --- Reactive: Cover ------------------------------------------------------
  cover <- reactive({
    lens <- filtered_data()
    cov <- if (input$cover_method == "Width-Balanced") {
      create_width_balanced_cover(
        min(lens), max(lens),
        input$num_patches, input$percent_overlap,
        use_union_format = TRUE
      )
    } else if (input$cover_method == "Gaussian PDF") {
      create_gaussian_pdf_cover(
        lens,
        n_components = input$n_components,
        z_threshold = input$z_threshold,
        min_interval_size = input$min_interval_size
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

    # Ensure union format
    if (!is_union_cover(cov)) {
      cov <- convert_to_union_cover(cov)
    }

    cov
  })

  # --- Reactive: Clusterer --------------------------------------------------
  clusterer <- reactive({
    dists <- dist(data())
    switch(input$clusterer,
      "local" = local_hierarchical_clusterer(input$method),
      "global" = global_hierarchical_clusterer(input$method, dists)
    )
  })

  # --- Reactive: Mapper Object ---------------------------------------------
  mapper <- reactive({
    # Use the union-aware mapper function
    create_1D_mapper_object_union(
      data(),
      dist(data()),
      filtered_data(),
      cover(),
      clusterer()
    )
  })

  histogram_result <- reactive({
    n_comp <- if (input$cover_method == "Gaussian PDF") {
      input$n_components
    } else {
      NULL
    }

    plot_global_histogram(filtered_data(), n_components = n_comp)
  })

  # --- Reactive: Global Histogram Result which is actually static!
  global_histogram_result <- reactive({
    plot_global_histogram(filtered_data(), n_components = NULL)
  })

  # --- Update Display Patch Slider -----------------------------------------
  observeEvent(cover(), {
    cov <- cover()
    n_elements <- if (is_union_cover(cov)) length(cov) else nrow(cov)

    updateSliderInput(
      session,
      "display_patch",
      max = n_elements,
      value = min(input$display_patch, n_elements),
      step = 1
    )
  })

  # ------------------------------------------------------------------
  # VISUALIZATIONS
  # ------------------------------------------------------------------

  output$filtered_data <- renderPlot({
    plot_filtered_data(data(), filtered_data())
  })

  output$mapper <- renderPlot({
    plot_mapper_graph(mapper())
  })

  output$staggered_data <- renderPlot({
    colors <- histogram_result()$colors
    plot_staggered_data(
      data(),
      cover(),
      get_lens(input$lens),
      input,
      filtered_data(),
      colors = colors
    )
  })

  output$cluster_histograms <- renderPlot({
    plot_cluster_histograms(data(), mapper(), filtered_data(), input$display_patch)
  })

  output$global_histogram <- renderPlot({
    # Return only the plot part
    global_histogram_result()$plot
  })

  output$cover_histogram <- renderPlot({
    # Return only the plot part
    histogram_result()$plot
  })


  output$mapper_cover_splits <- renderPlot({
    # Get xlim from histogram result and pass to cover splits
    xlim <- histogram_result()$xlim
    colors <- histogram_result()$colors
    plot_mapper_cover_splits(cover(), filtered_data(), bins = 30, xlim = xlim, colors = colors)
  })

  output$patch_histogram <- renderPlot({
    plot_patch_histogram(data(), mapper(), filtered_data(), input$display_patch)
  })

  output$patch_view <- renderPlot({
    plot_patch_view(
      data(),
      mapper(),
      input$display_patch,
      input$method,
      input$clusterer
    )
  })

  output$global_view <- renderPlot({
    plot_global_view(data(), input$method)
  })
}
