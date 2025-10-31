library(shiny)
library(mappeR)
library(RColorBrewer)

source("dataset_generation.R")
source("lens_functions.R")
source("hierarchical_clusterers.R")
source("plot_dendrograms.R")
source("gmapper_cover.R")
source("width_balanced_cover.R")
source("visualization.R")

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
      paste("Using uploaded dataset:", input$upload$name)
    } else {
      paste("Using built-in dataset:", input$data)
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
    create_1D_mapper_object(
      data(),
      dist(data()),
      filtered_data(),
      cover(),
      clusterer()
    )
  })

  # --- Update Display Patch Slider -----------------------------------------
  observeEvent(list(input$cover_method, input$num_patches, cover()), {
    if (input$cover_method == "Width-Balanced") {
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
    plot_staggered_data(
      data(),
      cover(),
      get_lens(input$lens),
      input,
      filtered_data()
    )
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
