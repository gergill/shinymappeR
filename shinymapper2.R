library(shiny)
library(devtools)
library(mappeR)
library(bslib)
library(ggplot2)
source("dataset_generation.R")
source("lens_functions.R")

data_inputs <- sidebar(
  title = "Data Parameters",
  selectInput(
    "dataset", "Dataset",
    c("circle", "figure 8", "spiral", "barbell"),
    selected = "circle"
  ),
  sliderInput(
    "num_points", "Number of Datapoints",
    0, 5000, 1000
  ),
  sliderInput(
    "noise", "Noise",
    0, 1, .1
  ),
  selectInput(
    "lens", "Lens",
    c("project to x", "project to y", "eccentricity", "PCA-1"),
    selected = "project to x"
  )
)

width_cover_inputs <- card(
  title = "Width-Balanced Cover Parameters",
  sliderInput(
    "patches", "Number of Patches",
    1, 30, 8
  ),
  sliderInput(
    "overlap", "Percent Overlap",
    0, 100, 25
  )
)

cover_flavors <- sidebar(
  title = "Cover Types",
  selectInput(
    "covertype", "Cover Type",
    c("Width-Balanced", "G-Mapper"),
    selected = "Width-Balanced"
  ),
  uiOutput("cover_inputs")
)


cards <- list(
  card(
    full_screen = TRUE,
    card_header("Data"),
    layout_sidebar(
      sidebar = data_inputs,
      plotOutput("data")
    )
  ),
  card(
    full_screen = TRUE,
    card_header("Covering"),
    layout_sidebar(
      sidebar = cover_flavors,
      plotOutput("covered_data")
    )
  )
)



ui <- page_navbar(
  title = "One Dimensional Mapper",
  navset_pill(
    nav_panel("Dataset Generation", cards[[1]]),
    nav_panel("Cover Maker", cards[[2]])
  )
)

server <- function(input, output) {
  data <- reactive({generate_data(input$dataset, input$num_points, input$noise)})
  filtered_data <- reactive({filter_data(input$lens, data())})
  cover <- reactive({
    data = data()
    filtered_data = filtered_data()
    if (input$covertype == "Width-Balanced") {
      cover = create_width_balanced_cover(
        min(filtered_data),
        max(filtered_data),
        input$patches,
        input$overlap
      )
    }
  })
  mapper <- reactive({
    # grab current data
    data = data()

    # grab current filter values
    filtered_data = filtered_data()

    # grab current cover
    cover = cover()

    # create mapper graph
    create_1D_mapper_object(data,
                            dist(data),
                            filtered_data,
                            cover,
                            clusterer = hierarchical_clusterer("single"))
  })

  filtered_plot <- reactive({
    data = data()
    filtered_data = filtered_data()
    ggplot(data, aes(x=x, y=y, color = filtered_data)) +
      theme_minimal() +
      theme(axis.title = element_blank()) +
      geom_point() +
      coord_fixed() +
      scale_color_viridis_c(option = "plasma", name=input$lens)
  })

  binned_plot <- reactive({
    data = data()
    filtered_data = filtered_data()
    cover = cover()
    mapper = mapper()

    vertices = mapper[[1]]

    patches = lapply(1:input$patches, function(x) vertices[vertices$bin == x,])
    patches_data = lapply(1:input$patches, function(x) patches[[x]][, "data"])
    patches_names = lapply(1:input$patches, function(x) unlist(strsplit(patches_data[[x]], ",")))
    rows = lapply(1:input$patches, function(x) as.numeric(patches_names[[x]]))
    datasub = lapply(1:input$patches, function(x) data[rows[[x]], ])
    hulls = lapply(1:input$patches, function(x) datasub[[x]][chull(datasub[[x]]),])
    patchcols = viridisLite::plasma(input$patches)

    ggplot(data, aes(x=x, y=y)) +
      theme_minimal() +
      theme(axis.title = element_blank()) +
      geom_point() +
      coord_fixed() +
      lapply(1:input$patches, function(x) geom_polygon(data = hulls[[x]], alpha = .2, fill = patchcols[x]))
  })

  patchplot <- reactive({
    data = data()
    filtered_data = filtered_data()
    cover = cover()
    mapper = mapper()

    vertices = mapper[[1]]

    patch = vertices[vertices$bin == input$patchnum,]
    patch_data = patch[, "data"]
    patch_names = unlist(strsplit(patch_data, ","))
    rows = as.numeric(patch_names)
    datasub = data[rows, ]

    ggplot(datasub, aes(x=x, y=y)) +
      theme_minimal() +
      theme(axis.title = element_blank()) +
      geom_point() +
      coord_fixed()
  })

  output$cover_inputs <- renderUI({
    switch(input$covertype,
           "Width-Balanced" = width_cover_inputs)
  })

  output$covered_data <- renderPlot(binned_plot())

  output$data <- renderPlot(filtered_plot())
}

shinyApp(ui, server)
