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

cards <- list(
  card(
    full_screen = TRUE,
    card_header("Data"),
    layout_sidebar(
      sidebar = data_inputs,
      plotOutput("data")
    )
  )
)

ui <- page_navbar(
  title = "One Dimensional Mapper",
  nav_panel("Dataset Generation", cards[[1]])
)

server <- function(input, output) {
  data <- reactive({generate_data(input$dataset, input$num_points, input$noise)})
  filtered_data <- reactive({filter_data(input$lens, data())})
  gg_plot <- reactive({
    data = data()
    filtered_data = filtered_data()
    ggplot(data, aes(x=x, y=y, color = filtered_data)) +
      theme_minimal() +
      theme(axis.title = element_blank()) +
      geom_point() +
      coord_fixed() +
      scale_color_viridis_c(option = "plasma", name=input$lens)
  })

  output$data <- renderPlot(gg_plot())
}

shinyApp(ui, server)
