source("dataset_generation.R")
source("lens_functions.R")

cards <- list(
  card(
    full_screen = TRUE,
    card_header("Data"),
    plotOutput("data")
  )
)

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
  )
)

ui <- page_sidebar(
  title = "Dataset Generation",
  sidebar = data_inputs,
  !!!cards
)

server <- function(input, output) {
  gg_plot <- reactive({
    ggplot(generate_data(input$dataset, input$num_points, input$noise), aes(x=x, y=y)) +
      theme_minimal() +
      theme(axis.title = element_blank()) +
      geom_point() +
      coord_fixed()
  })

  output$data <- renderPlot(gg_plot())
}

shinyApp(ui, server)
