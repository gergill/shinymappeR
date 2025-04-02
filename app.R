# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(devtools)
library(mappeR)
source("dataset_generation.R")
source("lens_functions.R")
source("cover_logic.R")

color_gradient = colorRampPalette(c('blue', 'gold', 'red'))

# Define UI for application that constructs mapper graph
ui <- fluidPage(

  titlePanel("1D Mapper"),

  # Sidebar with parameter input options
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "data",
        "Dataset",
        choices = c("circle",
                    "figure 8",
                    "spiral",
                    "barbell")
      ),
      conditionalPanel(
        condition = "input.data == 'barbell'",
        sliderInput(
          "points",
          "Number of points",
          value = 1000,
          min = 100,
          max = 2000,
          step = 100
        )
      ),
      # this panel only displays when the condition is true
      conditionalPanel(
        condition = "input.data != 'barbell'",
        # this is a slider
        sliderInput(
          # internal variable name
          inputId = "points",
          # display name
          label = "Number of points",
          # initial value
          value = 1000,
          # min value
          min = 100,
          # max value
          max = 2000,
          # step size for slider bar
          step = 1
        ),
        sliderInput(
          inputId = "noise",
          label = "Noise",
          value = .1,
          min = 0,
          max = 1,
          step = 0.01
        )
      ),
    ),

    # plot data
    mainPanel(plotOutput("inputdata"))
  ),

  sidebarLayout(
    sidebarPanel(
      selectInput(
        "lens",
        "Lens Function: ",
        choices = c(
          "project to x",
          "project to y",
          "use eccentricity value",
          "PCA-1",
          "PCA-2"
        )
      )
    ),
    mainPanel(plotOutput("filtered_data"))
  )

)

# Define server logic required to construct mapper graph
server <- function(input, output) {

# data generation and mapper steps ----------------------------------------

  # generate sample data
  data = reactive({
    switch(
      input$data,
      "circle" = generate_circle(input$points, input$noise),
      "figure 8" = generate_figure_eight(input$points, input$noise),
      "spiral" = generate_spiral(input$points, input$noise),
      "barbell" = generate_barbell(input$points)
    )
  })

  # filter data
  filtered_data = reactive({
    # grab current data
    data = data()

    switch(
      input$lens,
      "project to x" = data$x,
      "project to y" = data$y,
      "use eccentricity value" = eccentricity(data),
      "PCA-1" = pca_filter(data, 1),
      "PCA-2" = pca_filter(data, 2)
    )
  })

  # generate cover
  cover = reactive({
    # grab current data
    data = data()

    # grab current filter values
    filtered_data = filtered_data()

    # create 1D width-balanced cover
    create_width_balanced_cover(min(filtered_data),
                                max(filtered_data),
                                input$bins,
                                input$percent_overlap)
  })

  # generate mapper graph
  mapper = reactive({
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
                            clusterer = hierarchical_clusterer(input$method))
  })


# output plots ------------------------------------------------------------

  # output plot of data
  output$inputdata <- renderPlot({
    # grab data
    data = data()

    # plot data
    plot(data, pch = 20, axes=FALSE, xlab="", ylab="", asp = 1)
  })

  # output plot of filtered data
  output$filtered_data <- renderPlot({
    # grab data and filtered data
    data = data()
    filtered_data = filtered_data()

    # create a vector of colors according to lens function
    col <- color_gradient(50)[as.numeric(cut(filtered_data, breaks = 50))]

    # plot data with appropriate coloring
    plot(data, pch = 20, axes = FALSE, xlab="", ylab="", col = col, asp=1)
  })

  # output plot of mapper graph
  output$mapper <- renderPlot({
    # plot igraph object obtained from mappeR
    plot(mapper_object_to_igraph(mapper()))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
