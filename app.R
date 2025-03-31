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
source("data_input_viz.R")
source("lens_functions.R")
source("cover_logic.R")

# Define UI for application that constructs mapper graph
ui <- fluidPage(
  titlePanel("1D Mapper"),

  # Sidebar with parameter input options
  sidebarLayout(
    sidebarPanel(
            selectInput( # this is a drop down list
                "data", # internal variable name
                "Dataset", # display name
                choices = c("circle", "figure 8", "spiral", "barbell") # choices for drop down
      ),
        sliderInput( # this is a slider
          "points", # internal variable name
          "Number of points", # display name
          value = 1000, # initial value
          min = 100, # min value
          max = 2000, # max value
          step = 100 # step size for slider bar
        ),
      selectInput(
        "lens",
        "Lens Function: ",
                choices = c("project to x", "project to y", "use eccentricity value")
    ),
    sliderInput(
      "bins",
      "Number of bins:",
      min = 1,
      max = 50,
      value = 10
    ),
    sliderInput(
      "percent_overlap",
      "Percent overlap:",
      min = 0,
      max = 100,
      value = 25
    ),
    selectInput(
      "method",
      "Clustering method",
      choices = c("single", "complete", "average", "ward.D2", "mcquitty")
    )
        ),

        # plot mapper graph
        mainPanel(plotOutput("inputdata"), plotOutput("mapper"))
    ))

# Define server logic required to construct mapper graph
server <- function(input, output) {
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

      switch(input$lens, "project to x" = data$x, "project to y" = data$y, "use eccentricity value" = eccentricity_filter(data))
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

  # output data plot
  output$inputdata <- renderPlot({
    data = data()


    # plot data
    plot(data, pch = 20, axes=FALSE, asp = 1)


    #
    # # plot the bins on top of the data
    # switch(
    #   input$lens,
    #   "project to x" = rect(cover[, 1], min(data$y), cover[, 2], max(data$y), col = bincolors),
    #   "project to y" = rect(min(data$x), cover[, 2], max(data$x), cover[, 1], col = bincolors)
    # )

  })

  output$filtered_data <- renderPlot({
    data = data()
    filtered_data = filtered_data()

    cols = colorRampPalette(c('blue', 'gold', 'red'))
    col <- cols(20)[as.numeric(cut(filtered_data, breaks = 20))]

    plot(data, pch = 20, axes = FALSE, col = col, asp=1)
  })

  # output mapper graph
  output$mapper <- renderPlot({
    plot(mapper_object_to_igraph(mapper()))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
