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
library(bslib)
source("dataset_generation.R")
source("lens_functions.R")
source("global_clusterer.R")
source("local_clusterer.R")
source("plot_dendrograms.R")

color_gradient <- function(n, alpha = 0) {
  colors <- colorRampPalette(c('blue', 'gold', 'red'), space = "Lab")(n)
  if (alpha == 0) {
    return(colors)
  } else {
    return(paste(colors, sprintf("%x", ceiling(255 * alpha)), sep = ""))
  }
}

# Define UI for application that constructs mapper graph
ui <- navbarPage(
  "1D Mapper",


# classic flavor ----------------------------------------------------------


  tabPanel("Overview",
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
               sliderInput(
                 inputId = "noise",
                 label = "Noise",
                 value = .1,
                 min = 0,
                 max = 1,
                 step = 0.01
               ),
               selectInput(
                 "lens",
                 "Lens Function: ",
                 choices = c("project to x", "project to y", "use eccentricity value", "PCA-1", "PCA-2")
               ),
               sliderInput(
                 "num_patches",
                 "Number of patches:",
                 min = 1,
                 max = 20,
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
                 "Linkage method",
                 choices = c("single", "complete", "average", "ward.D2", "mcquitty")
               ),
	       selectInput(
		"clusterer",
		"Cutting Height Method",
		choices = c("global", "local")
	       )
             ),

             # plot mapper graph
             mainPanel(plotOutput("staggered_data"), plotOutput("mapper"), plotOutput("global_dendrogram"))
           )
  ),


  # lens input panel --------------------------------------------------------

  tabPanel(
    "Lens Functions",
    sidebarLayout(sidebarPanel(
      selectInput(
        "lens_seg",
        "Lens Function: ",
        choices = c(
          "project to x",
          "project to y",
          "use eccentricity value",
          "PCA-1",
          "PCA-2"
        )
      ),
      sliderInput(
        inputId = "display_patch",
        label = "Patch to Display: ",
        value = 1,
        min = 1,
        max = 2,
        step = 1
      )
    ),
    mainPanel(plotOutput("filtered_data"), plotOutput("patch_view"), plotOutput("patch_dendrogram")))
  )
)


# Define server logic required to construct mapper graph
server <- function(input, output) {


  # when lens changes, update selection
  observeEvent(input$lens, {
    updateSelectInput(
      inputId = "lens_seg",
      selected = input$lens
    )
  })

  # when lens changes, update selection
  observeEvent(input$lens_seg, {
    updateSelectInput(
      inputId = "lens",
      selected = input$lens_seg
    )
  })

  # when patch total changes, update selection
  observeEvent(input$num_patches, {
    updateSliderInput(
      inputId = "display_patch",
      max = input$num_patches
    )
  })


  # data generation and mapper steps ----------------------------------------

  # generate sample data
  data = reactive({
    switch(
      input$data,
      "circle" = generate_circle(input$points, input$noise),
      "figure 8" = generate_figure_eight(input$points, input$noise),
      "spiral" = generate_spiral(input$points, input$noise),
      "barbell" = generate_barbell(input$points, input$noise)
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
    create_width_balanced_cover(
      min(filtered_data),
      max(filtered_data),
      input$num_patches,
      input$percent_overlap
    )
  })

  # select global/local clusterer
  clusterer = reactive({
    data = data()
    dists = dist(data)
    switch(
	   input$clusterer,
	   "local" = local_tallest_hierarchical_clusterer(input$method),
	   "global" = global_tallest_hierarchical_clusterer(input$method, dists)
    )
  })

  # generate mapper graph
  mapper = reactive({
    # grab current data
    data = data()

    # grab current filter values
    filtered_data = filtered_data()

    # grab current cover
    cover = cover()

    # grab current clusterer
    clusterer = clusterer()

    # create mapper graph
    create_1D_mapper_object(data,
                            dist(data),
                            filtered_data,
			    cover,
                            clusterer)
  })


  # output plots ------------------------------------------------------------

  # output plot of data
  output$inputdata <- renderPlot({
    # grab data
    data = data()

    # plot data
    plot(
      data,
      pch = 20,
      axes = FALSE,
      xlab = "",
      ylab = "",
      asp = 1
    )
  })

  # output plot of filtered data
  output$filtered_data <- renderPlot({
    # grab data and filtered data
    data = data()
    filtered_data = filtered_data()

    # create a vector of colors according to lens function
    col <- color_gradient(50)[as.numeric(cut(filtered_data, breaks = 50))]

    # plot data with appropriate coloring
    plot(
      data,
      pch = 20,
      axes = FALSE,
      xlab = "",
      ylab = "",
      col = col,
      asp = 1
    )
  })

  output$patch_view <- renderPlot({
    data = data()
    filtered_data = filtered_data()
    cover = cover()
    mapper = mapper()

    vertices = mapper[[1]]

    this_patch = vertices[vertices$bin == input$display_patch, ]
    this_patch_data = this_patch[, "data"]
    this_patch_names = unlist(strsplit(this_patch_data, ","))
    rows = as.numeric(this_patch_names)
    datasub = data[rows, ]

    plot(
      datasub,
      pch = 20,
      axes = FALSE,
      xlab = "",
      ylab = "",
      asp = 1
    )

    rect(
      min(datasub$x),
      min(datasub$y),
      max(datasub$x),
      max(datasub$y),
      col = color_gradient(input$num_patches, .5)[[input$display_patch]]
    )
  })

  output$patch_dendrogram <- renderPlot({
    data = data()
    filtered_data = filtered_data()
    cover = cover()
    mapper = mapper()

    vertices = mapper[[1]]

    this_patch = vertices[vertices$bin == input$display_patch, ]
    this_patch_data = this_patch[, "data"]
    this_patch_names = unlist(strsplit(this_patch_data, ","))
    rows = as.numeric(this_patch_names)
    datasub = data[rows, ]
    dists = dist(datasub)

    dend = hclust(dists, input$method)

    plot_dendrogram(dend, input$method, max(dists))
  })

  # output plot of mapper graph
  output$mapper <- renderPlot({
    # plot igraph object obtained from mappeR
    plot(mapper_object_to_igraph(mapper()))
  })

  output$global_dendrogram <- renderPlot({
    data = data()
    dists = dist(data)
    dend = hclust(dists, input$method)

    plot_dendrogram(dend, input$method, max(dists))

    # plot(plot_dendrogram(dend, input$method, 2, max(dists)))
  })

  # plot of "staggered" level sets for x/y projection
  output$staggered_data <- renderPlot({
    data = data()
    filtered_data = filtered_data()
    cover = cover()

    # plot data
    plot(data, pch = 20)

    if (input$lens == "project to x") {
      rect(cover[, 1], min(data$y), cover[, 2], max(data$y), col = color_gradient(input$num_patches, .5))
    } else if (input$lens == "project to y") {
      rect(min(data$x), cover[, 2], max(data$x), cover[, 1], col = color_gradient(input$num_patches, .5))
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
