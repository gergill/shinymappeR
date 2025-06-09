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



# user interface ----------------------------------------------------------


ui <- navbarPage(
  "1D Mapper",

## data and lenses --------------------------------------------------------

  tabPanel("Data and Lenses", sidebarLayout(
    sidebarPanel(
      selectInput(
        "data",
        "Dataset",
        choices = c("circle", "fading circle", "figure 8", "spiral", "barbell")
      ),

      sliderInput(
        "points",
        "Number of points",
        value = 1000,
        min = 100,
        max = 2000,
        step = 100
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
        choices = c(
          "project to x",
          "project to y",
          "use eccentricity value",
          "PCA-1",
          "PCA-2"
          )
      )

    ),
    mainPanel(plotOutput("filtered_data"),
              plotOutput("mapper")
              )
  )),


## covering and clustering -------------------------------------------------

  tabPanel(
    "Covering and Clustering",
    sidebarLayout(
      sidebarPanel(
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

        sliderInput(
          inputId = "display_patch",
          label = "Patch to display: ",
          value = 1,
          min = 1,
          max = 2,
          step = 1
        ),

        selectInput(
          "method",
          "Linkage method",
          choices = c("single",
                      "complete",
                      "average",
                      "mcquitty"
                      )
        ),

        selectInput(
          "clusterer",
          "Cutting Height Method",
          choices = c("global",
                      "local"
                      )
          )
      ),
      mainPanel(
        plotOutput("staggered_data"),
        plotOutput("patch_view"),
        plotOutput("global_view")
      )
    )
  )
)

# data wrangling and viz creation --------------------------------------

# wrapper function to define logic on the back end
server <- function(input, output) {

  # when patch total changes, update slider values accordingly
  observeEvent(input$num_patches, {
    updateSliderInput(inputId = "display_patch", max = input$num_patches)
  })

  ## data generation and mapper steps ----------------------------------------

  # generate sample data
  data = reactive({
    switch(
      input$data,
      "circle" = generate_circle(input$points, input$noise),
      "fading circle" = generate_fading_circle(input$points, input$noise),
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
      "PCA-1" = prcomp(data, center = FALSE, scale. = FALSE)$x[, 1],
      "PCA-2" = prcomp(data, center = FALSE, scale. = FALSE)$x[, 2]
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
    create_1D_mapper_object(data, dist(data), filtered_data, cover, clusterer)
  })


  ## output plots ------------------------------------------------------------

  # output plot of filtered data with colored datapoints
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

    # we will need these values for patch and dendrogram visualization
    data = data()
    global_dists = dist(data)
    mapper = mapper()
    vertices = mapper[[1]]

    this_patch = vertices[vertices$bin == input$display_patch, ] # get vertices of mapper graph in patch
    this_patch_data = this_patch[, "data"] # grab data names from patch
    this_patch_names = unlist(strsplit(this_patch_data, ",")) # convert name strings into vector of names
    rows = as.numeric(this_patch_names) # names are chars so this works
    datasub = data[rows, ] # get actual datapoints in patch

    patch_dists = dist(datasub) # get distance matrix for patch data

    patch_dend = hclust(patch_dists, input$method) # hierarchical clustering on the patch
    global_dend = hclust(global_dists, input$method) # hierarchical clustering on the whole dataset

    global_cut_height = get_tallest_branch_height(global_dend, max(global_dists)) # best cut height for global dendrogram
    patch_cut_height = global_cut_height # default patch cut value is same as global

    if (input$clusterer == "local") {
      patch_cut_height = get_tallest_branch_height(patch_dend, max(patch_dists)) # individually find cut height if appropriate
    }

    par(mfrow = c(1, 2)) # we want one row, two columns for the plot

    clusters = cutree(patch_dend, h = patch_cut_height) # cut the patch dendrogram to find cluster assignment
    num_clusts = length(unique(clusters)) # find number of clusters
    cols = brewer.pal(num_clusts, "Dark2") # make as many colors as there are clusters
    data_cols = sapply(clusters, function(x) cols[x]) # assign colors to data points by cluster

    # plot data with appropriate coloring
    plot(
      datasub,
      pch = 20,
      axes = FALSE,
      xlab = "",
      ylab = "",
      col = data_cols,
      asp = 1
    )

    # dendrogram scale will differ depending on local/global clustering choice

    if (input$clusterer == "local") {
      plot_dendrogram(
        patch_dend,
        input$method,
        max(patch_dists), # dendrogram only goes as far as the maximum pairwise distance in the patch
        patch_cut_height,
        paste("Patch", input$display_patch),
        paste("Linkage:", input$method)
      )
    } else {
      plot_dendrogram(
        patch_dend,
        input$method,
        max(global_dists), # dendrogram goes all the way to the global maximum pairwise distance
        patch_cut_height,
        paste("Patch", input$display_patch),
        paste("Linkage:", input$method)
      )
    }
  })

  output$global_view <- renderPlot({
    data = data()
    cover = cover()
    mapper = mapper()

    vertices = mapper[[1]]

    this_patch = vertices[vertices$bin == input$display_patch, ]
    this_patch_data = this_patch[, "data"]
    this_patch_names = unlist(strsplit(this_patch_data, ","))
    rows = as.numeric(this_patch_names)
    datasub = data[rows, ]
    patch_dists = dist(datasub)
    global_dists = dist(data)


    global_dend = hclust(global_dists, input$method)
    global_cut_height = get_tallest_branch_height(global_dend, max(global_dists))

    par(mfrow = c(1, 2))

    clusters = cutree(global_dend, h = global_cut_height)
    num_clusts = length(unique(clusters))
    cols = brewer.pal(num_clusts, "Dark2")
    data_cols = sapply(clusters, function(x) cols[x])

    # plot data with appropriate coloring
    plot(
      data,
      pch = 20,
      axes = FALSE,
      xlab = "",
      ylab = "",
      col = data_cols,
      asp = 1
    )

    plot_dendrogram(
      global_dend,
      input$method,
      max(global_dists),
      global_cut_height,
      "All Data",
      paste("Linkage:", input$method)
    )
  })

  # output plot of mapper graph
  output$mapper <- renderPlot({
    # plot igraph object obtained from mappeR
    plot(mapper_to_igraph(mapper()))
  })

  # plot of "staggered" level sets for x/y projection
  output$staggered_data <- renderPlot({
    data = data()
    filtered_data = filtered_data()
    cover = cover()

    # plot data
    plot(data,
         xlim = c(min(data$x), max(data$x)),
         pch = 20,
         asp = 1)

    if (input$lens == "project to x") {
      rect(cover[, 1],
           min(data$y),
           cover[, 2],
           max(data$y),
           col = color_gradient(input$num_patches, .5))
    } else if (input$lens == "project to y") {
      rect(min(data$x),
           cover[, 2],
           max(data$x),
           cover[, 1],
           col = color_gradient(input$num_patches, .5))
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
