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
library(stats)

# stolen from sir jacob miller
generate_spiral <- function(n=1000, noise=0.1){
  t <- runif(n, 0, 4 * pi)
  h <- runif(n, 0, 1)

  hole_mask <- (t > 1.5*pi & t < 1.75*pi) & (h>0.25 & h<0.75)
  t <- t[!hole_mask]
  h <- h[!hole_mask]

  x <- t*cos(t) + rnorm(length(t), 0, noise)
  z <- t*sin(t) + rnorm(length(t), 0, noise)

  return(data.frame(x=x, y=z))
}

generate_barbell <- function(n) {
  r1 = sqrt(runif(n*.45, 0, .5))
  a1 = runif(n*.45, 0, 2*pi)
  disk1 = data.frame(x=r1*cos(a1) - 1, y=r1*sin(a1))

  r2 = sqrt(runif(n*.45, 0, .5))
  a2 = runif(n*.45, 0, 2*pi)
  disk2 = data.frame(x=r2*cos(a2) + 1, y=r2*sin(a2))

  line = data.frame(x = runif(n/20, -.5, .5), y = rep(0, n/20))

  return(rbind(disk1, disk2, line))
}

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
                choices = c("project to x", "project to y", "use eccentricity value", "PCA-1")
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
            "circle" = data.frame(
                x = sapply(1:input$points, cos) + runif(input$points, 0, .1),
                y = sapply(1:input$points, sin) + runif(input$points, 0, .1)
            ),
            "figure 8" = data.frame(
                x = sapply(1:input$points, function(x) cos(x) / (1 + sin(x)^2)) + runif(input$points, 0, .1),
                y = sapply(1:input$points, function(x) sin(x)*cos(x) / (1 + sin(x)^2)) + runif(input$points, 0, .1)
            ),
            "spiral" = generate_spiral(input$points),
            "barbell" = generate_barbell(input$points)
        )
    })

    # filter data
    filtered_data = reactive({
      # grab current data
      data = data()

      switch(input$lens,
             "project to x" = data$x,
             "project to y" = data$y,
             "use eccentricity value" = eccentricity_filter(data), 
             "PCA-1" = {
                pca_output <- prcomp(data, center = FALSE, scale. = FALSE)
                pca_output$x[,1]
             })
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
        create_1D_mapper_object(
            data,
            dist(data),
            filtered_data,
            cover,
            input$method
        )
    })

    # output data plot
    output$inputdata <- renderPlot({
        data = data()
        filtered_data = filtered_data()
        cover = cover()

        # plot data
        plot(data, pch = 20)

        # define a function that creates a color gradient
        colorRampAlpha <- function(..., n, alpha) {
            colors <- colorRampPalette(...)(n)
            paste(colors, sprintf("%x", ceiling(255 * alpha)), sep = "")
        }

        # create a color gradient from blue to gold to red with (number of bins) colors
        bincolors = colorRampAlpha(c("blue", "gold", "red"),
                                 alpha = .5,
                                 n = input$bins)

        # plot the bins on top of the data
        switch(input$lens,
               "project to x" = rect(cover[, 1], min(data$y), cover[, 2], max(data$y), col = bincolors),
               
               "project to y" = rect(min(data$x), cover[, 2], max(data$x), cover[, 1], col = bincolors),
               
               "PCA-1" = {
                  # draw PCA line
                  pca_output <- prcomp(data, center = FALSE, scale. = FALSE)
                  pca_vector <- pca_output$rotation[,1]
                  slope <- pca_vector[2] / pca_vector[1] 
                  abline(0, slope, col = "green", lwd = 3, lty = 3)
                  
                  # calculate perpendicular vector
                  perp_vector <- c(-pca_vector[2], pca_vector[1])
                  perp_vector <- perp_vector / sqrt(sum(perp_vector^2)) # normalize
                  
                  # color (super annoying) bins using a loop since polygon
                  for (i in 1:nrow(cover)) {
                          # calculate cut points for bins on the pca line
                          cut1 <- cover[i, 1] * pca_vector
                          cut2 <- cover[i, 2] * pca_vector
                          # the 100 is just to make sure the bins don't get cutoff in the image
                          corner1 <- cut1 + 100 * perp_vector
                          corner2 <- cut1 - 100 * perp_vector
                          corner3 <- cut2 - 100 * perp_vector
                          corner4 <- cut2 + 100 * perp_vector
                          
                          # Draw filled rectangle using polygon since rect didn't work :((
                          polygon(x = c(corner1[1], corner2[1], corner3[1], corner4[1]),
                                  y = c(corner1[2], corner2[2], corner3[2], corner4[2]),
                                  col = bincolors[i])
                  }
                })
    })

    # output mapper graph
    output$mapper <- renderPlot({
        plot(mapper_object_to_igraph(mapper()))
    })
}

# Run the application
shinyApp(ui = ui, server = server)
