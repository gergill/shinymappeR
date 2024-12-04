#
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
                "projection",
                "Projection coordinate",
                choices = c("x", "y")
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
            ),
            selectInput("global", "Global or local clustering?", choices = c("global", "local"))
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

    # generate cover
    cover = reactive({
        projection = switch(input$projection, "x" = data$x, "y" = data$y)
        create_width_balanced_cover(min(projection),
                                    max(projection),
                                    input$bins,
                                    input$percent_overlap)
    })

    # generate mapper graph
    mapper = reactive({
        data = data()
        cover = cover()
        projection = switch(input$projection, "x" = data$x, "y" = data$y)
        global = switch(input$global, "global" = TRUE, "local" = FALSE)

        create_1D_mapper_object(
            data,
            dist(data),
            projection,
            cover,
            input$method,
            global
        )
    })

    output$inputdata <- renderPlot({
        data = data()
        projection = switch(input$projection, "x" = data$x, "y" = data$y)
        cover = cover()

        colorRampAlpha <- function(..., n, alpha) {
            colors <- colorRampPalette(...)(n)
            paste(colors, sprintf("%x", ceiling(255 * alpha)), sep = "")
        }
        colfunc = colorRampAlpha(c("blue", "gold", "red"),
                                 alpha = .5,
                                 n = input$bins)
        plot(data, pch = 20)
        switch(input$projection,
               "x" = rect(cover[, 1], min(data$y), cover[, 2], max(data$y), col = colfunc),
               "y" = rect(min(data$x), cover[, 2], max(data$x), cover[, 1], col = colfunc))

    })
    output$distPlot <- renderPlot({
        plot(mapper_object_to_igraph(mapper()))
    })
}

# Run the application
shinyApp(ui = ui, server = server)
