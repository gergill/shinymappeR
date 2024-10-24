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
install_github("https://github.com/Uiowa-Applied-Topology/mappeR/tree/dev")
library(mappeR)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("1D mapper on a circle"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("method", "clustering method", choices = c("single", "complete", "average", "ward.D2", "mcquitty")),
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 20,
                        value = 10),
            sliderInput("percent_overlap",
                        "percent overlap:",
                        min = 0,
                        max = 100,
                        value = 25),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("circle"),
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    data = data.frame(x = sapply(1:1000, cos) + runif(1000, 0, .1), y = sapply(1:1000, sin) + runif(1000, 0, .1))
    dists = dist(data)

    output$circle <- renderPlot({
        colorRampAlpha <- function(..., n, alpha) {
            colors <- colorRampPalette(...)(n)
            paste(colors, sprintf("%x", ceiling(255*alpha)), sep="")
        }
        colfunc = colorRampAlpha(c("blue", "gold", "red"), alpha = .5, n = input$bins)
        cover = create_width_balanced_cover(min(data$x), max(data$x), input$bins, input$percent_overlap)
        plot(data, pch=20)
        rect(cover[,1], -10, cover[,2], 10, col = colfunc)
    })
    output$distPlot <- renderPlot({
        plot(mapper_object_to_igraph(create_1D_mapper_object(data, dists, data$x, create_width_balanced_cover(min(data$x), max(data$x), input$bins, input$percent_overlap), input$method)))
    })
}

# Run the application
shinyApp(ui = ui, server = server)
