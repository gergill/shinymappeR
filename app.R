# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)

source("ui.R")
source("server.R")

# RUN
shinyApp(ui = ui, server = server)
