
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(rhandsontable)

shinyUI(fluidPage(

  # Application title
  titlePanel("Random Data from a Normal Distribution"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      numericInput("n", "Number of Samples", 10),
      numericInput("m", "Mean of the Population", 100),
      numericInput("sd", "Standard deviation of the population", 10)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      rHandsontableOutput("data")
    )
  )
))
