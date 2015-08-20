library(shiny)
library(shinythemes)

shinyUI(fluidPage(theme=shinytheme('journal'),

  # Application title
  titlePanel("iPod Shuffle Simulation"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("songs",
                  "Number of Songs:",
                  min = 1,
                  max = 40,
                  value = 10),
      selectInput("device", "Which device do you want to test?",
                  c("iPod Shuffle (random)"="a", "GoTunes"="b", "ePlayer"="c", "Tracx"="d")),
      actionButton("go", "View new playlist")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tableOutput('table')
    )
  )
))
