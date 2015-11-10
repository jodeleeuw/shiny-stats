
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Readability Scores"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textInput('url', 'URL of Wikipedia Article'),
      actionButton('run', 'Calculate Readability')
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        type="tabs",
        tabPanel(
          "Current version",
          uiOutput('currentGrade'),
          textOutput('currentText')
        ),
        tabPanel(
          "Original entry",
          uiOutput('originalGrade'),
          textOutput('originalText')
          
        )
        
      )
    )
  )
))
