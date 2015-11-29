library(shiny)
library(shinyjs)
library(shinythemes)

## helper function for styled buttons
actionButton <- function(inputId, label, btn.style = "" , css.class = "") {
  if ( btn.style %in% c("primary","info","success","warning","danger","inverse","link")) {
    btn.css.class <- paste("btn",btn.style,sep="-")
  } else btn.css.class = ""
  
  tags$button(id=inputId, type="button", class=paste("btn action-button",btn.css.class,css.class,collapse=" "), label)
}


shinyUI(fluidPage(theme=shinytheme("journal"),
                  useShinyjs(),
                  # Application title
                  titlePanel("Student's t-distribution"),
                  
                  # Sidebar with a slider input for number of bins
                  fluidRow(
                    column(4,
                           wellPanel(
                             numericInput("numCoins", "How many degrees of freedom?",30,min=1,step=1)
                           ),
                           wellPanel(
                             radioButtons('displayType', "Select based on:",
                                          c("Values" = "number",
                                            "Percentiles" = "percentile")),
                             conditionalPanel('input.displayType == "number"',
                              selectInput("rangeType", "Select the outcomes that are", c("greater than or equal to"="greater", "less than or equal to"="less"), selected="greater")),
                             conditionalPanel('input.displayType == "number"',
                                              uiOutput('evaluationPanel')
                             ),
                             conditionalPanel('input.displayType == "percentile"',
                                              sliderInput("percentile", label="Show the cutoff for the percentile", min=0,max=100,step=0.5, val = c(50))
                             )
                             
                             
                             
                           )
                    ),
                    
                    # Show a plot of the generated distribution
                    column(8,
                           plotOutput("distPlot"),
                           wellPanel(
                             textOutput('rangeInfo')
                           )
                    )
                  )
))
