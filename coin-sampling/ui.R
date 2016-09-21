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
                  titlePanel("Coin Flip Simulation"),
                  
                  # Sidebar with a slider input for number of bins
                  fluidRow(
                    column(4,
                           wellPanel(
                             numericInput("numCoins", "How many coins to flip per trial?",10,min=1,step=1),
                             numericInput("probHeads", "Probability of heads?",min=0,max=1,value=0.5, step=0.001),
                             fluidRow(
                               column(12,
                                      actionButton("flip1", "Run 1", css.class="btn-sm"),
                                      actionButton("flip10", "Run 10", css.class="btn-sm"),
                                      actionButton("flip100", "Run 100", css.class="btn-sm"),
                                      actionButton("flip1000", "Run 1000", css.class="btn-sm"),
                                      actionButton("flip10000", "Run 10000", css.class="btn-sm"),
                                      class="form-group")
                             ),
                             fluidRow(column(12,style="text-align:center",class="form-group", 
                                             actionButton("reset","Reset Simulation", css.class="btn-sm")))
                           ),
                           wellPanel(
                             radioButtons('displayType', "Select range based on:",
                                          c("Number of heads" = "number",
                                            "Percentiles" = "percentile")),
                             # selectInput("rangeType", "Select the outcomes that are", c("inside", "outside"), selected="inside"),
                             conditionalPanel('input.displayType == "number"',
                                              uiOutput('evaluationPanel')
                             ),
                             conditionalPanel('input.displayType == "percentile"',
                                              sliderInput("percentile", label="Select outcomes inside the percentile range", min=0,max=100,step=0.5, val = c(25,75))
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
