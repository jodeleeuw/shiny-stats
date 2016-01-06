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
                  titlePanel("Dice Rolling Simulation"),
                  
                  # Sidebar with a slider input for number of bins
                  fluidRow(
                    column(4,
                           wellPanel(
                             numericInput("numDice", "How many dice to roll per trial?",2,min=1,step=1),
                             numericInput("numSides", "How many sides does each die have?",min=2,value=6, step=1),
                             fluidRow(
                               column(12,
                                      actionButton("roll1", "Run 1", css.class="btn-sm"),
                                      actionButton("roll10", "Run 10", css.class="btn-sm"),
                                      actionButton("roll100", "Run 100", css.class="btn-sm"),
                                      actionButton("roll1000", "Run 1000", css.class="btn-sm"),
                                      class="form-group")
                             ),
                             fluidRow(column(12,style="text-align:center",class="form-group", 
                                             actionButton("reset","Reset Simulation", css.class="btn-sm")))
                           ),
                           wellPanel(uiOutput('evaluationPanel')
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
