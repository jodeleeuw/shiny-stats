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


shinyUI(
  fluidPage(theme=shinytheme("journal"),
            useShinyjs(),
            # Application title
            titlePanel("Urn Sampling"),
            
            # Sidebar with a slider input for number of bins
            fluidRow(
              column(4,
                     wellPanel(
                       tableOutput('urnItems'),
                       actionButton('resetUrn', "Remove All Items")
                     ),
                     wellPanel(
                       numericInput('urnCount','Number',1, min=1, step=1),
                       textInput('urnName','Name'),
                       actionButton('addUrn', "Add")
                     ),
                     wellPanel(
                       radioButtons("samplingType", "Sampling Method",
                                    c("Fixed Sample Size"="fixed",
                                      "Conditional Stopping"="conditional")),
                       conditionalPanel(
                         condition = "input.samplingType == 'fixed'",
                         numericInput('sampleSize', 'Pick this many items',1,min=1,step=1),
                         selectInput('replacement',"",c('with replacement'='with','without replacement'='without'))
                       ),
                       conditionalPanel(
                         condition = "input.samplingType == 'conditional'",
                         numericInput('stoppingAmount', 'Stop after a sample contains',1),
                         uiOutput('typesList')
                         
                       )
                     ),
                     wellPanel(
                       
                       
                       fluidRow(
                         column(12,
                                actionButton("run1", "Run 1", css.class="btn-sm"),
                                actionButton("run10", "Run 10", css.class="btn-sm"),
                                actionButton("run100", "Run 100", css.class="btn-sm"),
                                actionButton("run1000", "Run 1000", css.class="btn-sm"),
                                class="form-group")
                       ),
                       fluidRow(column(12,style="text-align:center",class="form-group", 
                                       actionButton("reset","Reset Simulation", css.class="btn-sm")))
                     ),
                     wellPanel(
                       radioButtons("reportingType", "Show",
                                    c("Number of particular types in sample"="number",
                                      "Percentage of particular types in sample"="percentage")),
                       uiOutput('displayTypeChoices')
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
