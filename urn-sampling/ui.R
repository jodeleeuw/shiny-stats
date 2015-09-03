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
                       HTML('<legend>Urn Contents</legend>'),
                       uiOutput('urnItemsText'),
                       tableOutput('urnItemsTable'),
                       actionButton('resetUrn', "Remove All Items")
                     ),
                     wellPanel(
                       HTML('<legend>Add Item(s) to the Urn</legend>'),
                       fluidRow(
                         column(3,
                                numericInput('urnCount','Number',1, min=1, step=1)
                         ),
                         column(8,
                                textInput('urnName','Name')
                         )
                       ),
                       actionButton('addUrn', "Add")
                     ),
                     wellPanel(
                       HTML('<legend>Sampling Options</legend>'),
                       radioButtons("samplingType", "Sampling Method",
                                    c("Fixed Sample Size"="fixed",
                                      "Conditional Stopping"="conditional")),
                       conditionalPanel(
                         condition = "input.samplingType == 'fixed'",
                         HTML('<label class="control-label">Pick this many items:</label>'),
                         fluidRow(
                           column(3,numericInput('sampleSize', NULL,1,min=1,step=1)),
                           column(6,selectInput('replacement',NULL,c('with replacement'='with','without replacement'='without')))
                         )
                       ),
                       conditionalPanel(
                         condition = "input.samplingType == 'conditional'",
                         HTML('<label class="control-label">Sample with replacement. Stop after a sample contains:</label>'),
                         fluidRow(
                           column(3,
                                  numericInput('stoppingAmount', NULL,1)
                           ),
                           column(6, 
                                  uiOutput('typesList')
                           )
                         )
                       ),
                       uiOutput('sampleSizeError')
                     ),
                     wellPanel(
                       HTML('<legend>Run the Simulation</legend>'),
                       fluidRow(
                         column(12, style="text-align:center",
                                actionButton("run1", "Run 1" ),
                                actionButton("run10", "Run 10" ),
                                actionButton("run100", "Run 100"),
                                actionButton("run1000", "Run 1000"),
                                class="form-group")
                       ),
                       fluidRow(column(12,style="text-align:center",class="form-group", 
                                       actionButton("reset","Reset Simulation")))
                     )
                     
              ),
              
              # Show a plot of the generated distribution
              column(8,
                     plotOutput("distPlot"),
                     fluidRow(
                       column(4,
                              wellPanel(
                                HTML('<legend>Filter Items</legend>'),
                                uiOutput('displayTypeChoices')
                              )
                       ),
                       column(4,
                              wellPanel(
                                HTML('<legend>Histogram Options</legend>'),
                                radioButtons("reportingType", "Show",
                                             c("Number of particular types in sample"="number",
                                               "Percentage of particular types in sample"="percentage"))
                                #uiOutput('displayTypeChoices')
                              )),
                       column(4,
                              wellPanel(
                                HTML('<legend>Summary Statistics</legend>'),
                                checkboxInput('summaryNumItems', 'Number of Items in Sample'),
                                conditionalPanel('input.summaryNumItems == true',
                                                 textOutput('summaryNumItemsMean')),
                                checkboxInput('summaryRange', 'Percentage of Items in Specified Range'),
                                conditionalPanel('input.summaryRange == true',
                                                 uiOutput('rangeSlider'))
                              )
                       )),
                     wellPanel(
                       textOutput('simInfo')
                     )
              )
            )
  ))
