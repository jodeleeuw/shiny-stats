library(shiny)
library(ggplot2)
library(shinyjs)
library(shinythemes)
require(shinysky)
#library(shinyTable)
library(rhandsontable)

## helper function for styled buttons
actionButton <- function(inputId, label, btn.style = "" , css.class = "") {
  if ( btn.style %in% c("primary","info","success","warning","danger","inverse","link")) {
    btn.css.class <- paste("btn",btn.style,sep="-")
  } else btn.css.class = ""
  
  tags$button(id=inputId, type="button", class=paste("btn action-button",btn.css.class,css.class,collapse=" "), label)
}

shinyUI(fluidPage(
  titlePanel('Bootstrapping - Within Subject'),
  
  fluidRow(
    column(4,
           wellPanel(
             HTML('<legend>Input observed data</legend>'),
             numericInput("obsA", "# of observations", 1, min = 1),
             rHandsontableOutput("tblA")
           ),
           
           wellPanel(
             HTML('<legend>Run simulations</legend>'),
             fluidRow(
               column(12,
                      actionButton("flip1", "Run 1", css.class="btn-sm"),
                      actionButton("flip10", "Run 10", css.class="btn-sm"),
                      actionButton("flip100", "Run 100", css.class="btn-sm"),
                      actionButton("flip1000", "Run 1000", css.class="btn-sm"),
                      actionButton("flip10000", "Run 10000", css.class="btn-sm"),
                      class="form-group"
               )
             ),
             radioButtons('statistic', "Show results for:",
                          c("Mean" = "meanStat",
                            "Median" = "medianStat"))
             
           ),
           wellPanel(
             HTML('<legend>Observed data summary</legend>'),
             htmlOutput('observedSummary'),
             plotOutput("groupsPlot")
           )
    ),
  
  
    column(8,
           uiOutput("plotArea"),
           # plotOutput("distPlot"),
           wellPanel(
             textOutput('rangeInfo')
           ),
           wellPanel(
             HTML('<legend>Select outcomes</legend>'),
             radioButtons('displayType', "Select range based on:",
                          c("Bootstrapped value" = "number",
                            "Percentiles" = "percentile")),
             # selectInput("rangeType", "Select the outcomes that are", c("inside", "outside"), selected="inside"),
             conditionalPanel('input.displayType == "number"',
                              uiOutput('evaluationPanel')
             ),
             conditionalPanel('input.displayType == "percentile"',
                              sliderInput("percentile", label="the percentile range", min=0,max=100,step=0.5, val = c(25,75))
             )
           )
    )
  )))
