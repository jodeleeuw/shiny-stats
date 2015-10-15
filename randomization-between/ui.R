
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(shinyjs)
library(shinythemes)
require(shinysky)
#library(shinyTable)

## helper function for styled buttons
actionButton <- function(inputId, label, btn.style = "" , css.class = "") {
  if ( btn.style %in% c("primary","info","success","warning","danger","inverse","link")) {
    btn.css.class <- paste("btn",btn.style,sep="-")
  } else btn.css.class = ""
  
  tags$button(id=inputId, type="button", class=paste("btn action-button",btn.css.class,css.class,collapse=" "), label)
}

shinyUI(fluidPage(
  titlePanel('Resampling'),
  
  fluidRow(
    column(4,
           wellPanel(
             HTML('<legend>Input observed data</legend>'),
             fluidRow(
               column(6, numericInput("obsA", "# of observations in Group A", 1, min = 1),
                      hotable("tblA")),
               column(6, numericInput("obsB", "# of observations in Group B", 1, min = 1), 
                      hotable("tblB"))
             )
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
             )
             
           ),
           wellPanel(
             HTML('<legend>Observed data summary</legend>'),
             htmlOutput('observedSummary'),
             plotOutput("groupsPlot")
           )
           
    ),
    column(8,
           uiOutput("plotArea"),
           #plotOutput("distPlot"),
           wellPanel(
             textOutput('rangeInfo')
           ),
           wellPanel(
             HTML('<legend>Select outcomes</legend>'),
             radioButtons('displayType', "Select range based on:",
                          c("Difference of means" = "number",
                            "Percentiles" = "percentile")),
             selectInput("rangeType", "Select the outcomes that are", c("inside", "outside"), selected="inside"),
             conditionalPanel('input.displayType == "number"',
                              uiOutput('evaluationPanel')
             ),
             conditionalPanel('input.displayType == "percentile"',
                              sliderInput("percentile", label="the percentile range", min=0,max=100,step=0.5, val = c(25,75))
             )
           )
    )
  )))
