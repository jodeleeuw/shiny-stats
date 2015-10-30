
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Populations & Samples"),
  
  column(
    4,
    wellPanel(
      HTML('<legend>Population options</legend>'),
      selectInput(
        "populationType", "Pick a simulation type",
        c("IQ (Normal distribution)" = "iq",
          "Election (Proportions)" = "election")),
      conditionalPanel(
        condition = "input.populationType == 'iq'",
        numericInput("iqMean", "Mean", 100),
        numericInput("iqSD", "Standard deviation", 10, min=0),
        checkboxInput("iqPopSizeSelect", "Specify the population size?", FALSE),
        conditionalPanel(
          condition= "input.iqPopSizeSelect == true",
          p("This option will let you specify the exact number of individuals in the population. However, the population mean and standard deviation will not exactly match the parameters above. The actual mean and standard deviation will be shown under the graph of the population."),
          numericInput("iqPopSize", "How many individuals are in the population?", 10000, step=1)
        )
      ),
      conditionalPanel(
        condition = "input.populationType == 'election'",
        numericInput("electionN", "How many candidates?", value=2,min=2, step=1)
      )
    ),
    wellPanel(
      HTML('<legend>Sample from the population</legend>'),
      actionButton("reset", "Reset the sample"),
      actionButton("add1", "Add 1"),
      actionButton("add10", "Add 10"),
      actionButton("add100", "Add 100"),
      actionButton("add1000", "Add 1000")
    )
  ),
  column(
    8,
    plotOutput('iqPopulationPlot'),
    wellPanel(textOutput('iqPopulationSummary')),
    plotOutput('iqSamplePlot'),
    wellPanel(textOutput('iqSampleSummary')),
    plotOutput('iqSamplingErrorPlot')
  )
))
