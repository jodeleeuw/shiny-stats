library(shiny)
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
  sidebarLayout(
    sidebarPanel(
      numericInput("numCoins", "How many coins to flip per trial?",1,min=1,step=1),
      sliderInput("probHeads", "Probability of heads?",min=0,max=1,value=0.5),
      fluidRow(
        column(12,
        actionButton("flip1", "Run 1", css.class="btn-sm"),
        actionButton("flip10", "Run 10", css.class="btn-sm"),
        actionButton("flip100", "Run 100", css.class="btn-sm"),
        actionButton("flip1000", "Run 1000", css.class="btn-sm"),
        class="form-group")
      ),
      fluidRow(column(12,style="text-align:center",class="form-group", 
                      actionButton("reset","Reset Simulation", css.class="btn-sm")))
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))
