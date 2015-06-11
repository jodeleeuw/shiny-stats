library(shiny)
library(shinythemes)

shinyUI(fluidPage(theme=shinytheme("journal"),
  verticalLayout(
    h1("Guessing Game", align="center"),
    p("Can you beat randomness?", class="lead", align="center"),
    htmlOutput("coin"),
    #h3(textOutput("result"), align="center"),
    fluidRow(
      column(2, offset=4, actionButton("heads", "Guess Heads"), align="center"),
      column(2, actionButton("tails", "Guess Tails"), align="center")
    ),
    h3(textOutput("correct"), align="center"),
    fluidRow(
      column(4, offset=4, align="center", htmlOutput("history"))
    )
  )
))
