
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyjs)
library(ggplot2)

shinyServer(function(input, output, session) {

  doFlip <- function(nCoins, p, times){
    outcome <- rbinom(times, nCoins, p)
    return(outcome)
  }
  
  rv <- reactiveValues(
    outcomes = numeric(),
    started = F
  )
  
  observeEvent(input$flip1, {
    runFlips(1)
  })
  
  observeEvent(input$flip10, {
    runFlips(10)
  })
  
  observeEvent(input$flip100, {
    runFlips(100)
  })
  
  observeEvent(input$flip1000, {
    runFlips(1000)
  })
  
  runFlips <- function(n){
    rv$started <- T
    o <- doFlip(input$numCoins, input$probHeads, n)
    rv$outcomes <- c(rv$outcomes, o)
  }
  
  observeEvent(input$reset, {
    rv$started <- F
    rv$outcomes <- numeric()
  })
  
  output$distPlot <- renderPlot({
    if(length(rv$outcomes)==0){ return(NULL) }
    
    data <- data.frame(table(rv$outcomes))
    colnames(data) <- c("val","freq")
    
    p <- ggplot(data, aes(x=val,y=freq)) +
      geom_bar(stat="identity")+
      labs(y="# of trials\n",x="\n# of heads in trial")+
      theme_bw()
    return(p)

  })
  
  observe({
    if(rv$started){
      disable("numCoins")
      disable("probHeads")
    }
    if(!rv$started){
      enable("numCoins")
      enable("probHeads")
    }
  })

})
