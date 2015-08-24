
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
#library(shinyjs)
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
    
    b <- 0:input$numCoins
    v <- sapply(b, function(b){
      if(input$rangeType == 'inside'){
        if(b >= input$range[1] & b <= input$range[2]){
          return("red")
        } else {
          return("black")
        }
      } else {
        if(b >= input$range[1] & b <= input$range[2]){
          return("black")
        } else {
          return("red")
        }
      }
    })
    
    if(input$numCoins >= 10000){
      byval <- 25
    } else if(input$numCoins >= 1000){
      byval <- 10
    } else if(input$numCoins >= 500){
      byval <- 5
    } else if(input$numCoins >= 100){
      byval <- 2
    } else {
      byval <- 1
    }
    
    lab_breaks <- seq(from=0, to=input$numCoins, by=byval)
    
    
    p <- ggplot(data, aes(x=val,y=freq, fill=val)) +
      geom_bar(stat="identity")+
      labs(y="# of trials\n",x="\n# of heads in trial")+
      scale_fill_manual(guide=F, limits=b,values=v, drop=F)+
      scale_x_discrete(breaks=lab_breaks)+
      theme_minimal(base_size=18)
    return(p)

  })
  
  output$rangeInfo <- renderText({
    if(input$rangeType == 'inside'){
      v <- sum(rv$outcomes >= input$range[1] & rv$outcomes <= input$range[2])
    } else {
      v <- sum(rv$outcomes < input$range[1] | rv$outcomes > input$range[2])
    }
    p <- v / length(rv$outcomes)*100
    if(is.nan(p)){
      return("Flip some coins to see the result!")
    } else {
      return(paste0("There have been ",length(rv$outcomes)," runs of the simulation. ",round(p,digits=2),"% of the outcomes meet the selection criteria."))
    }
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
  
  output$evaluationPanel <- renderUI({
    maxV <- input$numCoins
    qV <- round(maxV / 4)
    sliderInput("range",label="of the following range", min=0,max=input$numCoins,step=1,value=c(qV,input$numCoins-qV))
  })

})
