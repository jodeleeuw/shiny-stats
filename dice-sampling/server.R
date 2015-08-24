
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
#library(shinyjs)
library(ggplot2)

shinyServer(function(input, output, session) {
  
  doRoll <- function(nDice, sides, times){
    outcome <- replicate(times, sum(sample(1:sides, nDice, replace=T)))
    return(outcome)
  }
  
  rv <- reactiveValues(
    outcomes = numeric(),
    started = F
  )
  
  observeEvent(input$roll1, {
    runRolls(1)
  })
  
  observeEvent(input$roll10, {
    runRolls(10)
  })
  
  observeEvent(input$roll100, {
    runRolls(100)
  })
  
  observeEvent(input$roll1000, {
    runRolls(1000)
  })
  
  runRolls <- function(n){
    rv$started <- T
    o <- doRoll(input$numDice, input$numSides, n)
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
    
    b <- 0:(input$numDice*input$numSides)
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
    
    if(input$numDice * input$numSides >= 1000){
      byval <- 25
    } else if(input$numDice * input$numSides >= 500){
      byval <- 10
    } else if(input$numDice * input$numSides >= 200){
      byval <- 5
    } else if(input$numDice * input$numSides >= 50){
      byval <- 2
    } else {
      byval <- 1
    }
    
    lab_breaks <- seq(from=0, to=input$numDice*input$numSides, by=byval)
    
    
    p <- ggplot(data, aes(x=val,y=freq, fill=val)) +
      geom_bar(stat="identity")+
      labs(y="# of trials\n",x="\nOutcome")+
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
      return("Roll some dice to see the result!")
    } else {
      return(paste0("There have been ",length(rv$outcomes)," runs of the simulation. ",round(p,digits=2),"% of the outcomes meet the selection criteria."))
    }
  })
  
  observe({
    if(rv$started){
      disable("numDice")
      disable("numSides")
    }
    if(!rv$started){
      enable("numDice")
      enable("numSides")
    }
  })
  
  output$evaluationPanel <- renderUI({
    maxV <- input$numDice*input$numSides
    minV <- input$numDice
    qV <- round((maxV-minV) / 4)
    sliderInput("range",label="of the following range", min=minV,max=maxV,step=1,value=c(minV+qV,maxV-qV))
  })
  
})
