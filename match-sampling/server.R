
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
#library(shinyjs)
library(ggplot2)

shinyServer(function(input, output, session) {
  
  doSimulation <- function(times){
    rv$started <- T
    pairs <- input$numPairs
    s <- replicate(times, {
      s1 <- sample(1:pairs)
      val <- sum(s1 == (1:pairs))
      return(val)
    })
    rv$outcomes <- c(rv$outcomes, s)
  }
  
  rv <- reactiveValues(
    outcomes = numeric(),
    started = F
  )
  
  observeEvent(input$run1, {
    doSimulation(1)
  })
  
  observeEvent(input$run10, {
    doSimulation(10)
  })
  
  observeEvent(input$run100, {
    doSimulation(100)
  })
  
  observeEvent(input$run1000, {
    doSimulation(1000)
  })
  
  observeEvent(input$reset, {
    rv$started <- F
    rv$outcomes <- numeric()
  })
  
  output$distPlot <- renderPlot({
    if(length(rv$outcomes)==0){ return(NULL) }
    
    data <- data.frame(table(rv$outcomes))
    colnames(data) <- c("val","freq")
    data$val <- as.numeric(as.character(data$val))
    data$freq <- as.numeric(as.character(data$freq))
    
    data$inrange <- sapply(data$val, function(b){
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
    
    data$inrange <- as.factor(data$inrange)
    
    fillv <- levels(data$inrange)
    
    p <- ggplot(data, aes(x=val,y=freq, fill=inrange)) +
      geom_bar(stat="identity")+
      labs(y="# of trials\n",x="\n# of matches in trial")+
      scale_fill_manual(guide=F, values=fillv)+
      #scale_x_discrete(breaks=lab_breaks)+
      theme_minimal(base_size=18)
    return(p)
    
  })
  
  output$rangeInfo <- renderText({
    if(length(rv$outcomes)==0){return("Run a simulation to see the result!") }
    
    if(input$rangeType == 'inside'){
      v <- sum(rv$outcomes >= input$range[1] & rv$outcomes <= input$range[2])
    } else {
      v <- sum(rv$outcomes < input$range[1] | rv$outcomes > input$range[2])
    }
    p <- v / length(rv$outcomes)*100
    
    return(paste0("There have been ",length(rv$outcomes)," runs of the simulation. ",round(p,digits=2),"% of the outcomes meet the selection criteria."))
    
  })
  
  observe({
    if(rv$started){
      disable("numPairs")
    }
    if(!rv$started){
      enable("numPairs")
    }
  })
  
  output$evaluationPanel <- renderUI({
    maxV <- input$numPairs
    qV <- round(maxV / 4)
    sliderInput("range",label="of the following range", min=0,max=maxV,step=1,value=c(qV,maxV-qV))
  })
  
})
