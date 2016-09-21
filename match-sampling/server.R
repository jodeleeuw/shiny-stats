
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
  
  observeEvent(input$run10000, {
    doSimulation(10000)
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
        if(b >= input$range[1] & b <= input$range[2]){
          return("red")
        } else {
          return("black")
        }

    })
    
    data$inrange <- as.factor(data$inrange)
    
    fillv <- levels(data$inrange)
    
    p <- ggplot(data, aes(x=val,y=freq, fill=inrange)) +
      geom_bar(stat="identity")+
      labs(y="# of trials\n",x="\n# of matches in trial")+
      scale_fill_manual(guide=F, values=fillv)+
      scale_x_continuous(expand = c(0, 0), breaks = scales::pretty_breaks())+
      scale_y_continuous(expand = c(0, 0)) + 
      theme_minimal(base_size=18) +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            # panel.grid.major.y = element_line(color = "white"),
            plot.background = element_blank(),
            panel.ontop = FALSE)
    return(p)
    
  })
  
  output$rangeInfo <- renderText({
    if(length(rv$outcomes)==0){return("Run a simulation to see the result!") }
    
    v <- sum(rv$outcomes >= input$range[1] & rv$outcomes <= input$range[2])

    return(paste0("There have been ",length(rv$outcomes)," runs of the simulation. ",v," of the outcomes are between ",input$range[1], " and ", input$range[2],"."))
    
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
    sliderInput("range",label="Show outcomes that are inside the following range", min=0,max=maxV,step=1,value=c(qV,maxV-qV))
  })
  
})
