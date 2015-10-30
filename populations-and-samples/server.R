
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
  
  rv <- reactiveValues(
    iqPopulation = NULL,
    iqSample = NULL
  )
  
  iqPopAvg <- reactive({
    if(input$iqPopSizeSelect){
      return(mean(rv$iqPopulation$IQ))
    } else {
      return(input$iqMean)
    }
  })
  
  sampleFromInfiniteIQPopulation <- function(n){
    s <- rnorm(n, mean=input$iqMean, sd=input$iqSD)
    if(is.null(rv$iqSample)){
      rv$iqSample <- s
    } else {
      rv$iqSample <- c(rv$iqSample, s)
    }
  }
  
  observeEvent(input$add100, {
    sampleFromInfiniteIQPopulation(100)
  })
  
  output$iqPopulationPlot <- renderPlot({
    
    if(input$iqPopSizeSelect){
      rv$iqPopulation <- data.frame(IQ=rnorm(input$iqPopSize, mean=input$iqMean, sd=input$iqSD))
      ggplot(rv$iqPopulation, aes(x=IQ))+
        geom_histogram(binwidth = input$iqSD/5)+
        geom_vline(xintercept = mean(rv$iqPopulation$IQ))+
        labs(y="Frequency\n",x="\nIQ")+
        theme_minimal(base_size=18)
      
    } else {
      
      limit <- c(input$iqMean - input$iqSD*6, input$iqMean + input$iqSD*6)
      ggplot(data.frame(x = limit), aes(x)) +
        coord_cartesian(xlim=limit)+
        stat_function(fun = dnorm, args = list(mean=input$iqMean, sd=input$iqSD))+
        geom_vline(xintercept = input$iqMean)+
        labs(y="Probability\n",x="\nIQ")+
        theme_minimal(base_size=18)
    }
  })
  
  output$iqPopulationSummary <- renderText({
    if(input$iqPopSizeSelect){
      paste0('There are ',input$iqPopSize,' individuals in the population. The mean IQ of the population is ', round(iqPopAvg(), digits=2))
    } else {
      paste0('The population is infinitely large. The mean IQ of the population is ', iqPopAvg())
    }
  })
  
  output$iqSamplePlot <- renderPlot({
    if(is.null(rv$iqSample)){
      return(NULL)
    } else {
      limit <- c(input$iqMean - input$iqSD*6, input$iqMean + input$iqSD*6)
      ggplot(data.frame(IQ = rv$iqSample), aes(x=IQ))+
        coord_cartesian(xlim=limit)+
        geom_histogram(binwidth = max(1, sd(rv$iqSample)/5))+
        geom_vline(xintercept = mean(rv$iqSample))+
        labs(y="Frequency\n",x="\nIQ")+
        theme_minimal(base_size=18)
    }
  })
  
  output$iqSampleSummary <- renderText({
    if(!is.null(rv$iqSample)){
      paste0('There are ', length(rv$iqSample), ' individuals in the sample. The mean of the sample is ', round(mean(rv$iqSample),digits=2))
    }
  })
  
  output$iqSamplingErrorPlot <- renderPlot({
    if(!is.null(rv$iqSample)){
      cavg <- cumsum(rv$iqSample) / (1:length(rv$iqSample))
      size <- 1:length(rv$iqSample)
      error <- abs(cavg - iqPopAvg())
    }
    
    ggplot(data.frame(samplesize = size, sampleerror = error, samplemean=cavg),
           aes(x=samplesize, y=samplemean))+
      geom_line()+
      geom_hline(yintercept=iqPopAvg())+
      theme_minimal(base_size=18)
  })
  
})
