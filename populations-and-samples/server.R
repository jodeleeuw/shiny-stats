
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
  
  createFiniteIQPopulation <- function(){
    rv$iqPopulation <- data.frame(IQ=rnorm(input$iqPopSize, mean=input$iqMean, sd=input$iqSD), Sampled=F)
  }
  
  sampleIQ <- function(n){
    if(input$iqPopSizeSelect){
      sampleFromFiniteIQPopulation(n)
    } else {
      sampleFromInfiniteIQPopulation(n)
    }
  }
  
  sampleFromInfiniteIQPopulation <- function(n){
    s <- rnorm(n, mean=input$iqMean, sd=input$iqSD)
    if(is.null(rv$iqSample)){
      rv$iqSample <- s
    } else {
      rv$iqSample <- c(rv$iqSample, s)
    }
  }
  
  sampleFromFiniteIQPopulation <- function(n){
    opts <- which(rv$iqPopulation$Sampled==F)
    if(length(opts) < n){
      n <- length(opts)
      if(n == 0){
        return()
      }
    }
    s <- sample(opts, n)
    rv$iqPopulation$Sampled[s] <- T
    if(is.null(rv$iqSample)){
      rv$iqSample <- rv$iqPopulation$IQ[s]
    } else {
      rv$iqSample <- c(rv$iqSample, rv$iqPopulation$IQ[s])
    }
  }
  
  reset <- function(){
    rv$iqPopulation = NULL
    rv$iqSample = NULL
    if(input$iqPopSizeSelect){
      createFiniteIQPopulation()
    }
  }
  
  observeEvent(input$add1, {
    sampleIQ(1)
  })
  
  observeEvent(input$add10, {
    sampleIQ(10)
  })
  
  observeEvent(input$add100, {
    sampleIQ(100)
  })
  
  observeEvent(input$add1000, {
    sampleIQ(1000)
  })
  
  observeEvent(input$reset, {
    reset()
  })
  
  observe({
    fixed <- input$iqPopSizeSelect
    n <- input$iqPopSize
    if(fixed){
      createFiniteIQPopulation()
    }
  })
  
  output$iqPopulationPlot <- renderPlot({
    limit <- c(input$iqMean - input$iqSD*6, input$iqMean + input$iqSD*6)
    
    if(input$iqPopSizeSelect){
      
      ggplot(rv$iqPopulation, aes(x=IQ))+
        coord_cartesian(xlim=limit)+
        geom_histogram(binwidth = input$iqSD/5)+
        geom_vline(xintercept = mean(rv$iqPopulation$IQ))+
        labs(y="Frequency\n",x="\nIQ")+
        theme_minimal(base_size=18)
      
    } else {
      
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
        geom_histogram(binwidth = input$iqSD/5)+
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
      
      ggplot(data.frame(samplesize = size, sampleerror = error, samplemean=cavg),
             aes(x=samplesize, y=samplemean))+
        geom_line()+
        geom_hline(yintercept=iqPopAvg())+
        coord_cartesian(ylim=c(iqPopAvg() - input$iqSD*1.5, iqPopAvg() + input$iqSD*1.5))+
        labs(x="\nSample size",y="Mean of the sample\n")+
        theme_minimal(base_size=18)
    }
  })
  
})
