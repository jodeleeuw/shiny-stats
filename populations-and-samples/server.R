library(shiny)
library(ggplot2)
library(randomNames)

shinyServer(function(input, output) {
  
  rv <- reactiveValues(
    iqPopulation = NULL,
    iqSample = NULL,
    electionCandidates = NULL,
    electionPopulation = NULL,
    electionSample = NULL
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
  
  createElectionCandidates <- function(){
    names <- randomNames(input$electionN, name.order = "first.last", name.sep=" ")
    electionData <- data.frame(Candidate = names, 'Percent Support'=(100/input$electionN), check.names=F)
    rv$electionCandidates <- electionData
  }
  
  createFiniteElectionPopulation <- function(){
    n <- input$electionPopSize
    if(n < 10){ return(NULL) }
    support <- rv$electionCandidates$`Percent Support`
    votes <- n*support/100
    rv$electionPopulation <- data.frame(Candidate=rep(rv$electionCandidates$Candidate,votes), Sampled=F)
  }
  
  observe({
    d <- hot.to.df(input$electionCandidates)
    d$`Percent Support` <- as.numeric(d$`Percent Support`)
    rv$electionCandidates <- d
  })
  
  #### sampling methods ####
  
  do.sample <- function(n){
    if(input$populationType == 'iq'){
      sampleIQ(n)
    }
    if(input$populationType == 'election'){
      sampleElection(n)
    }
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
  
  sampleElection <- function(n){
    if(input$electionPopSizeSelect){
      sampleFromFiniteElectionPopulation(n)
    } else {
      sampleFromInfiniteElectionPopulation(n)
    }
  }
  
  sampleFromFiniteElectionPopulation <- function(n){
    opts <- which(rv$electionPopulation$Sampled==F)
    if(length(opts) < n){
      n <- length(opts)
      if(n == 0){
        return()
      }
    }
    s <- sample(opts, n)
    rv$electionPopulation$Sampled[s] <- T
    if(is.null(rv$electionSample)){
      rv$electionSample <- as.character(rv$electionPopulation$Candidate)[s]
    } else {
      rv$electionSample <- c(rv$electionSample, as.character(rv$electionPopulation$Candidate)[s])
    }
  }
  
  sampleFromInfiniteElectionPopulation <- function(n){
    s <- sample(as.character(rv$electionCandidates$Candidate), n, replace=T, prob=rv$electionCandidates$`Percent Support`)
    if(is.null(rv$electionSample)){
      rv$electionSample <- s
    } else {
      rv$electionSample <- c(rv$electionSample, s)
    }
  }
  
  #### reset simulation ####
  
  reset <- function(){
    rv$iqPopulation = NULL
    rv$iqSample = NULL
    rv$electionSample = NULL
    rv$electionPopulation = NULL
    if(input$iqPopSizeSelect){
      createFiniteIQPopulation()
    }
    if(input$electionPopSizeSelect){
      createFiniteElectionPopulation()
    }
  }
  
  #### button handlers ####
  
  observeEvent(input$add1, {
    do.sample(1)
  })
  
  observeEvent(input$add10, {
    do.sample(10)
  })
  
  observeEvent(input$add100, {
    do.sample(100)
  })
  
  observeEvent(input$add1000, {
    do.sample(1000)
  })
  
  observeEvent(input$reset, {
    reset()
  })
  
  #### generate new populations ####
  
  observe({
    fixed <- input$iqPopSizeSelect
    n <- input$iqPopSize
    if(fixed){
      createFiniteIQPopulation()
    }
  })
  
  observe({
    type <- input$populationType
    n <- input$electionN
    createElectionCandidates()
  })
  
  observe({
    fixed <- input$electionPopSizeSelect
    data <- rv$electionCandidates
    if(fixed){
      createFiniteElectionPopulation()
    }
  })
  
  #### UI ####
  
  output$electionCandidates <- renderHotable({
    rv$electionCandidates
  }, readOnly = c(T,F))
  
  #### output IQ ####
  
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
      paste0('There are ',input$iqPopSize,' individuals in the population. The mean IQ of the population is ', round(iqPopAvg(), digits=2),'.')
    } else {
      paste0('The population is infinitely large. The mean IQ of the population is ', iqPopAvg(),'.')
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
  
  #### output election ####
  
  output$electionPopulationPlot <- renderPlot({
    if(!input$electionPopSizeSelect){
      ggplot(rv$electionCandidates, aes(x=Candidate, y=`Percent Support`))+
        labs(y="Percent Support\n")+
        geom_bar(stat="identity")+
        theme_minimal(base_size = 18)
    } else {
      ggplot(rv$electionPopulation, aes(x=Candidate))+
        labs(y="Number of Supporters\n")+
        geom_bar()+
        theme_minimal(base_size = 18)
    }
  })
  
  
  output$electionSamplePlot <- renderPlot({
    if(is.null(rv$electionSample)){
      return(NULL)
    } else {
      ggplot(data.frame(Candidate=rv$electionSample), aes(x=Candidate))+
        geom_bar(binwidth=1)+
        labs(y="Number of Supporters\n")+
        theme_minimal(base_size = 18)
    }
  })
  
  output$electionSamplingErrorPlot <- renderPlot({
    if(!is.null(rv$electionSample)){
      allcandidates <- as.character(rv$electionCandidates$Candidate)
      actualsupport <- rv$electionCandidates$`Percent Support`
      size <- 1:length(rv$electionSample)
      df <- expand.grid(candidate=allcandidates,size=size)
      df$support <- mapply(function(c,s){
        return(100*sum(rv$electionSample[1:s]==c)/s)
      },df$candidate,df$size)
      
      ggplot(df, aes(x=size, y=support, colour=candidate, group=candidate))+
        geom_line()+
        geom_hline(data=data.frame(asup=actualsupport,candidate=allcandidates), aes(yintercept=asup, colour=candidate))+
        coord_cartesian(ylim=c(0,100))+
        labs(x="\nSample size",y="Mean of the sample\n")+
        theme_minimal(base_size=18)
    }
  })
  
  output$electionSampleSummary <- renderText({
    if(!is.null(rv$electionSample)){
      candidates <- unique(rv$electionSample)
      df <- data.frame(candidates=candidates)
      df$support <- sapply(df$candidates, function(c){
        return(100*sum(rv$electionSample==c)/length(rv$electionSample))
      })
      s <- paste0('There are ', length(rv$electionSample), ' individuals in the sample. ')
      for(i in 1:nrow(df)){
        s <- paste0(s, df[i,]$candidates, ' is receiving ',df[i,]$support,'% of the vote. ')
      }
      return(s)
    }
  })
})
