
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(gdata)

shinyServer(function(input, output, session) {
  
  rv <- reactiveValues(
    outcomes = NULL,
    started = F,
    types = data.frame(Count=numeric(), Type=character())
  )
  
  observeEvent(input$resetUrn, {
    rv$types = rv$types[NULL,]
  })
  
  observeEvent(input$addUrn, {
    if(input$urnName %in% rv$types$Type){
      rv$types[rv$types$Type == input$urnName,1] <- rv$types[rv$types$Type == input$urnName,1] + input$urnCount
    } else {
      rv$types = rbind(rv$types, data.frame(Count=input$urnCount,Type=input$urnName))
    }
    updateNumericInput(session, 'urnCount', value=1)
    updateTextInput(session, 'urnName',value='')
  })
  
  output$typesList <- renderUI({
    choices <- make.names(rv$types$Type)
    names(choices) <- rv$types$Type
    selectInput('stoppingType',NULL,choices)
  })
  
  output$displayTypeChoices <- renderUI({
    choices <- make.names(rv$types$Type)
    names(choices) <- rv$types$Type
    if(length(choices) > 0){
      checkboxGroupInput('displayTypes','Which types?',choices, inline=T)
    } else {
      HTML('<div class="shiny-text-output">You must add an item to the urn before setting the filters.</div>')
    }
  })
  
  output$sampleSizeError <- renderText({
    replacement <- input$replacement == 'with'
    set <- createSet()
    if(!replacement && input$samplingType == 'fixed'){
      if(input$sampleSize <= length(set)){
        enable('run1')
        enable('run10')
        enable('run100')
        enable('run1000')
        return(NULL)
      } else {
        disable('run1')
        disable('run10')
        disable('run100')
        disable('run1000')
        return('<div class="shiny-output-error">Sample size must not be larger than the set of items in the urn when sampling without replacement.</div>')
      }
    } else {
      enable('run1')
      enable('run10')
      enable('run100')
      enable('run1000')
      return(NULL)
    }
  })
  
  createSet <- function(){
    df <- rv$types
    df$Type <- as.character(df$Type)
    set <- mapply(function(t,r){
      rep(t,r)
    }, df$Type, df$Count)
    set <- unlist(set)
    names(set) <- NULL
    return(set)
  }
  
  doSample <- function(times){
    replacement <- input$replacement == 'with'
    set <- createSet()
    if(!replacement){
      if(input$sampleSize > length(set)){
        return() 
      }
    }
    
    rv$started <- T
    
    if(input$samplingType == 'fixed'){
      s <- replicate(times, sample(set, input$sampleSize, replace=replacement))
    }
    if(input$samplingType == 'conditional'){
      if(input$replacementConditional == 'with'){
        s <- replicate(times, {
          targetCount <- 0
          samp <- character()
          while(targetCount < input$stoppingAmount){
            this_samp <- sample(set, 1)
            if(this_samp == input$stoppingType){
              targetCount <- targetCount + 1
            }
            samp <- c(samp, this_samp)
          }
          return(samp)
        }, simplify=FALSE)
      } else if(input$replacementConditional == 'without'){
        s <- replicate(times, {
          items <- sample(set) # shuffle the vector
          samp <- items[1]
          end <- 1
          while(sum(samp==input$stoppingType) < input$stoppingAmount && end < length(set)){
            end <- end+1
            samp <- items[1:end]
          }
          return(samp)
        }, simplify=FALSE)
      }
      maxl <- max(sapply(s,length))
      s <- sapply(s, function(x){
        c(x,rep(NA, maxl-length(x)))
      })
      #s <- do.call(cbind, s)
    }
    if(is.null(rv$outcomes)){
      if(is.matrix(s)){
        rv$outcomes <- s
      } else {
        rv$outcomes <- matrix(s, ncol=length(s))
      }
    } else {
      if(!is.matrix(s)){
        s <- matrix(s, ncol=length(s))
      }
      rv$outcomes <- cbindX(rv$outcomes, s)
    }
    #print(rv$outcomes)
  }
  
  observeEvent(input$run1, {
    doSample(1)
  })
  
  observeEvent(input$run10, {
    doSample(10)
  })
  
  observeEvent(input$run100, {
    doSample(100)
  })
  
  observeEvent(input$run1000, {
    doSample(1000)
  })
  
  observeEvent(input$reset, {
    rv$started <- F
    rv$outcomes <- NULL
  })
  
  output$distPlot <- renderPlot({
    
    if(length(rv$outcomes)==0) { return(NULL) }
    
    whichSummary <- input$reportingType
    summarySetTypes <- input$displayTypes
    
    if(length(summarySetTypes)==0) { return(NULL) }
    
    summaryStats <- sumOutcomes()
    
    data <- data.frame(table(summaryStats))
    colnames(data) <- c("val","freq")
    data$val <- as.numeric(as.character(data$val))
    
    maxV <- 10
    if(input$samplingType == 'fixed'){
      maxV <- input$sampleSize
    }
    if(input$samplingType == 'conditional'){
      
    }
    
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
    
    p <- ggplot(data, aes(x=val,y=freq))+ #, fill=val)) +
      geom_bar(stat="identity")+
      labs(y="# of trials\n",x="\nOutcome")+
      theme_minimal(base_size=18)
    
    if(input$summaryRange == TRUE){
      p <- p + aes(fill=inrange) + scale_fill_manual(values=c('black','red'),guide=F)
    }
    
    return(p)
    
  })
  
  output$simInfo <- renderText({
    if(is.null(dim(rv$outcomes)[2])){
      return("Run a trial to see the results!")
    } else {
      return(paste0("There have been ",dim(rv$outcomes)[2]," runs of the simulation. "))
    }
  })
  
  output$summaryNumItemsMean <- renderText({
    
    if(is.null(rv$outcomes)) { return(NULL) }
    
    summaryStats <- sumOutcomes()
    m <- mean(summaryStats)
    
    return(paste0("The mean number of selected items in each sample is ",m))
  })
  
  output$rangeSlider <- renderUI({
    stepsize <- 1
    
      if(input$samplingType == 'fixed'){
        maxV <- input$sampleSize
      }
      if(input$samplingType == 'conditional'){
        maxV <- max(sumOutcomes())
      }
   
    qV <- round(maxV / 4)
    sliderInput("range",label="of the following range", min=0,max=maxV,step=stepsize,value=c(qV,maxV-qV))
  })
  
  output$rangeInfo <- renderText({
    
    if(is.null(rv$outcomes)) { return('Run more simulations') }
    
    summaryStats <- sumOutcomes()
    
    if(input$rangeType == 'inside'){
      v <- sum(summaryStats >= input$range[1] & summaryStats <= input$range[2])
    } else {
      v <- sum(summaryStats < input$range[1] | summaryStats > input$range[2])
    }
    p <- v / length(summaryStats)*100
    if(is.nan(p)){
      return("Run more simulations.")
    } else {
      return(paste0(round(p,digits=2),"% of the outcomes meet the selection criteria."))
    }
  })
  
  output$urnItemsText <- renderUI({
    if(nrow(rv$types) == 0){
      return(HTML('<p>The urn is currently empty.</p>'))
    } else {
      return(NULL)
    }
  })
  
  output$urnItemsTable <- renderTable({
    if(nrow(rv$types) == 0){
      return(NULL)
    } else {
      return(rv$types)
    }
  }, include.rownames=F)
  
  observe({
    if(rv$started){
      disable("urnCount")
      disable("urnName")
      disable("addUrn")
      disable("samplingType")
      disable("sampleSize")
      disable("replacement")
      disable("replacementConditional")
      disable("stoppingAmount")
      disable("stoppingType")
      disable("resetUrn")
    }
    if(!rv$started){
      enable("urnCount")
      enable("urnName")
      enable("addUrn")
      enable("samplingType")
      enable("sampleSize")
      enable("replacement")
      enable("replacementConditional")
      enable("stoppingAmount")
      enable("stoppingType")
      enable("resetUrn")
    }
  })
  
  sumOutcomes <- reactive({
    if(is.null(rv$outcomes)){return(NA)}
    apply(rv$outcomes, 2, function(v){
      v <- make.names(v[!is.na(v)])
      return(sum(v %in% input$displayTypes))
    })
  })
  
})
