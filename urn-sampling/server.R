
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
    types = data.frame(Count=numeric(), Type=numeric()),
    names = data.frame(Type = character(), Number = numeric())
  )
  
  observeEvent(input$resetUrn, {
    rv$types = rv$types[NULL,]
    rv$names = rv$names[NULL,]
  })
  
  observeEvent(input$addUrn, {
    if(input$urnCount%%1){
      shinyjs::info("Please add items to the urn in whole numbers only.")
    } else {
      if(input$urnName %in% rv$types$Type){
        rv$types[rv$names == input$urnName,1] <- rv$types[rv$names == input$urnName,1] + input$urnCount
      } else {
        rv$types = rbind(rv$types, data.frame(Count=input$urnCount,Type=input$urnName))
        rv$names = rbind(rv$names, data.frame(Type = input$urnName, Number = length(rv$names$Number)+1))
      }
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
    df$Type <- rv$names[as.character(rv$names$Type)==as.character(df$Type),"Number"]
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
      stopper <- input$stoppingType
      stopper <- rv$names[make.names(rv$names$Type)==make.names(stopper),"Number"]
      if(input$replacementConditional == 'with'){
        s <- replicate(times, {
          this_set = sample(set,10*length(set), replace=TRUE) #draws a big sample
          samp <- cumsum(1*(this_set==stopper)) #adds up the number of targets at each point
          targ <- match(input$stoppingAmount, samp) #finds the first index where we have enough targets
          if(!is.na(targ)){
            result <- this_set[1:targ] 
            return(result)
          } else { #if not enough targets, draws a bigger sample
            this_set = sample(set,100*length(set), replace=TRUE)
            samp <- cumsum(1*(this_set==stopper))
            targ <- match(input$stoppingAmount, samp)
            if(!is.na(targ)){
              result <- this_set[1:targ]
              return(result)
            } else { #should never get this far
              this_set = sample(set,1000*length(set), replace=TRUE)
              samp <- cumsum(1*(this_set==stopper))
              targ <- match(input$stoppingAmount, samp)
              result <- this_set[1:targ]
              return(result)
            }
          }
        }, simplify=FALSE)
      } else if(input$replacementConditional == 'without'){
        s <- replicate(times, {
          this_set = sample(set) #shuffles the set
          samp <- cumsum(1*(this_set==stopper)) #adds up the number of targets at each point
          targ <- match(input$stoppingAmount, samp) #finds the first index where we have enough targets
          result <- this_set[1:targ]
          return(result)
        }, simplify=FALSE)
      }
      maxl <- max(sapply(s,length))
      s <- sapply(s, function(x){
        c(x,rep(NA, maxl-length(x)))
      })
      #s <- do.call(cbind, s)
    }
    numvals <- length(unique(set))
    s <- apply(s, 2, function(v){
      b <- c()
      for(i in 1:numvals){
        count <- sum(1*(v==i), na.rm = TRUE)
        b <- c(b, count)
      }
      return(b)
    })
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
      if(b >= input$range[1] & b <= input$range[2]){
        return("red")
      } else {
        return("black")
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
    sliderInput("range",label="Show outcomes inside the range", min=0,max=maxV,step=stepsize,value=c(qV,maxV-qV))
  })
  
  output$rangeInfo <- renderText({
    
    if(is.null(rv$outcomes)) { return('Run more simulations') }
    
    summaryStats <- sumOutcomes()
    
    v <- sum(summaryStats >= input$range[1] & summaryStats <= input$range[2])
    if(length(summaryStats)<1){
      return("Run more simulations.")
    } else {
      return(paste0(v," outcomes meet the selection criteria."))
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
    types <- input$displayTypes
    types <- rv$names[make.names(rv$names$Type) %in% make.names(types), "Number"]
    outcomes <- rv$outcomes
    typeTrue <- matrix(0, nrow = 1, ncol = nrow(outcomes) )
    typeTrue[1,types] <- 1
    sums <- typeTrue%*%outcomes
    return(sums)
  })
  
})
