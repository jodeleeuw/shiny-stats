
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
  
  output$urnItems <- renderTable(rv$types, include.rownames=F)
  
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
      s <- replicate(times, {
        samp <- sample(set, 1)
        while(sum(samp==input$stoppingType) < input$stoppingAmount){
          samp <- c(samp, sample(set, 1))
        }
        return(samp)
      }, simplify=FALSE)
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
  
  getFilteredSet <- function(){
    summarySetTypes <- input$displayTypes
    
    summaryStats <- apply(rv$outcomes, 2, function(v){
      v <- v[!is.na(v)]
      return(v %in% summarySetTypes)
    })
  }
  
  output$distPlot <- renderPlot({
    
    if(length(rv$outcomes)==0) { return(NULL) }
    
    whichSummary <- input$reportingType
    summarySetTypes <- input$displayTypes
    
    summaryStats <- apply(rv$outcomes, 2, function(v){
      v <- v[!is.na(v)]
      if(whichSummary == 'number'){
        return(sum(v %in% summarySetTypes))
      } else if(whichSummary == 'percentage'){
        return(sum(v %in% summarySetTypes)/length(v)*100)
      }
    })
    
    data <- data.frame(table(summaryStats))
    colnames(data) <- c("val","freq")
    data$val <- as.numeric(as.character(data$val))
    
    p <- ggplot(data, aes(x=val,y=freq))+ #, fill=val)) +
      geom_bar(stat="identity")+
      labs(y="# of trials\n",x="\nOutcome")+
      #scale_fill_manual(guide=F, limits=b,values=v, drop=F)+
      #scale_x_discrete(breaks = data$val)+
      theme_minimal(base_size=18)
    return(p)
    
    #     if(length(rv$outcomes)==0){ return(NULL) }
    #     
    #     data <- data.frame(table(rv$outcomes))
    #     colnames(data) <- c("val","freq")
    #     
    #     b <- 0:input$numCoins
    #     v <- sapply(b, function(b){
    #       if(input$rangeType == 'inside'){
    #         if(b >= input$range[1] & b <= input$range[2]){
    #           return("red")
    #         } else {
    #           return("black")
    #         }
    #       } else {
    #         if(b >= input$range[1] & b <= input$range[2]){
    #           return("black")
    #         } else {
    #           return("red")
    #         }
    #       }
    #     })
    #     
    #     if(input$numCoins >= 10000){
    #       byval <- 25
    #     } else if(input$numCoins >= 1000){
    #       byval <- 10
    #     } else if(input$numCoins >= 500){
    #       byval <- 5
    #     } else if(input$numCoins >= 100){
    #       byval <- 2
    #     } else {
    #       byval <- 1
    #     }
    #     
    #     lab_breaks <- seq(from=0, to=input$numCoins, by=byval)
    #     
    #     
    #     p <- ggplot(data, aes(x=val,y=freq, fill=val)) +
    #       geom_bar(stat="identity")+
    #       labs(y="# of trials\n",x="\n# of heads in trial")+
    #       scale_fill_manual(guide=F, limits=b,values=v, drop=F)+
    #       scale_x_discrete(breaks=lab_breaks)+
    #       theme_minimal(base_size=18)
    #     return(p)
    
  })
  
    output$simInfo <- renderText({
      if(is.null(dim(rv$outcomes)[2])){
        return("Run a trial to see the results!")
      } else {
        return(paste0("There have been ",dim(rv$outcomes)[2]," runs of the simulation. "))
      }
    })
    
    output$summaryNumItemsMean <- renderText({
      return("Show the mean here")
    })
  
#     output$rangeInfo <- renderText({
#       if(input$rangeType == 'inside'){
#         v <- sum(rv$outcomes >= input$range[1] & rv$outcomes <= input$range[2])
#       } else {
#         v <- sum(rv$outcomes < input$range[1] | rv$outcomes > input$range[2])
#       }
#       p <- v / length(rv$outcomes)*100
#       if(is.nan(p)){
#         return("Run a trial to see the results!")
#       } else {
#         return(paste0("There have been ",dim(rv$outcomes)[2]," runs of the simulation. ",round(p,digits=2),"% of the outcomes meet the selection criteria."))
#       }
#     })
  
  observe({
    if(rv$started){
      disable("urnCount")
      disable("urnName")
      disable("addUrn")
      disable("samplingType")
      disable("sampleSize")
      disable("replacement")
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
      enable("stoppingAmount")
      enable("stoppingType")
      enable("resetUrn")
    }
  })
  
  #   output$evaluationPanel <- renderUI({
  #     maxV <- input$numCoins
  #     qV <- round(maxV / 4)
  #     sliderInput("range",label="of the following range", min=0,max=input$numCoins,step=1,value=c(qV,input$numCoins-qV))
  #   })
  
})
