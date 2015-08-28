
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
      checkboxGroupInput('displayTypes','Which Types',choices)
    }
  })
  
  doSample <- function(times){
    rv$started <- T
    df <- rv$types
    df$Type <- as.character(df$Type)
    
    # TODO: DON'T USE PROB for COUNT. MAKE FULL SET
    
    if(input$samplingType == 'fixed'){
      s <- replicate(times, sample(df$Type, input$sampleSize, prob=df$Count))
    }
    if(input$samplingType == 'conditional'){
      s <- replicate(times, function(){
        samp <- sample(df$Type, 1, prob=df$Count)
        while(sum(samp==input$samplingType) < input$stoppingAmount){
          samp <- c(samp, sample(df$Type, 1, prob=df$Count))
        }
        return(samp)
      })
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
    print(rv$outcomes)
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
  
#   output$rangeInfo <- renderText({
#     if(input$rangeType == 'inside'){
#       v <- sum(rv$outcomes >= input$range[1] & rv$outcomes <= input$range[2])
#     } else {
#       v <- sum(rv$outcomes < input$range[1] | rv$outcomes > input$range[2])
#     }
#     p <- v / length(rv$outcomes)*100
#     if(is.nan(p)){
#       return("Flip some coins to see the result!")
#     } else {
#       return(paste0("There have been ",length(rv$outcomes)," runs of the simulation. ",round(p,digits=2),"% of the outcomes meet the selection criteria."))
#     }
#   })
  
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
    }
  })
  
#   output$evaluationPanel <- renderUI({
#     maxV <- input$numCoins
#     qV <- round(maxV / 4)
#     sliderInput("range",label="of the following range", min=0,max=input$numCoins,step=1,value=c(qV,input$numCoins-qV))
#   })
  
})
