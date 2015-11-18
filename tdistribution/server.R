
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
#library(shinyjs)
library(ggplot2)

shinyServer(function(input, output, session) {
  
#   doFlip <- function(nCoins, p, times){
#     outcome <- rbinom(times, nCoins, p)
#     return(outcome)
#   }
#   
#   rv <- reactiveValues(
#     outcomes = numeric(),
#     started = F
#   )
#   
#   observeEvent(input$flip1, {
#     runFlips(1)
#   })
#   
#   observeEvent(input$flip10, {
#     runFlips(10)
#   })
#   
#   observeEvent(input$flip100, {
#     runFlips(100)
#   })
#   
#   observeEvent(input$flip1000, {
#     runFlips(1000)
#   })
#   
#   observe({
#     x <- input$probHeads
#     if(is.na(x)){ return() }
#     if(x > 1) {
#       updateNumericInput(session, 'probHeads',value=1)
#     } else if(x < 0){
#       updateNumericInput(session, 'probHeads',value=0)
#     }
#   })
#   
#   runFlips <- function(n){
#     rv$started <- T
#     o <- doFlip(input$numCoins, input$probHeads, n)
#     rv$outcomes <- c(rv$outcomes, o)
#   }
#   
#   observeEvent(input$reset, {
#     rv$started <- F
#     rv$outcomes <- numeric()
#   })
  
  output$distPlot <- renderPlot({
    df <- input$numCoins
    if(is.na(df)){ return(NULL) }
    
    x <- seq(-40, 40, 0.1)
    d <- dt(x, df)
    data <- data.frame(x = x, d = d)
    colnames(data) <- c("val","freq")
    data$val <- as.numeric(as.character(data$val))
    data$freq <- as.numeric(as.character(data$freq))
    
    if(input$displayType == 'number'){
      rng <- input$range
      if(is.null(rng)){return(NULL)}
      if(is.na(rng)){return(NULL)}
    } else if(input$displayType == 'percentile'){
      # rng <- quantile(rv$outcomes, probs = input$percentile/100, type =1)
      rng <- qt(p = input$percentile/100, df)
    }
    
    data$inrange <- sapply(data$val, function(b){
      if(input$rangeType == 'inside'){
        if(b <= rng ){
          return("red")
        } else {
          return("black")
        }
      } else {
        if(b <= rng){
          return("black")
        } else {
          return("red")
        }
      }
    })
    
    data$inrange <- as.factor(data$inrange)
    
    fillv <- levels(data$inrange)
    lim <- abs(qt(.01, df))
    p <- ggplot(data, aes(x=val,y=freq, fill=inrange)) +
      geom_bar(stat="identity")+
      labs(y="Value\n",x="\nFrequency")+
      scale_fill_manual(guide=F, values=fillv)+
      #scale_x_discrete(breaks=lab_breaks)+
      theme_minimal(base_size=18)+
      xlim(-lim, lim)
    return(p)
    
  })
  
  output$rangeInfo <- renderText({
    df <- input$numCoins
    if(is.na(df)){df <- 1}
    if( input$displayType == 'number' ){
      if(!is.null(input$range)){
        if(input$rangeType == 'inside'){
          
  
          v <- pt(input$range, df)# - pt(input$range[2], df)
        } else {
          v <- 1 - (pt(input$range, df)) #- pt(input$range[2], df))
        }
      } else {v <- 0 }
      p <- v*100
      if(is.na(p)){p <- 0}
      
      return(paste0("With ",df," degrees of freedom, ",round(p,digits=2),"% of the distribution falls within the selected range."))
      
    } else if( input$displayType == 'percentile'){
      q <- round(qt(p = input$percentile/100, df),2)
      
#       return(paste0("With ",df," degrees of freedom, ",
#                     "The ",input$percentile[1]," percentile is ",q[1]," and the ",input$percentile[2]," percentile is ",q[2],"."))
      return(paste0("With ",df," degrees of freedom, ",
                    "The ",input$percentile[1]," percentile is ",q,"."))
    }
    
    
    return(paste0("There have been ",length(rv$outcomes)," runs of the simulation. ",round(p,digits=2),"% of the outcomes meet the selection criteria. ",
                  "The ",input$percentile," percentile is ",q,"."))
    
  })
  
#   observe({
#     if(rv$started){
#       disable("numCoins")
#       disable("probHeads")
#     }
#     if(!rv$started){
#       enable("numCoins")
#       enable("probHeads")
#     }
#   })
  
  output$evaluationPanel <- renderUI({
#     maxV <- input$numCoins
#     qV <- round(maxV / 4)
    # sliderInput("range",label="the range", min=-40,max=40,step=.5,value=c(-5,5))
    numericInput("range", label = "the range from negative infinity to", value = 0)
  })
  
})
