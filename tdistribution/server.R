
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
    if(is.na(df) | is.null(df)){ return(NULL) }
    
    rng <- input$range
    if(length(rng)>0){
      if(is.na(rng) | is.null(rng)){
        rng <- 0
      }
    } else {
      rng <- 0
    }

    lim <- abs(qt(.005, df))
    x <- seq(-lim, 0, 0.01)
    x <- c(x, -x[length(x):1])
    d <- dt(x, df)
    data <- data.frame(x = x, d = d)
    # print(length(data$x) - length(data$d)  + 1000)
    data <- rbind(c(-lim, 0), data, c(data[length(data$x), "x"], 0))
    # print(length(data$x) - length(data$d))
    
#     if(rng>lim){
#       rng <- data[length(data$x), "x"] - 0.01
#     } else if(rng<(-lim)){
#       rng <- data[1,"x"] + 0.01
#     }
  
    
    p <- ggplot(data, aes(x=x,y=d)) +
      geom_line(stat="identity")+
      labs(y="Frequency\n",x="\nValue")+
      scale_y_continuous(breaks=NULL) + 
      theme_minimal(base_size=18)+
      xlim(-lim, lim) + geom_polygon(fill = "black")
    if(input$displayType == 'percentile'){
      rng <- qt(input$percentile/100, df)
      p <- p + geom_vline(x = rng, color = 'red', size = 1.5)
    } else if(input$displayType=='number' & rng<lim & rng>(-lim)) {
      if(input$rangeType =='at least as extreme as'){
        if(rng>=0){
          shade <- rbind(c(rng,0), subset(data, x > rng), c(data[nrow(data), "x"], 0))

          p <- p + geom_polygon(data = shade, aes(x=x,y=d),fill = 'red')
          print("plot")
        } else {
          shade <- rbind(c(data[1, "x"], 0), subset(data, x < rng), c(rng,0))
          
          p <- p + geom_polygon(data = shade, aes(x=x,y=d),fill = 'red')
        }
      } else {
        if(rng<0){
          shade <- rbind(c(rng,0), subset(data, x > rng), c(data[nrow(data), "x"], 0))
          
          p <- p + geom_polygon(data = shade, aes(x=x,y=d),fill = 'red')
        } else {
          shade <- rbind(c(data[1, "x"], 0), subset(data, x < rng), c(rng,0))
          
          p <- p + geom_polygon(data = shade, aes(x=x,y=d),fill = 'red')
        }
      }
    }
    return(p)
    
  })
  
  output$rangeInfo <- renderText({
    df <- input$numCoins
    if(is.na(df)){df <- 1}
    if( input$displayType == 'number' & length(input$range)>0){
      if(!is.null(input$range) & !is.na(input$range)){
        if(input$range<0){
          if(input$rangeType == 'at least as extreme as'){
            
            v <- pt(input$range, df)# - pt(input$range[2], df)
          } else {
            v <- 1 - (pt(input$range, df)) #- pt(input$range[2], df))
          }
        } else { 
          if(input$rangeType == 'at least as extreme as'){
            
            v <- pt(-input$range, df)# - pt(input$range[2], df)
          } else {
            v <- 1 - (pt(-input$range, df)) #- pt(input$range[2], df))
          }
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
    
    return(NULL)
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
    numericInput("range", label = "the value", value = 0)
  })
  
})
