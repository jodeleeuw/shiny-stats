
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)

# Function for getting the means when resampling
getCor <- function(val, order){
  c <- cor(val, order)
  return(c)
}

shinyServer(function(input, output) {
  
  rv <- reactiveValues(
    outcomes = numeric(),
    swapped = numeric(),
    started = F,
    showResampledData = F,
    newOrder = character()
  )
  
  # store As and Bs in reactive expression, so that they only update once.
  groupArv <- reactive({ 
    data <- input$tblA
    if(!is.null(data)){
      data <- hot_to_r(input$tblA)
    } else {
      data <- NULL
    }
    if(is.null(data)){return(NULL)} 
    vectorData <- data$A
    if(is.null(vectorData)){return(NULL)}
    vectorData <- vectorData[!is.na(vectorData)]
    return(as.numeric(vectorData))
  })
  groupBrv <- reactive({ 
    data <- input$tblB
    if(!is.null(data)){
      data <- hot_to_r(input$tblB)
    } else {
      data <- NULL
    }
    if(is.null(data)){return(NULL)}
    vectorData <- data$B
    if(is.null(vectorData)){return(NULL)}
    vectorData <- vectorData[!is.na(vectorData)]
    return(as.numeric(vectorData))
  })
  lastResampleDiff <- reactive({
    As <- groupArv()
    Values <- c(As)
    NewGroup <- rv$newOrder
    diff <- round(cor(Values, NewGroup),digits=2)
    return(diff)
  })
  
  # hotable for group A
  output$tblA <- renderRHandsontable({
    timesA <- as.numeric(input$obsA)
    if(is.na(timesA) | timesA <1){
      timesA <- 1
    }
    datA <- data.frame(Obs = c(1:timesA), A = as.numeric(c(rep(NA, timesA))))
    #height argument makes the column height proportional to number of obs + header
    #maxRows makes sure they can't paste in data longer than the number of obs
    rhandsontable(datA,rowHeaders=F, contextMenu=F, height = timesA*23 + 27, maxRows = timesA) %>%
      hot_col(col = "A", copyable = TRUE) %>% #make sure command+v works
      hot_col(col = "Obs", readOnly = TRUE)   #make sure they can't edit observation numbers
  })
  # hotable for group B
  output$tblB <- renderRHandsontable({
    timesB <- as.numeric(input$obsA)
    if(is.na(timesB) | timesB <1){
      timesB <- 1
    }
    datA <- data.frame(Obs = c(1:timesB), B = as.numeric(c(rep(NA, timesB))))
    #height argument makes the column height proportional to number of obs + header
    #maxRows makes sure they can't paste in data longer than the number of obs
    rhandsontable(datA,rowHeaders=F, contextMenu=F, height = timesB*23 + 27, maxRows = timesB) %>%
      hot_col(col = "B", copyable = TRUE) %>% #make sure command+v works
      hot_col(col = "Obs", readOnly = TRUE)   #make sure they can't edit observation numbers
  })
  
  # table to show the means and mean difference of the groups
  output$observedSummary <- renderText({
    dataA <- groupArv()
    dataB <- groupBrv()
    
    if(length(dataA)==length(dataB) && length(dataA)>1){
      corrAB <- cor(dataA, dataB)
    } else {
      corrAB <- NA
    }
    if(!is.na(corrAB) ){
      return(paste0(
        '<p>The observed correlation between A and B is ',round(corrAB,digits=2),'.</p>'
      ))
    } else {
      return(NULL)
    }
  })
  
  # reset things if they enter new data  
  observeEvent(input$tblA, {
    rv$outcomes = c()
    rv$started = F
    rv$newOrder = c()
  })
  
  observeEvent(input$tblB, {
    rv$outcomes = c()
    rv$started = F
    rv$newOrder = c()
  })
  
  observeEvent(input$obsA, {
    rv$outcomes = c()
    rv$started = F
    rv$newOrder = c()
  })
  
  observeEvent(input$obsB, {
    rv$outcomes = c()
    rv$started = F
    rv$newOrder = c()
  })
  
  # run the resampler
  observeEvent(input$flip1, {
    rv$showResampledData <- T
    resample(1)
  })
  
  observeEvent(input$flip10, {
    rv$showResampledData <- F
    resample(10)
  })
  
  observeEvent(input$flip100, {
    rv$showResampledData <- F
    resample(100)
  })
  
  observeEvent(input$flip1000, {
    rv$showResampledData <- F
    resample(1000)
  })
  
  observeEvent(input$flip10000, {
    rv$showResampledData <- F
    resample(10000)
  })
  
  #### method for resampling ####
  resample <- function(n){
    #for each resampling it shuffles data and gets the mean difference
    rv$started <- T
    dataA <- groupArv()
    dataB <- groupBrv()
    orders <- replicate(n, sample(dataB))
    rv$newOrder <- orders[,n]
    m <- apply(orders, MARGIN = 2, FUN = getCor, val = dataA )
    rv$outcomes <- c(rv$outcomes, m)
  }
  
  # Plot the resampled data points
  output$groupsPlot <- renderPlot({
    As <- groupArv()
    Bs <- groupBrv()
    nodataflag <- F
    
    # if there's no data, the plot tells them something's funny
    if(length(As)<2 | length(Bs)<2 | (length(As)!=length(Bs))){
      matV <- matrix(0, ncol = 2)
      nodataflag <- T
    } else {
      matV <- matrix(c(As, Bs), nrow = length(As), ncol = 2)
    }
    
    # data frame for the plot
    def <- data.frame(A = matV[,1], B = matV[,2])
    
    p <- ggplot(def, aes(x = A, y = B))
    if(nodataflag){
      p <- p + geom_blank()
    } else {
      p <- p + geom_point()
    }
    p <- p + 
      labs(x="\nVariable A", y="\nVariable B")+
      theme_bw(base_size=14)+
      stat_smooth(se = FALSE, method = "lm")
    theme(legend.position = "bottom", 
          plot.background = element_rect(fill="transparent", colour=NA), 
          panel.background = element_rect(fill="transparent", colour=NA)
    )
    print(p)
  }, bg="transparent")
  
  output$plotArea <- renderUI({
    if(rv$showResampledData && rv$started){
      return(
        fluidRow(
          column(4,
                 h3('Last resample'),
                 plotOutput("resampledData"),
                 textOutput("lastResampleDiff")
          ),
          column(8,
                 h3('All samples'),
                 plotOutput("distPlot")
          )
        )
      )
    } else {
      return(plotOutput("distPlot"))
    }
  })
  
  # plot the histogram of means
  output$distPlot <- renderPlot({
    if(length(rv$outcomes)==0){ return(NULL) }
    h <- hist(rv$outcomes)
    freqtable <- data.frame(val=h$mids,freq=h$counts,min=h$breaks[1:(length(h$breaks)-1)],max=h$breaks[2:length(h$breaks)])
    
    #if(anyNA(data)){
    #  data <- data.frame(val = c(0), freq = c(0))
    #}
    
    if(input$displayType == 'number'){
      rng <- input$range
    } else if(input$displayType == 'percentile'){
      rng <- quantile(rv$outcomes, probs = input$percentile/100, type =1)
    }
    
    freqtable$inrange <- mapply(function(low,high){
      if(rv$showResampledData){
        last <- lastResampleDiff()
        if(last >= low & last < high){
          return("deepskyblue2")
        }
      }
      if(low >= rng[1] & high <= rng[2]){
        return("red")
      } else {
        return("black")
      }
      
    }, freqtable$min, freqtable$max)
    
    freqtable$inrange <- as.factor(freqtable$inrange)
    
    fillv <- levels(freqtable$inrange)
    
    dataA <- groupArv()
    dataB <- groupBrv()
    values <- c(dataA, dataB)
    if(is.null(values)){
      values <- c(-1, 1)
    }
    lim <- max(sd(values), max(abs(rv$outcomes)))+.5
    if(length(rv$outcomes)>10){
      lim <- max(abs(rv$outcomes))+.5
    }
    
    p <- ggplot(freqtable, aes(x=val,y=freq, fill = inrange)) +
      geom_bar(stat="identity")+
      labs(y="Frequency\n",x="\nCorrelations in resampled data")+
      scale_fill_manual(guide=F, values=fillv)+
      #scale_x_discrete(breaks=lab_breaks)+
      theme_minimal(base_size=18) + 
      coord_cartesian(xlim=c(-lim, lim))
    return(p)
    
  })
  
  output$lastResampleDiff <- renderText({
    if(rv$showResampledData==F){
      return(NULL)
    }
    diff <- lastResampleDiff()
    return(paste0("The resampled correlation ", diff))
  })
  
  output$resampledData <- renderPlot({
    As <- groupArv()
    Bs <- rv$newOrder
    
    nodataflag <- F
    
    # if there's no data, the plot tells them something's funny
    if(length(As)<2 | length(Bs)<2 | (length(As)!=length(Bs))){
      matV <- matrix(0, ncol = 2)
      nodataflag <- T
    } else {
      matV <- matrix(c(As, Bs), nrow = length(As), ncol = 2)
    }
    
    # data frame for the plot
    def <- data.frame(A = matV[,1], B = matV[,2])
    
    p <- ggplot(def, aes(x = A, y = B))
    if(nodataflag){
      p <- p + geom_blank()
    } else {
      p <- p + geom_point()
    }
    p <- p + 
      labs(x="\nVariable A", y="\nVariable B")+
      theme_bw(base_size=14)+
      stat_smooth(se = FALSE, method = "lm")
    theme(legend.position = "bottom", 
          plot.background = element_rect(fill="transparent", colour=NA), 
          panel.background = element_rect(fill="transparent", colour=NA)
    )
    print(p)
  }, bg="transparent")
  
  output$evaluationPanel <- renderUI({
    if(is.null(rv$outcomes)){
      maxV <- 0
      minV <- 0
    } else {
      maxV <- ceiling(max(rv$outcomes))
      minV <-floor(min(rv$outcomes))
    }
    
    if(length(rv$outcomes)<10){
      if(abs(minV)>=abs(maxV)){
        maxV <- -minV
      } else {
        minV <- -maxV
      }
    }
    qV <- round(maxV / 2)
    if(is.nan(qV)){
      qV <- 0
    }
    sliderInput("range",label="the range", min=minV,max=maxV,step=0.01,value=c(minV+qV,maxV-qV))
  })
  
  output$rangeInfo <- renderText({
    if( length(rv$outcomes) == 0 ) { return("Run the simulation to see the result!") }
    
    if( input$displayType == 'number' ){
      v <- sum(rv$outcomes >= input$range[1] & rv$outcomes <= input$range[2])
      
      return(paste0("There have been ",length(rv$outcomes)," runs of the simulation. ",v," of the outcomes are between ", input$range[1], " and ", input$range[2],"."))
      
    } else if( input$displayType == 'percentile'){
      q <- quantile(rv$outcomes, probs = input$percentile/100, type =1)
      
      return(paste0("There have been ",length(rv$outcomes)," runs of the simulation.",
                    "The ",input$percentile[1]," percentile is ",round(q[[1]], 2)," and the ",input$percentile[2]," percentile is ", round(q[[2]], 2),"."))
    }
    
    
    return(paste0("There have been ",length(rv$outcomes)," runs of the simulation. ",v," of the outcomes are between ", input$range[1], " and ", input$range[2],". ",
                  "The ",input$percentile," percentile is ",q,"."))
    
  })
  
  
})




