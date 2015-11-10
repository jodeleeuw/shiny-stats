
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)

getMean <- function(val, order){
  v <- mapply(function(val, flip){
    if(!flip){ return(val) }
    else { return(-val) }
  }, val,order)
  m <- mean(v)
  return(m)
}

getMedian <- function(val, order){
  v <- mapply(function(val, flip){
    if(!flip){ return(val) }
    else { return(-val) }
  }, val,order)
  m <- median(v)
  return(m)
}


shinyServer(function(input, output) {
  
  rv <- reactiveValues(
    outcomesMeans = numeric(),
    outcomesMedians = numeric(),
    newA = numeric(),
    newB = numeric(),
    swapped = numeric(),
    started = F,
    showResampledData = F
    # swapH = numeric() 
  )
  
  # store As and Bs in reactive expression, so that they only update once.
  groupArv <- reactive({ 
    data <- hot.to.df(input$tblA) 
    vectorData <- data$A
    if(is.null(vectorData)){return(NULL)}
    vectorData <- vectorData[!is.na(vectorData)]
    return(as.numeric(vectorData))
  })

  lastResampleMeanDiff <- reactive({
    means <- rv$outcomesMeans
    diff <- round(means[length(means)],digits=2)
    return(diff)
  })
  lastResampleMedianDiff <- reactive({
    medians <- rv$outcomesMedians
    diff <- round(medians[length(medians)],digits=2)
    return(diff)
  })
  
  getDataTable <- reactive({
    data <- hot.to.df(input$tblA)
    return(data)
  })
  
  dataDiffRV <- reactive({ 
    data <- getDataTable()
    if(is.null(data)){ return(NULL) }
    #if(is.null(data$A)){return(NULL)}
    diff <- data$A - data$B
    diff <- diff[!is.na(diff)]
    return((as.numeric(diff)))
  })
  dataAmeanRV <- reactive({
    data <- getDataTable()
    if(is.null(data)){ return(NULL) }
    return(mean(data$A))
  })
  dataBmeanRV <- reactive({
    data <- getDataTable()
    if(is.null(data)){ return(NULL) }
    return(mean(data$B))
  })
  
  dataAmedianRV <- reactive({
    data <- getDataTable()
    if(is.null(data)){ return(NULL) }
    return(median(data$A))
  })
  dataBmedianRV <- reactive({
    data <- getDataTable()
    if(is.null(data)){ return(NULL) }
    return(median(data$B))
  })

  
  
  # hotable for group A
  output$tblA <- renderHotable({
    timesA <- as.numeric(input$obsA)
    if(is.na(timesA) | timesA <1){
      timesA <- 1
    }
    datA <- data.frame(Subject = c(1:timesA), A = as.numeric(c(rep(NA, timesA))), B = as.numeric(c(rep(NA, timesA))))
    return(datA)
  }, readOnly = c(TRUE, FALSE, FALSE))
  
  # table to show the means and mean difference of the groups
  output$observedSummary <- renderText({
    data <- dataDiffRV()
    if(length(data[!is.na(data)])<2) {return(NULL)}
    
    if(input$statistic=="meanStat"){
      Amean <- dataAmeanRV()
      Bmean <- dataBmeanRV()
      Dmean <- dataDiffRV()
        return(paste0(
          '<p>The observed mean for condition A is ',round(Amean,digits=2),'.<br>',
          'The observed mean for condition B is ',round(Bmean,digits=2),'.<br>',
          'The observed mean difference is ',round(mean(Dmean),digits=2),'.</p>'
        ))
    } else {
      Amed <- dataAmedianRV()
      Bmed <- dataBmedianRV()
      Dmed <- dataDiffRV()
        return(paste0(
          '<p>The observed median for condition A is ',round(Amed,digits=2),'.<br>',
          'The observed median for condition B is ',round(Bmed,digits=2),'.<br>',
          'The observed median difference is ',round(median(Dmed),digits=2),'.</p>'
        ))
    }
  })
  
  
  # reset things if they enter new data  
  observeEvent(input$tblA, {
    rv$outcomesMeans = c()
    rv$outcomesMedians = c()
    rv$started = F
    rv$newOrder = c()
    # rv$swapH = c()
  })
  
  observeEvent(input$obsA, {
    rv$outcomesMeans = c()
    rv$outcomesMedians = c()
    rv$started = F
    rv$newOrder = c()
    # rv$swapH = c()
  })
  
  observeEvent(input$tblB, {
    rv$outcomesMeans = c()
    rv$outcomesMedians = c()
    rv$started = F
    rv$newOrder = c()
    # rv$swapH = c()
  })
  
  observeEvent(input$obsB, {
    rv$outcomesMeans = c()
    rv$outcomesMedians = c()
    rv$started = F
    rv$newOrder = c()
    # rv$swapH = c()
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
      diff <- dataDiffRV()
      orders <- replicate(n, sample(diff, replace = TRUE))
      rv$newOrder <- orders[,n]
      means <- apply(orders, MARGIN = 2, FUN = mean)
      medians <- apply(orders, MARGIN = 2, FUN = median)
      rv$outcomesMeans <- c(rv$outcomesMeans, means)
      rv$outcomesMedians <- c(rv$outcomesMedians, medians)
    }
  
  
  # Plot the original data points
  output$groupsPlot <- renderPlot({
    Values <- dataDiffRV()
    nodataflag <- F
    
    # if there's no data, the plot tells them something's funny
    if(length(Values)<2){
      Values <- c(0,0)
      nodataflag <- T
    }
  
    # data frame for the plot
    def <- data.frame(Values)
    
    
    if(nodataflag){
      limMin <- -10
      limMax <- 10
    } else {
      # find the size of the biggest bin (if binsize = 1)
      limtab <- as.data.frame(table(Values))
      yMax <- max(limtab[,"Freq"])
      limtab$Values <- as.numeric(limtab$Values)
      yRange <- max(max(Values) - min(Values), 1)
      yLength <- length(Values)
      
      # scale the binsize
      wid <- (yRange)/(1.5*log(yRange^1.5+1)+(yLength^.5))
      
      # find the size of the biggest bin (when binsize  = wid)
      yVal <- limtab[limtab[,"Freq"]==yMax,"Values"]
      yMax2 <- sum(limtab[abs(limtab[,"Values"]-yVal<=wid), "Freq"])
      
      # scale the (relative) size of the dots
      maxRat <- .4/log(yMax2^.05+1)+.6*(1-(1000/((max(Values, 1))+1000)))
      
      # scale the x axis (otherwise it gets too small when range = 1)
      limMin <- min(def$Values)-wid*(yLength)*.4/(log(yRange^.8+1))
      limMax <- max(def$Values)+wid*(yLength)*.4/(log(yRange^.8+1))
    }
    
    p <- ggplot(def, aes(x = Values, fill = 'A'))
    if(nodataflag){
      p <- p + geom_blank()
    } else {
      p <- p + geom_dotplot(binwidth=wid, stackgroups = TRUE, dotsize = maxRat, binpositions = "all")
    }
    p <- p +
      scale_y_continuous(name = "", breaks = NULL) +
      labs(x="\nObserved outcome")+
      xlim(limMin, limMax) + 
      theme_bw(base_size=14)+
      scale_fill_discrete(guide=F)+
      theme(legend.position = "bottom", 
            plot.background = element_rect(fill="transparent", colour=NA), 
            panel.background = element_rect(fill="transparent", colour=NA)
      )
    if(input$statistic=="meanStat"){
      p <- p + geom_vline(xintercept=mean(Values), 
                          linetype="dashed", color="black")
    } else if(input$statistic=="medianStat"){
      p <- p + geom_vline(xintercept=median(Values), mu,
                          linetype="dashed", color="black")
    }
    print(p)
  }, bg="transparent")
  
  output$plotArea <- renderUI({
    if(input$statistic=="meanStat"){
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
                   plotOutput("distPlotMean")
            )
          )
        )
      } else {
        return(plotOutput("distPlotMean"))
      }
    }else if (input$statistic=="medianStat"){
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
                   plotOutput("distPlotMedian")
            )
          )
        )
      } else {
        return(plotOutput("distPlotMedian"))
      }
    }
  })
  
  
  # plot the histogram of means
  output$distPlotMean <- renderPlot({
    if(length(rv$outcomesMeans)==0){ return(NULL) }
    h <- hist(rv$outcomesMeans)
    freqtable <- data.frame(val=h$mids,freq=h$counts,min=h$breaks[1:(length(h$breaks)-1)],max=h$breaks[2:length(h$breaks)])
    
    if(length(rv$outcomesMeans)<10){
      # freqtable <- data.frame(table(round(rv$outcomesMeans)))
      # freqtable <- data.frame(val=freqtable$Var1,freq=freqtable$Freq,min=min(rv$outcomesMeans) - .5,max=max(rv$outcomesMeans) + .5)
      h <- hist(rv$outcomesMeans, breaks = seq(from =floor(min(rv$outcomesMeans)-.5), to = ceiling(max(rv$outcomesMeans)+.5), by = 1))
      freqtable <- data.frame(val=h$mids,freq=h$counts,min=h$breaks[1:(length(h$breaks)-1)],max=h$breaks[2:length(h$breaks)])
      freqtable <- freqtable[freqtable[,"freq"]>0,]
    }
    
    if(input$displayType == 'number'){
      rng <- input$range
    } else if(input$displayType == 'percentile'){
      rng <- quantile(rv$outcomesMeans, probs = input$percentile/100, type =1)
    }
    
    freqtable$inrange <- mapply(function(low,high){
      if(rv$showResampledData){
        last <- lastResampleMeanDiff()
        if(last%%1==0){
          last <- last-0.01
        }
        if(last >= low & last <=high){
          return("deepskyblue2")
        }
      }
      if(input$rangeType == 'inside'){
        if(low >= rng[1] & high < rng[2]){
          return("red")
        } else {
          return("black")
        }
      } else {
        if(high >= rng[1] & low < rng[2]){
          return("black")
        } else {
          return("red")
        }
      }
    }, freqtable$min, freqtable$max)
    
    freqtable$inrange <- as.factor(freqtable$inrange)
    
    fillv <- levels(freqtable$inrange)
    
    data <- getDataTable()
    diff <- dataDiffRV()
    limMax <- mean(diff) + sd(diff)
    limMax <- max(limMax, max((rv$outcomesMeans))+1)
    limMin <- mean(diff) + sd(diff)
    limMin <- min(limMin, min((rv$outcomesMeans))-1)
    if(length(rv$outcomesMeans)>10){
      limMax <- max((rv$outcomesMeans))+1
      limMin <- min((rv$outcomesMeans))-1
    }
    
    p <- ggplot(freqtable, aes(x=val,y=freq, fill = inrange)) +
      geom_bar(stat="identity")+
      labs(y="Frequency\n",x="\nMean difference of bootstrapped data")+
      scale_fill_manual(guide=F, values=fillv)+
      #scale_x_discrete(breaks=lab_breaks)+
      theme_minimal(base_size=18) + 
      coord_cartesian(xlim=c(limMin, limMax))
    return(p)
    
  })
  
  # plot the histogram of Medians
  output$distPlotMedian <- renderPlot({
    if(length(rv$outcomesMedians)==0){ return(NULL) }
    h <- hist(rv$outcomesMedians)
    freqtable <- data.frame(val=h$mids,freq=h$counts,min=h$breaks[1:(length(h$breaks)-1)],max=h$breaks[2:length(h$breaks)])
    
    if(length(rv$outcomesMedians)<10){
      # freqtable <- data.frame(table(round(rv$outcomesMedians)))
      # freqtable <- data.frame(val=freqtable$Var1,freq=freqtable$Freq,min=min(rv$outcomesMedians) - .5,max=max(rv$outcomesMedians) + .5)
      h <- hist(rv$outcomesMedians, breaks = seq(from =floor(min(rv$outcomesMedians)-.5), to = ceiling(max(rv$outcomesMedians)+.5), by = 1))
      freqtable <- data.frame(val=h$mids,freq=h$counts,min=h$breaks[1:(length(h$breaks)-1)],max=h$breaks[2:length(h$breaks)])
      
    }
    
    freqtable <- freqtable[freqtable[,"freq"]>0,]
    
    if(input$displayType == 'number'){
      rng <- input$range
    } else if(input$displayType == 'percentile'){
      rng <- quantile(rv$outcomesMedians, probs = input$percentile/100, type =1)
    }
    
    freqtable$inrange <- mapply(function(low,high){
      if(rv$showResampledData){
        last <- lastResampleMedianDiff()
        if(last%%1==0){
          last <- last-0.01
        }
        if(last >= low & last <=high){
          return("deepskyblue2")
        }
      }
      if(input$rangeType == 'inside'){
        if(low >= rng[1] & high < rng[2]){
          return("red")
        } else {
          return("black")
        }
      } else {
        if(high >= rng[1] & low < rng[2]){
          return("black")
        } else {
          return("red")
        }
      }
    }, freqtable$min, freqtable$max)
    
    freqtable$inrange <- as.factor(freqtable$inrange)
    
    fillv <- levels(freqtable$inrange)
    
    data <- getDataTable()
    diff <- dataDiffRV()
    limMax <- median(diff) + sd(diff)
    limMax <- max(limMax, max((rv$outcomesMedians))+1)
    limMin <- median(diff) + sd(diff)
    limMin <- min(limMin, min((rv$outcomesMedians))-1)
    if(length(rv$outcomesMedians)>10){
      limMax <- max((rv$outcomesMedians))+1
      limMin <- min((rv$outcomesMedians))-1
    }
    
    
    p <- ggplot(freqtable, aes(x=val,y=freq, fill = inrange)) +
      geom_bar(stat="identity")+
      labs(y="Frequency\n",x="\nMedian difference of bootstrapped data")+
      scale_fill_manual(guide=F, values=fillv)+
      #scale_x_discrete(breaks=lab_breaks)+
      theme_minimal(base_size=18) + 
      coord_cartesian(xlim=c(limMin, limMax))
    return(p)
    
  })
  
  
  output$lastResampleDiff <- renderText({
    if(input$statistic=="meanStat"){
      if(rv$showResampledData==F){
        return(NULL)
      }
      diff <- lastResampleMeanDiff()
      return(paste0("The bootstrapped mean difference is ", round(diff, 2)))
    } else if(input$statistic=="medianStat"){
      if(rv$showResampledData==F){
        return(NULL)
      }
      diff <- lastResampleMedianDiff()
      return(paste0("The bootstrapped median difference is ", round(diff, 2)))
    }
  })
  
  output$resampledData <- renderPlot({
    
    Values <- rv$newOrder
    nodataflag <- F
    
    # if there's no data, the plot tells them something's funny
    if(length(Values)<1){
      Values <- c(0)
      nodataflag <- T
    }
    
    # data frame for the plot
    def <- data.frame(Values)
  
    # find the size of the biggest bin (if binsize = 1)
    limtab <- as.data.frame(table(Values))
    yMax <- max(limtab[,"Freq"])
    limtab$Values <- as.numeric(limtab$Values)
    yRange <- max(max(Values) - min(Values), 1)
    yLength <- length(Values)
    
    # scale the binsize
    wid <- (yRange)/(1.5*log(yRange^1.5+1)+(yLength^.5))
    
    # find the size of the biggest bin (when binsize  = wid)
    yVal <- limtab[limtab[,"Freq"]==yMax,"Values"]
    yMax2 <- sum(limtab[abs(limtab[,"Values"]-yVal<=wid), "Freq"])
    
    # scale the (relative) size of the dots
    maxRat <- .4/log(yMax2^.05+1)+.6*(1-(1000/((max(Values, 1))+1000)))
    
    # scale the x axis (otherwise it gets too small when range = 1)
    limMin <- min(def$Values)-wid*(yLength)*.4/(log(yRange^.8+1))
    limMax <- max(def$Values)+wid*(yLength)*.4/(log(yRange^.8+1))
    
    p <- ggplot(def, aes(x = Values))
    if(nodataflag){
      p <- p + geom_blank()
    } else {
      p <- p + geom_dotplot(binwidth=wid, stackgroups = TRUE, dotsize = maxRat, binpositions = "all")
    }
    p <- p + scale_y_continuous(name = "", breaks = NULL) +
      labs(x="\nObserved outcome")+
      xlim(limMin, limMax) + 
      theme_bw(base_size=14)+
      scale_fill_discrete(guide=F)+
      theme(legend.position = "bottom", 
            plot.background = element_rect(fill="transparent", colour=NA), 
            panel.background = element_rect(fill="transparent", colour=NA)
      )
    if(input$statistic=="meanStat"){
      p <- p + geom_vline(xintercept=mean(Values),
                          linetype="dashed", color="black")
    } else if(input$statistic=="medianStat"){
      p <- p + geom_vline(xintercept=median(Values),
                          linetype="dashed", color="black")
    }
    print(p)
  }, bg="transparent")
  
  output$evaluationPanel <- renderUI({
    if(is.null(rv$outcomesMeans)){
      maxV <- 0
      minV <- 0
    } else {
      maxV <- ceiling(max(max(rv$outcomesMeans),(max(rv$outcomesMedians))))
      minV <-floor(min(min(rv$outcomesMeans),min(rv$outcomesMedians)))
    }
    
    
    qV <- round(maxV / 2)
    if(is.nan(qV)){
      qV <- 0
    }
    sliderInput("range",label="the range", min=minV,max=maxV,step=0.01,value=c(minV+qV,maxV-qV))
  })
  
  output$rangeInfo <- renderText({
    if( length(rv$outcomesMeans) == 0 ) { return("Run the simulation to see the result!") }
    if(input$statistic=="meanStat"){
      if( input$displayType == 'number' ){
        if(input$rangeType == 'inside'){
          v <- sum(rv$outcomesMeans >= input$range[1] & rv$outcomesMeans <= input$range[2])
        } else {
          v <- sum(rv$outcomesMeans < input$range[1] | rv$outcomesMeans > input$range[2])
        }
        p <- v / length(rv$outcomesMeans)*100
        
        return(paste0("There have been ",length(rv$outcomesMeans)," runs of the simulation. ",round(p,digits=2),"% of the outcomes meet the selection criteria."))
        
      } else if( input$displayType == 'percentile'){
        q <- quantile(rv$outcomesMeans, probs = input$percentile/100, type =1)
        
        return(paste0("There have been ",length(rv$outcomesMeans)," runs of the simulation.",
                      "The ",input$percentile[1]," percentile is ",round(q[[1]], 2)," and the ",input$percentile[2]," percentile is ", round(q[[2]], 2),"."))
      }
    } else if (input$statistic=="medianStat"){
      if( input$displayType == 'number' ){
        if(input$rangeType == 'inside'){
          v <- sum(rv$outcomesMedians >= input$range[1] & rv$outcomesMedians <= input$range[2])
        } else {
          v <- sum(rv$outcomesMedians < input$range[1] | rv$outcomesMedians > input$range[2])
        }
        p <- v / length(rv$outcomesMedians)*100
        
        return(paste0("There have been ",length(rv$outcomesMedians)," runs of the simulation. ",round(p,digits=2),"% of the outcomes meet the selection criteria."))
        
      } else if( input$displayType == 'percentile'){
        q <- quantile(rv$outcomesMedians, probs = input$percentile/100, type =1)
        
        return(paste0("There have been ",length(rv$outcomesMedians)," runs of the simulation.",
                      "The ",input$percentile[1]," percentile is ",round(q[[1]], 2)," and the ",input$percentile[2]," percentile is ", round(q[[2]], 2),"."))
      }
    }
    
    
    return(paste0("There have been ",length(rv$outcomesMeans)," runs of the simulation. ",round(p,digits=2),"% of the outcomes meet the selection criteria. ",
                  "The ",input$percentile," percentile is ",q,"."))
    
  })
  
  
})

