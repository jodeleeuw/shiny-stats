
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)

# Function for getting the means when resampling
getMean <- function(val, order){
  m <- mean(val[order=="A"]) - mean(val[order=="B"])
  return(m)
}

shinyServer(function(input, output) {
  
  rv <- reactiveValues(
    outcomesMeans = numeric(),
    outcomesMedians = numeric(),
    swapped = numeric(),
    started = F,
    showResampledData = F,
    newOrder = numeric()
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
  lastResampleMean <- reactive({
    NewGroup <- rv$newOrder
    diff <- mean(NewGroup)
    return(diff)
  })
  lastResampleMedian <- reactive({
    NewGroup <- rv$newOrder
    diff <- median(NewGroup)
    return(diff)
  })
  
  
  # hotable for group A
  output$tblA <- renderHotable({
    timesA <- as.numeric(input$obsA)
    if(is.na(timesA) | timesA <1){
      timesA <- 1
    }
    datA <- data.frame(Obs = c(1:timesA), A = as.numeric(c(rep(NA, timesA))))
    return(datA)
  }, readOnly = c(TRUE, FALSE))
  
  # table to show the means and mean difference of the groups
  output$observedSummary <- renderText({
    dataA <- groupArv()
    Group_mean <- mean(dataA)
    Group_median <- median(dataA)
    # Difference <- GroupA_mean - GroupB_mean
    if(!is.na(Group_mean) && !is.na(Group_median)){
      return(paste0(
        '<p>The observed mean is ',round(Group_mean,digits=2),'.<br>',
        'The observed median is ',round(Group_median,digits=2),'.</p>'
      ))
    } else {
      return(NULL)
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
    a <- groupArv()
    if(length(a)>0){
      rv$started <- T
      dataA <- groupArv()
      orders <- replicate(n, sample(dataA, replace = TRUE))
      rv$newOrder <- orders[,n]
      means <- apply(orders, MARGIN = 2, FUN = mean )
      medians <- apply(orders, MARGIN = 2, FUN = median )
      rv$outcomesMeans <- c(rv$outcomesMeans, means)
      rv$outcomesMedians <- c(rv$outcomesMedians, medians)
    }
  }
  
  # Plot the resampled data points
  output$groupsPlot <- renderPlot({
    Values <- groupArv()
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
    
    mu <- data.frame(grp.mean = mean(Values), grp.median = median(Values))
    p <- ggplot(def, aes(x = Values))
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
      p <- p + geom_vline(aes(xintercept=grp.mean), mu,
                 linetype="dashed", color="black")
    } else if(input$statistic=="medianStat"){
      p <- p + geom_vline(aes(xintercept=grp.median), mu,
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
        last <- lastResampleMean()
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
    
    values <- groupArv()
    if(is.null(values)){
      values <- c(0)
    }
    limMax <- sd(values)+mean(rv$outcomesMeans)+.5
    limMin <- -sd(values)+mean(rv$outcomesMeans)-.5
    if(length(rv$outcomesMeans)>10){
      limMax <- max((rv$outcomesMeans))+1
      limMin <- min((rv$outcomesMeans))-1
    }
    p <- ggplot(freqtable, aes(x=val,y=freq, fill = inrange)) +
      geom_bar(stat="identity")+
      labs(y="Frequency\n",x="\nBootstrapped mean")+
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
        last <- lastResampleMedian()
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
    
    values <- groupArv()
    if(is.null(values)){
      values <- c(0)
    }
    limMax <- sd(values)+mean(rv$outcomesMedians)+.5
    limMin <- -sd(values)+mean(rv$outcomesMedians)-.5
    if(length(rv$outcomesMedians)>10){
      limMax <- max((rv$outcomesMedians))+1
      limMin <- min((rv$outcomesMedians))-1
    }
    print(freqtable)
    p <- ggplot(freqtable, aes(x=val,y=freq, fill = inrange)) +
      geom_bar(stat="identity")+
      labs(y="Frequency\n",x="\nBootstrapped Median")+
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
      diff <- lastResampleMean()
      return(paste0("The bootstrapped mean is ", round(diff, 2)))
    } else if(input$statistic=="medianStat"){
      if(rv$showResampledData==F){
        return(NULL)
      }
      diff <- lastResampleMedian()
      return(paste0("The bootstrapped median is ", round(diff, 2)))
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
    
    mu <- data.frame(grp.mean = mean(Values), grp.median = median(Values))
    p <- ggplot(def, aes(x = Values))
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
      p <- p + geom_vline(aes(xintercept=grp.mean), mu,
                          linetype="dashed", color="black")
    } else if(input$statistic=="medianStat"){
      p <- p + geom_vline(aes(xintercept=grp.median), mu,
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


# getMedian
# possibly put the dotplot in the same part as the histogram, make it only show up if you run one 
# maybe make a dotplot for the original data that just hangs out



