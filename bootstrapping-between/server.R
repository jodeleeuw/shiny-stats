
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)


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
    data <- input$tblA
    if(!is.null(data)){
      data <- hot_to_r(input$tblA)
    } else {
      data <- NULL
    }
    if(is.null(data)){return(NULL)}
    return(data$A[!is.na(data$A)])
  })
  groupBrv <- reactive({ 
    data <- input$tblB
    if(!is.null(data)){
      data <- hot_to_r(input$tblB)
    } else {
      data <- NULL
    }
    if(is.null(data)){return(NULL)}
    return(data$B[!is.na(data$B)])
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
    timesB <- as.numeric(input$obsB)
    if(is.na(timesB) | timesB <1){
      timesB <- 1
    }
    datB <- data.frame(Obs = c(1:timesB), B = as.numeric(c(rep(NA, timesB))))
    #height argument makes the column height proportional to number of obs + header
    #maxRows makes sure they can't paste in data longer than the number of obs
    rhandsontable(datB,rowHeaders=F, contextMenu=F, height = timesB*23 + 27, maxRows = timesB) %>%
      hot_col(col = "B", copyable = TRUE) %>% #make sure command+v works
      hot_col(col = "Obs", readOnly = TRUE)   #make sure they can't edit observation numbers
  })
  
  # table to show the means and mean difference of the groups
  output$observedSummary <- renderText({
    if(input$statistic=="meanStat"){
      dataA <- groupArv()
      dataB <- groupBrv()
      GroupA_mean <- mean(dataA)
      GroupB_mean <- mean(dataB)
      Difference <- GroupA_mean - GroupB_mean
      if(!is.nan(GroupA_mean) && !is.nan(GroupB_mean) && !is.na(GroupA_mean) && !is.na(GroupB_mean)){
        return(paste0(
          '<p>The observed mean for Group A is ',round(GroupA_mean,digits=2),'.<br>',
          'The observed mean for Group B is ',round(GroupB_mean,digits=2),'.<br>',
          'The observed difference in means is ',round(Difference,digits=2),'.</p>'
        ))
      } else {
        return(NULL)
      }
    } else {
      dataA <- groupArv()
      dataB <- groupBrv()
      GroupA_median <- median(dataA)
      GroupB_median <- median(dataB)
      Difference <- GroupA_median - GroupB_median
      #Need different if conditions b/c mean(NULL) gives NA, but median(NULL) gives NULL
      if(!is.null(GroupA_median) && !is.null(GroupB_median) && !is.na(GroupA_median) && !is.na(GroupB_median)){
        return(paste0(
          '<p>The observed median for Group A is ',round(GroupA_median,digits=2),'.<br>',
          'The observed median for Group B is ',round(GroupB_median,digits=2),'.<br>',
          'The observed difference in medians is ',round(Difference,digits=2),'.</p>'
        ))
      } else {
        return(NULL)
      }
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
    a <- groupArv()
    b <- groupBrv()
    if(length(a)>0){
      rv$started <- T
      dataA <- groupArv()
      dataB <- groupBrv()
      newA <- replicate(n, sample(dataA, replace = TRUE))
      rv$newA <- newA[,n]
      newB <- replicate(n, sample(dataB, replace = TRUE))
      rv$newB <- newB[,n]
      meansA <- apply(newA, MARGIN = 2, FUN = mean )
      mediansA <- apply(newA, MARGIN = 2, FUN = median )
      meansB <- apply(newB, MARGIN = 2, FUN = mean )
      mediansB <- apply(newB, MARGIN = 2, FUN = median )
      means <- meansA - meansB
      medians <- mediansA - mediansB
      rv$outcomesMeans <- c(rv$outcomesMeans, means)
      rv$outcomesMedians <- c(rv$outcomesMedians, medians)
    }
  }
  
  # Plot the original data points
  output$groupsPlot <- renderPlot({
    As <- groupArv()
    Bs <- groupBrv()
    Values <- c(As, Bs)
    nodataflag <- F
    
    # if there's no data, the plot tells them something's funny
    if(length(Values)<2){
      Values <- c(0,0)
      nodataflag <- T
    }
    OriginalGroup <- c(rep("A", length(As)), rep("B", length(Bs)))
    if(length(OriginalGroup)<2){
      OriginalGroup <- c("A", "B")
    }
    
    # data frame for the plot
    def <- data.frame(Values, OriginalGroup)
    
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
    
    mu <- data.frame(grp.mean = c(mean(def[def[,"OriginalGroup"]=="A","Values"]), 
                                  mean(def[def[,"OriginalGroup"]=="B","Values"])),
                     OriginalGroup = c("A", "B"), 
                     grp.median = c(median(def[def[,"OriginalGroup"]=="A","Values"]),
                                    median(def[def[,"OriginalGroup"]=="B","Values"])))
    p <- ggplot(def, aes(x = Values, fill = OriginalGroup))
    if(nodataflag){
      p <- p + geom_blank()
    } else {
      p <- p + geom_dotplot(binwidth=wid, stackgroups = TRUE, dotsize = maxRat, binpositions = "all")
    }
    p <- p + facet_grid( OriginalGroup ~ .) + 
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
        last <- lastResampleMeanDiff()
        if(last%%1==0){
          last <- last-0.01
        }
        if(last >= low & last <=high){
          return("deepskyblue2")
        }
      }
      if(low >= rng[1] & high < rng[2]){
        return("red")
      } else {
        return("black")
      }
      
    }, freqtable$min, freqtable$max)
    
    freqtable$inrange <- as.factor(freqtable$inrange)
    
    fillv <- levels(freqtable$inrange)
    As <- groupArv()
    Bs <- groupBrv()
    values <- c(As, Bs)
    if(is.null(values)){
      values <- c(0)
    }
    limMax <- mean(As) - mean(Bs) + sd(values)
    limMax <- max(limMax, max((rv$outcomesMeans))+1)
    limMin <- mean(As) - mean(Bs) - sd(values)
    limMin <- min(limMin, min((rv$outcomesMeans))-1)
    if(length(rv$outcomesMeans)>10){
      limMax <- max((rv$outcomesMeans))+1
      limMin <- min((rv$outcomesMeans))-1
    }
    p <- ggplot(freqtable, aes(x=val,y=freq, fill = inrange)) +
      geom_bar(stat="identity")+
      labs(y="Frequency\n",x="\nBootstrapped difference of means")+
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
      if(low >= rng[1] & high < rng[2]){
        return("red")
      } else {
        return("black")
      }
    }, freqtable$min, freqtable$max)
    
    freqtable$inrange <- as.factor(freqtable$inrange)
    
    fillv <- levels(freqtable$inrange)
    As <- groupArv()
    Bs <- groupBrv()
    values <- c(As, Bs)
    if(is.null(values)){
      values <- c(0)
    }
    limMax <- median(As) - median(Bs) + sd(values)
    limMax <- max(limMax, max((rv$outcomesMedians))+1)
    limMin <- median(As) - median(Bs) - sd(values)
    limMin <- min(limMin, min((rv$outcomesMedians))-1)
    if(length(rv$outcomesMedians)>10){
      limMax <- max((rv$outcomesMedians))+1
      limMin <- min((rv$outcomesMedians))-1
    }

    p <- ggplot(freqtable, aes(x=val,y=freq, fill = inrange)) +
      geom_bar(stat="identity")+
      labs(y="Frequency\n",x="\nBootstrapped difference of medians")+
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
      return(paste0("The bootstrapped difference of means is ", round(diff, 2)))
    } else if(input$statistic=="medianStat"){
      if(rv$showResampledData==F){
        return(NULL)
      }
      diff <- lastResampleMedianDiff()
      return(paste0("The bootstrapped difference of medians is ", round(diff, 2)))
    }
  })
  
  output$resampledData <- renderPlot({
    As <- rv$newA
    Bs <- rv$newB
    Values <- c(As, Bs)
    nodataflag <- F

    # if there's no data, the plot tells them something's funny
    if(length(Values)<1){
      Values <- c(0)
      nodataflag <- T
    }
    
    # data frame for the plot
    def <- data.frame(Values, Group = c(rep("A", length(As)), rep("B", length(Bs))))
    
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
    
    mu <- data.frame(grp.mean = c(mean(As), mean(Bs)), grp.median =c(median(As), median(Bs)), Group = c("A", "B"))
    p <- ggplot(def, aes(x = Values, color = Group))
    if(nodataflag){
      p <- p + geom_blank()
    } else {
      p <- p + geom_dotplot(binwidth=wid, stackgroups = TRUE, dotsize = maxRat, binpositions = "all")
    }
    p <- p + facet_grid(Group ~ .) + 
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
    
    
    qV <- round((maxV - minV) / 4)
    if(is.nan(qV)){
      qV <- 0
    }
    sliderInput("range",label="Select outcomes that are inside the range", min=minV,max=maxV,step=0.01,value=c(minV+qV,maxV-qV))
  })
  
  output$rangeInfo <- renderText({
    if( length(rv$outcomesMeans) == 0 ) { return("Run the simulation to see the result!") }
    if(input$statistic=="meanStat"){
      if( input$displayType == 'number' ){

        v <- sum(rv$outcomesMeans >= input$range[1] & rv$outcomesMeans <= input$range[2])
        
        return(paste0("There have been ",length(rv$outcomesMeans)," runs of the simulation. ",v," of the outcomes are between ", input$range[1]," and ", input$range[2],"."))
        
      } else if( input$displayType == 'percentile'){
        q <- quantile(rv$outcomesMeans, probs = input$percentile/100, type =1)
        
        return(paste0("There have been ",length(rv$outcomesMeans)," runs of the simulation.",
                      "The ",input$percentile[1]," percentile is ",round(q[[1]], 2)," and the ",input$percentile[2]," percentile is ", round(q[[2]], 2),"."))
      }
    } else if (input$statistic=="medianStat"){
      if( input$displayType == 'number' ){
        
        v <- sum(rv$outcomesMedians >= input$range[1] & rv$outcomesMedians <= input$range[2])
        
        return(paste0("There have been ",length(rv$outcomesMedians)," runs of the simulation. ",v," of the outcomes are between ", input$range[1]," and ", input$range[2],"."))
        
      } else if( input$displayType == 'percentile'){
        q <- quantile(rv$outcomesMedians, probs = input$percentile/100, type =1)
        
        return(paste0("There have been ",length(rv$outcomesMedians)," runs of the simulation.",
                      "The ",input$percentile[1]," percentile is ",round(q[[1]], 2)," and the ",input$percentile[2]," percentile is ", round(q[[2]], 2),"."))
      }
    }
    
    
    return(paste0("There have been ",length(rv$outcomes)," runs of the simulation. ",v," of the outcomes are between ", input$range[1]," and ", input$range[2],".",
                  "The ",input$percentile," percentile is ",q,"."))
    
  })
  
  
})

