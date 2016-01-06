
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

makeList <- function(vec, numA, numB, numC){
  lst <- list(a1 = vec[1:numA], a2 = vec[(numA+1):(numA+numB)], 
              a3 = vec[(numB+numA+1):length(vec)])
  return(lst)
}

calculate.f <- function(lst){
  msw <- (var(lst$a1) * (length(lst$a1) - 1) + var(lst$a2) * (length(lst$a2) - 1) + 
            var(lst$a3) * (length(lst$a3) - 1))/((length(lst$a1) + length(lst$a2) + length(lst$a3)-3))
  msb <- (length(lst$a1)*(mean(lst$a1-mean(unlist(lst))))^2 + length(lst$a2)*(mean(lst$a2-mean(unlist(lst))))^2 +
            length(lst$a3)*(mean(lst$a3-mean(unlist(lst))))^2)/2
  f <- msb/msw
  return(f)
}

calculate.f.fromVec <- function(vec, numA, numB, numC){
  lst <- makeList(vec, numA, numB, numC)
  msw <- (var(lst$a1) * (length(lst$a1) - 1) + var(lst$a2) * (length(lst$a2) - 1) + 
            var(lst$a3) * (length(lst$a3) - 1))/((length(lst$a1) + length(lst$a2) + length(lst$a3)-3))
  msb <- (length(lst$a1)*(mean(lst$a1-mean(unlist(lst))))^2 + length(lst$a2)*(mean(lst$a2-mean(unlist(lst))))^2 +
            length(lst$a3)*(mean(lst$a3-mean(unlist(lst))))^2)/2
  f <- msb/msw
  return(f)
}

shinyServer(function(input, output) {
  
  rv <- reactiveValues(
    outcomes = numeric(),
    swapped = numeric(),
    started = F,
    showResampledData = F,
    newOrder = character()
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
  groupBrv <- reactive({ 
    data <- hot.to.df(input$tblB) 
    vectorData <- data$B
    if(is.null(vectorData)){return(NULL)}
    vectorData <- vectorData[!is.na(vectorData)]
    return(as.numeric(vectorData))
  })
  groupCrv <- reactive({ 
    data <- hot.to.df(input$tblC) 
    vectorData <- data$C
    if(is.null(vectorData)){return(NULL)}
    vectorData <- vectorData[!is.na(vectorData)]
    return(as.numeric(vectorData))
  })
  lastResampleF <- reactive({
    NewGroup <- rv$newOrder
    newList <- list(a1 = NewGroup[1:input$obsA], a2 = NewGroup[(input$obsA+1):(input$obsA+input$obsB)], 
                    a3 = NewGroup[(input$obsB+input$obsA+1):length(NewGroup)])
    f <- round(calculate.f(newList),digits=2)
    return(f)
  })
  observed.f <- reactive({
    dataA <- groupArv()
    dataB <- groupBrv()
    dataC <- groupCrv()
    f <- calculate.f(lst = list(a1 = dataA, a2 = dataB, a3 = dataC))
    return(f)
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
  # hotable for group B
  output$tblB <- renderHotable({
    timesB <- as.numeric(input$obsB)
    if(is.na(timesB) | timesB <1){
      timesB <- 1
    }
    datA <- data.frame(Obs = c(1:timesB), B = as.numeric(c(rep(NA, timesB))))
    return(datA)
  }, readOnly = c(TRUE, FALSE))
  output$tblC <- renderHotable({
    timesC <- as.numeric(input$obsC)
    if(is.na(timesC) | timesC <1){
      timesC <- 1
    }
    datA <- data.frame(Obs = c(1:timesC), C = as.numeric(c(rep(NA, timesC))))
    return(datA)
  }, readOnly = c(TRUE, FALSE))
  
  # table to show the means and mean difference of the groups
  output$observedSummary <- renderText({
    dataA <- groupArv()
    dataB <- groupBrv()
    dataC <- groupCrv()
    GroupA_mean <- mean(dataA)
    GroupB_mean <- mean(dataB)
    GroupC_mean <- mean(dataC)
    f <- observed.f()
    if(!is.nan(GroupA_mean) && !is.nan(GroupB_mean)){
      return(paste0(
        '<p>The observed mean for Group A is ',round(GroupA_mean,digits=2),'.<br>',
        'The observed mean for Group B is ',round(GroupB_mean,digits=2),'.<br>',
        'The observed mean for Group C is ',round(GroupC_mean,digits=2),'.<br>',
        'The observed F statistic is ',round(f,digits=2),'.</p>'
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
    # rv$swapH = c()
  })
  
  observeEvent(input$tblB, {
    rv$outcomes = c()
    rv$started = F
    rv$newOrder = c()
    # rv$swapH = c()
  })
  
  observeEvent(input$tblC, {
    rv$outcomes = c()
    rv$started = F
    rv$newOrder = c()
    # rv$swapH = c()
  })
  
  observeEvent(input$obsA, {
    rv$outcomes = c()
    rv$started = F
    rv$newOrder = c()
    # rv$swapH = c()
  })
  
  observeEvent(input$obsB, {
    rv$outcomes = c()
    rv$started = F
    rv$newOrder = c()
    # rv$swapH = c()
  })
  
  observeEvent(input$obsC, {
    rv$outcomes = c()
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
    #for each resampling it shuffles data and gets the f stat
    rv$started <- T
    dataA <- groupArv()
    dataB <- groupBrv()
    dataC <- groupCrv()
    val <- c(dataA, dataB, dataC)
    orders <- replicate(n, sample(val))
    rv$newOrder <- orders[,n]
    f <- apply(orders, MARGIN = 2, FUN = calculate.f.fromVec, numA = length(dataA), numB = length(dataB),
               numC = length(dataC))
    rv$outcomes <- c(rv$outcomes, f)
  }
  
  # Plot the resampled data points
  output$groupsPlot <- renderPlot({
    As <- groupArv()
    Bs <- groupBrv()
    Cs <- groupCrv()
    Values <- c(As, Bs, Cs)
    nodataflag <- F
    
    # if there's no data, the plot tells them something's funny
    if(length(Values)<3){
      Values <- c(0,0,0)
      nodataflag <- T
    }
    OriginalGroup <- c(rep("A", length(As)), rep("B", length(Bs)), rep("C", length(Cs)))
    if(length(OriginalGroup)<2){
      OriginalGroup <- c("A", "B", "C")
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
    
    mu <- data.frame(grp.mean = c(mean(def[def[,"OriginalGroup"]=="A","Values"]), mean(def[def[,"OriginalGroup"]=="B","Values"]), 
                                  mean(def[def[,"OriginalGroup"]=="C","Values"])),
                     OriginalGroup = c("A", "B", "C"))
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
      ) +
      geom_vline(aes(xintercept=grp.mean), mu,
                 linetype="dashed", color="black")
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
    
    
    if(input$displayType == 'number'){
      rng <- input$range
    } else if(input$displayType == 'percentile'){
      rng <- quantile(rv$outcomes, probs = input$percentile/100, type =1)
    }
    
    freqtable$inrange <- mapply(function(low,high){
      if(rv$showResampledData){
        last <- lastResampleF()
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
    lim <- max(sd(values), max(abs(rv$outcomes)))+1
    if(length(rv$outcomes)>10){
      lim <- max(abs(rv$outcomes))
    }
    
    p <- ggplot(freqtable, aes(x=val,y=freq, fill = inrange)) +
      geom_bar(stat="identity")+
      labs(y="Frequency\n",x="\nF statistic of resampled data")+
      scale_fill_manual(guide=F, values=fillv)+
      #scale_x_discrete(breaks=lab_breaks)+
      theme_minimal(base_size=18) 
      if(length(rv$outcomes)<10){
        p <- p + coord_cartesian(xlim=c(0, lim))
      }
    return(p)
    
  })
  
  output$lastResampleDiff <- renderText({
    if(rv$showResampledData==F){
      return(NULL)
    }
    f <- lastResampleF()
    return(paste0("The resampled F statistic is ", f))
  })
  
  output$resampledData <- renderPlot({
    As <- groupArv()
    Bs <- groupBrv()
    Cs <- groupCrv()
    Values <- c(As, Bs, Cs)
    nodataflag <- F
    
    # if there's no data, the plot tells them something's funny
    if(length(Values)<2){
      Values <- c(0,0,0)
      nodataflag <- T
    }
    OriginalGroup <- c(rep("A", length(As)), rep("B", length(Bs)), rep("C", length(Cs)))
    if(length(OriginalGroup)<2){
      OriginalGroup <- c("A", "B", "C")
    }
    NewGroup <- rv$newOrder
    if(!rv$started){
      NewGroup <- OriginalGroup
    }
    
    # data frame for the plot
    def <- data.frame(Values = NewGroup, OriginalGroup)
    
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
    
    mu <- data.frame(grp.mean = c(mean(def[def[,"OriginalGroup"]=="A","Values"]), mean(def[def[,"OriginalGroup"]=="B","Values"]),
                                  mean(def[def[,"OriginalGroup"]=="C","Values"])), OriginalGroup = c("A", "B", "C"))
    p <- ggplot(def, aes(x = Values))
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
      #scale_fill_discrete(guide=F)+
      theme(legend.position = "bottom", 
            plot.background = element_rect(fill="transparent", colour=NA), 
            panel.background = element_rect(fill="transparent", colour=NA)
      ) +
      geom_vline(aes(xintercept=grp.mean), mu,
                 linetype="dashed", color="black")
    print(p)
  }, bg="transparent")
  
  output$evaluationPanel <- renderUI({
    if(is.null(rv$outcomes)){
      maxV <- 0
      minV <- 0
    } else {
      maxV <- max(ceiling(1+observed.f()),ceiling(max(rv$outcomes)))
      # minV <-floor(min(rv$outcomes))
      minV <- 0
    }
    
    qV <- round(maxV / 2)
    if(is.nan(qV)){
      qV <- 0
    }
    sliderInput("range",label="Select outcomes that are inside the range", min=minV,max=maxV,step=0.01,value=c(minV,minV+qV))
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




