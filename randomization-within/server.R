
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)

# Function for getting the means when resampling
getMean <- function(val, order){
  v <- mapply(function(val, flip){
    if(flip){ return(val) }
    else { return(-val) }
  }, val,order)
  m <- mean(v)
  return(m)
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
  dataDiffRV <- reactive({ 
    data <- hot.to.df(input$tblA) 
    if(is.null(data$A)){return(NULL)}
    diff <- data$A - data$B
    diff <- diff[!is.na(diff)]
    return(as.numeric(diff))
  })
  dataAmeanRV <- reactive({
    data <- hot.to.df(input$tblA) 
    if(is.null(data$A)){return(NULL)}
    return(mean(data$A))
  })
  dataBmeanRV <- reactive({
    data <- hot.to.df(input$tblA) 
    if(is.null(data$B)){return(NULL)}
    return(mean(data$B))
  })
  lastResampleDiff <- reactive({
    v <- rv$outcomes[length(rv$outcomes)]
    return(v)
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
    GroupA_mean <- dataAmeanRV()
    GroupB_mean <- dataBmeanRV()
    Difference <- mean(dataDiffRV())
    if(!is.nan(GroupA_mean) && !is.nan(GroupB_mean)){
      return(paste0(
        '<p>The observed mean for Group A is ',round(GroupA_mean,digits=2),'.<br>',
        'The observed mean for Group B is ',round(GroupB_mean,digits=2),'.<br>',
        'The observed difference in means is ',round(Difference,digits=2),'.</p>'
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
    orders <- replicate(n, {return(rbinom(length(diff),1,0.5)==1)})
    rv$newOrder <- orders[,n]
    m <- apply(orders, MARGIN = 2, FUN = getMean, val = val )
    rv$outcomes <- c(rv$outcomes, m)
  }
  
  # Plot the resampled data points
  #   output$groupsPlot <- renderPlot({
  #     As <- groupArv()
  #     Bs <- groupBrv()
  #     Values <- c(As, Bs)
  #     nodataflag <- F
  #     
  #     # if there's no data, the plot tells them something's funny
  #     if(length(Values)<2){
  #       Values <- c(0,0)
  #       nodataflag <- T
  #     }
  #     OriginalGroup <- c(rep("A", length(As)), rep("B", length(Bs)))
  #     if(length(OriginalGroup)<2){
  #       OriginalGroup <- c("A", "B")
  #     }
  #     
  #     # data frame for the plot
  #     def <- data.frame(Values, OriginalGroup)
  #     
  #     # find the size of the biggest bin (if binsize = 1)
  #     limtab <- as.data.frame(table(Values))
  #     yMax <- max(limtab[,"Freq"])
  #     limtab$Values <- as.numeric(limtab$Values)
  #     yRange <- max(max(Values) - min(Values), 1)
  #     yLength <- length(Values)
  #     
  #     # scale the binsize
  #     wid <- (yRange)/(1.5*log(yRange^1.5+1)+(yLength^.5))
  #     
  #     # find the size of the biggest bin (when binsize  = wid)
  #     yVal <- limtab[limtab[,"Freq"]==yMax,"Values"]
  #     yMax2 <- sum(limtab[abs(limtab[,"Values"]-yVal<=wid), "Freq"])
  #     
  #     # scale the (relative) size of the dots
  #     maxRat <- .4/log(yMax2^.05+1)+.6*(1-(1000/((max(Values, 1))+1000)))
  #     
  #     # scale the x axis (otherwise it gets too small when range = 1)
  #     limMin <- min(def$Values)-wid*(yLength)*.4/(log(yRange^.8+1))
  #     limMax <- max(def$Values)+wid*(yLength)*.4/(log(yRange^.8+1))
  #     
  #     mu <- data.frame(grp.mean = c(mean(def[def[,"OriginalGroup"]=="A","Values"]), mean(def[def[,"OriginalGroup"]=="B","Values"])),
  #                      OriginalGroup = c("A", "B"))
  #     p <- ggplot(def, aes(x = Values, fill = OriginalGroup))
  #     if(nodataflag){
  #       p <- p + geom_blank()
  #     } else {
  #       p <- p + geom_dotplot(binwidth=wid, stackgroups = TRUE, dotsize = maxRat, binpositions = "all")
  #     }
  #     p <- p + facet_grid( OriginalGroup ~ .) + 
  #       scale_y_continuous(name = "", breaks = NULL) +
  #       labs(x="\nObserved outcome")+
  #       xlim(limMin, limMax) + 
  #       theme_bw(base_size=14)+
  #       scale_fill_discrete(guide=F)+
  #       theme(legend.position = "bottom", 
  #             plot.background = element_rect(fill="transparent", colour=NA), 
  #             panel.background = element_rect(fill="transparent", colour=NA)
  #       ) +
  #       geom_vline(aes(xintercept=grp.mean), mu,
  #                  linetype="dashed", color="black")
  #     print(p)
  #   }, bg="transparent")
  
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
    
    dataA <- groupArv()
    dataB <- groupBrv()
    values <- c(dataA, dataB)
    if(is.null(values)){
      values <- c(-1, 1)
    }
    lim <- max(sd(values), max(abs(rv$outcomes)))+1
    if(length(rv$outcomes)>10){
      lim <- max(abs(rv$outcomes))+1
    }
    
    p <- ggplot(freqtable, aes(x=val,y=freq, fill = inrange)) +
      geom_bar(stat="identity")+
      labs(y="Frequency\n",x="\nMean difference of resampled data")+
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
    return(paste0("The resampled difference in means is ", diff))
  })
  
  output$resampledData <- renderPlot({
    
    data <- dataDiffRV()
    df <- data.frame(Value=data)
    df$NewGroup <- rv$newOrder
    
    
    # find the size of the biggest bin (if binsize = 1)
    limtab <- as.data.frame(table(data))
    yMax <- max(limtab[,"Freq"])
    limtab$Values <- as.numeric(limtab$Values)
    yRange <- max(max(data) - min(data), 1)
    yLength <- length(data)
    
    # scale the binsize
    wid <- (yRange)/(1.5*log(yRange^1.5+1)+(yLength^.5))
    
    # find the size of the biggest bin (when binsize  = wid)
    yVal <- limtab[limtab[,"Freq"]==yMax,"Values"]
    yMax2 <- sum(limtab[abs(limtab[,"Values"]-yVal<=wid), "Freq"])
    
    # scale the (relative) size of the dots
    maxRat <- .4/log(yMax2^.05+1)+.6*(1-(1000/((max(data, 1))+1000)))
    
    # scale the x axis (otherwise it gets too small when range = 1)
    limMin <- min(data)-wid*(yLength)*.4/(log(yRange^.8+1))
    limMax <- max(data)+wid*(yLength)*.4/(log(yRange^.8+1))
    
    p <- ggplot(df, aes(x = Values, fill = NewGroup))
    if(nodataflag){
      p <- p + geom_blank()
    } else {
      p <- p + geom_dotplot(binwidth=wid, stackgroups = TRUE, dotsize = maxRat, binpositions = "all")
    }
    p <- p + facet_grid( NewGroup ~ .) + 
      scale_y_continuous(name = "", breaks = NULL) +
      labs(x="\nObserved outcome")+
      xlim(limMin, limMax) + 
      theme_bw(base_size=14)+
      scale_fill_discrete(guide=F)+
      theme(legend.position = "bottom", 
            plot.background = element_rect(fill="transparent", colour=NA), 
            panel.background = element_rect(fill="transparent", colour=NA)
      ) +
      geom_vline(xintercept=mean(data), linetype="dashed", color="black")
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
      if(input$rangeType == 'inside'){
        v <- sum(rv$outcomes >= input$range[1] & rv$outcomes <= input$range[2])
      } else {
        v <- sum(rv$outcomes < input$range[1] | rv$outcomes > input$range[2])
      }
      p <- v / length(rv$outcomes)*100
      
      return(paste0("There have been ",length(rv$outcomes)," runs of the simulation. ",round(p,digits=2),"% of the outcomes meet the selection criteria."))
      
    } else if( input$displayType == 'percentile'){
      q <- quantile(rv$outcomes, probs = input$percentile/100, type =1)
      
      return(paste0("There have been ",length(rv$outcomes)," runs of the simulation.",
                    "The ",input$percentile[1]," percentile is ",round(q[[1]], 2)," and the ",input$percentile[2]," percentile is ", round(q[[2]], 2),"."))
    }
    
    
    return(paste0("There have been ",length(rv$outcomes)," runs of the simulation. ",round(p,digits=2),"% of the outcomes meet the selection criteria. ",
                  "The ",input$percentile," percentile is ",q,"."))
    
  })
  
  
})


# getMedian
# possibly put the dotplot in the same part as the histogram, make it only show up if you run one 
# maybe make a dotplot for the original data that just hangs out



