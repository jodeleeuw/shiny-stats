
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
    outcomes = numeric(),
    swapped = numeric(),
    started = F,
    showResampledData = F,
    newOrder = character()
    # swapH = numeric() 
  )
  
  
  # store As and Bs in reactive expression, so that they only update once.
  groupArv <- reactive({
    tbl <- input$tblA
    if(!is.null(tbl)){
    data <- hot_to_r(input$tblA)
    vectorData <- data$A
    } else {
      vectorData <- NULL
    }
    if(is.null(vectorData)){return(NULL)}
    vectorData <- vectorData[!is.na(vectorData)]
    return(as.numeric(vectorData))
  })
  groupBrv <- reactive({ 
    tbl <- input$tblB
    if(!is.null(tbl)){
      data <- hot_to_r(input$tblB)
      vectorData <- data$B
    } else {
      vectorData <- NULL
    }
    if(is.null(vectorData)){return(NULL)}
    vectorData <- vectorData[!is.na(vectorData)]
    return(as.numeric(vectorData))
  })
  lastResampleDiff <- reactive({
    As <- groupArv()
    Bs <- groupBrv()
    Values <- c(As, Bs)
    NewGroup <- rv$newOrder
    diff <- round(mean(Values[NewGroup=="A"]) - mean(Values[NewGroup=="B"]),digits=2)
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
    datA <- data.frame(Obs = c(1:timesB), B = as.numeric(c(rep(NA, timesB))))
    rhandsontable(datA,rowHeaders=F, contextMenu=F, height = timesB*23+27, maxRows = timesB) %>%
      hot_col(col = "B", copyable = TRUE) %>%
      hot_col(col = "Obs", readOnly = TRUE)
  })
  
  # table to show the means and mean difference of the groups
  output$observedSummary <- renderText({
    dataA <- groupArv()
    # print(dataA)
    dataB <- groupBrv()
    if(is.null(dataA)|is.null(dataB)){return("Enter some data to see a summary.")}
    GroupA_mean <- mean(dataA)
    GroupB_mean <- mean(dataB)
    Difference <- GroupA_mean - GroupB_mean
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
    dataA <- groupArv()
    dataB <- groupBrv()
    val <- c(dataA, dataB)
    group <- c(rep("A", length(dataA)), rep("B", length(dataB)))
    orders <- replicate(n, sample(group))
    rv$newOrder <- orders[,n]
    m <- apply(orders, MARGIN = 2, FUN = getMean, val = val )
    rv$outcomes <- c(rv$outcomes, m)
  }
  
  # Plot the resampled data points
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
    if(yRange<=20){
      wid <- 1
    }
    
    # find the size of the biggest bin (when binsize  = wid)
    yVal <- limtab[limtab[,"Freq"]==yMax,"Values"]
    yMax2 <- sum(limtab[abs(limtab[,"Values"]-yVal<=wid), "Freq"])
    
    # scale the (relative) size of the dots
    maxRat <- .4/log(yMax2^.05+1)+.6*(1-(1000/((max(Values, 1))+1000)))
    if(wid==1){
      maxRat <- 1
    }
    
    # scale the x axis (otherwise it gets too small when range = 1)
    limMin <- min(def$Values)-wid*(yLength)*.4/(log(yRange^.8+1))
    limMax <- max(def$Values)+wid*(yLength)*.4/(log(yRange^.8+1))
    
    mu <- data.frame(grp.mean = c(mean(def[def[,"OriginalGroup"]=="A","Values"]), mean(def[def[,"OriginalGroup"]=="B","Values"])),
                     OriginalGroup = c("A", "B"))
    p <- ggplot(def, aes(x = Values, fill = OriginalGroup))
    if(nodataflag){
      p <- p + geom_blank()
    } else {
      p <- p + geom_dotplot(binwidth=wid, stackgroups = TRUE, method = "histodot",
                            dotsize = maxRat, binpositions = "all")
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
    lim <- max(sd(values), max(abs(rv$outcomes)))+1
    if(length(rv$outcomes)>10){
      lim <- max(abs(rv$outcomes))+1
    }
    
    p <- ggplot(freqtable, aes(x=val,y=freq, fill = inrange)) +
      geom_bar(stat="identity")+
      labs(y="Frequency\n",x="\nDifference in means of resampled data")+
      scale_fill_manual(guide=F, values=fillv)+
      scale_x_continuous(expand = c(0, 0), breaks = scales::pretty_breaks()) +
      theme_minimal(base_size=18) +
      scale_y_continuous(expand = c(0, 0)) +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            # panel.grid.major.y = element_line(color = "white"),
            plot.background = element_blank(),
            panel.ontop = FALSE) + 
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
    NewGroup <- rv$newOrder
    if(!rv$started){
      NewGroup <- OriginalGroup
    }
    
    # data frame for the plot
    def <- data.frame(Values, OriginalGroup, NewGroup)
    
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
    
    mu <- data.frame(grp.mean = c(mean(def[def[,"NewGroup"]=="A","Values"]), mean(def[def[,"NewGroup"]=="B","Values"])),
                     NewGroup = c("A", "B"))
    p <- ggplot(def, aes(x = Values, fill = OriginalGroup))
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
      geom_vline(aes(xintercept=grp.mean), mu,
                 linetype="dashed", color="black")
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
    
    if(!is.null(rv$outcomes) & (length(rv$outcomes)<10)){
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
    
    # Get observed difference to make sure sliders include it
    dataA <- groupArv()
    dataB <- groupBrv()
    if(is.null(dataA)|is.null(dataB)){
      observedDiff <- 0
    } else { 
      GroupA_mean <- mean(dataA)
      GroupB_mean <- mean(dataB)
      observedDiff <- GroupA_mean - GroupB_mean
    }
    sliderInput("range",label="Select outcomes that are inside the range", 
                min=round(min(minV, observedDiff), 2),max=round(max(maxV, observedDiff), 2),
                step=0.01,value=c(minV+qV,maxV-qV))
  })
  
  output$rangeInfo <- renderText({
    if( length(rv$outcomes) == 0 ) { return("Run the simulation to see the result!") }
    
    if( input$displayType == 'number' ){
      v <- sum(rv$outcomes >= input$range[1] & rv$outcomes <= input$range[2])

      return(paste0("There have been ",length(rv$outcomes)," runs of the simulation. ",v," of the outcomes are between ", input$range[1]," and ", input$range[2],"."))
      
    } else if( input$displayType == 'percentile'){
      q <- quantile(rv$outcomes, probs = input$percentile/100, type =1)
      
      return(paste0("There have been ",length(rv$outcomes)," runs of the simulation.",
                    "The ",input$percentile[1]," percentile is ",round(q[[1]], 2)," and the ",input$percentile[2]," percentile is ", round(q[[2]], 2),"."))
    }
    
    
    return(paste0("There have been ",length(rv$outcomes)," runs of the simulation. ",v," of the outcomes are between ", input$range[1]," and ", input$range[2],".",
                  "The ",input$percentile," percentile is ",q,"."))
    
  })
  
  
})


# getMedian
# possibly put the dotplot in the same part as the histogram, make it only show up if you run one 
# maybe make a dotplot for the original data that just hangs out



