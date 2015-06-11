library(shiny)

shinyServer(function(input, output, session) {
  
  num <- 0
  flipping <- FALSE
  h_counter <- 0
  t_counter <- 0
  
  doFlip <- function(guess){
    flip <- sample(c("H","T"),1)
    correct <- flip == guess
    num <<- num + 1
    return(list(n=num,guess=guess,result=flip,correct=correct))
  }
  
  f <- reactiveValues(
    flip = NULL, 
    history_data = data.frame(Guess=factor(levels=c("H","T")),Result=factor(levels=c("H","T")),Correct=logical()),
    coinimage = "side.png"
  )
  
  observe({
    if(!is.null(f$flip)){
      if(f$flip$result == "H") { f$coinimage <- "heads.png" }
      if(f$flip$result == "T") { f$coinimage <- "tails.png" }
      new_row <- c(f$flip$guess, f$flip$result, f$flip$correct)
      f$history_data[num,] <- new_row
    }
  })
  
  observe({
    guess <- NULL
    h <- input$heads
    t <- input$tails
    
    if(!flipping){
      f$coinimage <- "side.png"
      flipping <<- T
      invalidateLater(500,session)
    } else {
      if(h > h_counter){
        guess <- "H"
        h_counter <<- h
      } else if(t > t_counter) {
        guess <- "T"
        t_counter <<- t
      }
      if(!is.null(guess)){
        f$flip <- doFlip(guess)
        flipping <<- F
      }
    }
  })
  
  output$coin <- renderUI(HTML(paste0("<img src='",f$coinimage,"' style='width:200px;margin:auto;display:block;'></img>")))
  
  output$correct <- renderText({
    c <- as.character(sum(as.logical(f$history_data$Correct)))
    t <- as.character(length(f$history_data$Correct))
    return(paste(c,"out of",t))
  })
  
  output$history <- renderUI({
    tablestring <- "<table class='data table table-striped table-condensed'><thead><tr>"
    for(i in colnames(f$history_data)){
      tablestring <- paste0(tablestring, "<th>",i,"</th>")
    }
    tablestring <- paste0(tablestring, "</tr></thead><tbody>")
    if(nrow(f$history_data)>0){
      for(i in 1:nrow(f$history_data)){
        tablestring <- paste0(tablestring, "<tr>")
        for(j in f$history_data[i,]){
          tablestring <- paste0(tablestring, "<td>", j, "</td>")
        }
        tablestring <- paste0(tablestring, "</tr>")
      }
    }
    tablestring <- paste0(tablestring, "</tbody></table>")
    return(HTML(tablestring))
  })

})
