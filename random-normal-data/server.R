
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {
  
  output$data <- renderRHandsontable({
    n <- input$n
    m <- input$m
    sd <- input$sd
    
    if(!is.null(n) && !is.null(m) && !is.null(sd)){
      
      sample <- round(rnorm(n,m,sd))
      df <- data.frame(Value=sample)
      
      rhandsontable(df, readOnly = TRUE)
    } else {
      return(NULL)
    }
  })
  
})
