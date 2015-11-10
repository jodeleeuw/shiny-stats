
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(rvest)
library(quanteda)
library(jsonlite)


shinyServer(function(input, output) {
  
  rv <- reactiveValues(
    currentText = NULL,
    currentGrade = NULL,
    originalText = NULL,
    originalGrade = NULL
  )
  
  observeEvent(input$run,{
    url <- input$url
    spliturl <- strsplit(url, '/')[[1]]
    title <- spliturl[length(spliturl)]
    #xmlurl <- paste0("https://en.wikipedia.org/w/api.php?format=xml&action=query&prop=extracts&titles=",title,"&redirects=true", encoding="UTF-8")
    jsonurl <- paste0("https://en.wikipedia.org/w/api.php?format=json&action=query&prop=extracts&titles=",title,"&redirects=true", encoding="UTF-8")
    text <- fromJSON(jsonurl)
    text <- text$query$pages[[1]]$extract
    
    #text <- read_xml(xmlurl) %>% xml_node("extract") %>% xml_text() %>% read_html() %>% html_text()
    #text <- gsub("\n", " ", text)
    text <- read_html(text) %>% html_text()
    
    rv$currentText <- text
    
    rv$currentGrade <- readability(text, measure=c("Coleman.Liau.grade", "FORCAST.RGL"),drop=F)
    
    revurl <- paste0("https://en.wikipedia.org/w/api.php?action=query&prop=revisions&format=json&rvprop=content&rvlimit=1&rvparse=&rvdir=newer&titles=",title)
    oldtext <- fromJSON(revurl)
    oldtext <- oldtext$query$pages[[1]]$revisions$`*`
    oldtext <- read_html(oldtext) %>% html_text()
    
    rv$originalText <- oldtext
    
    rv$originalGrade <- readability(oldtext, measure=c("Coleman.Liau.grade", "FORCAST.RGL"),drop=F)
  })
  
  output$currentGrade <- renderUI({
    if(!is.null(rv$currentGrade)){
      return(wellPanel(
        p(paste0("The Coleman-Liau grade level for the text below is ",round(rv$currentGrade$Coleman.Liau.grade,digits=2),".")),
        p(paste0("The FORCAST grade level for the text below is ",round(rv$currentGrade$FORCAST.RGL, digits=2),"."))
      ))
    }
  })
  
  output$currentText <- renderText({
    rv$currentText
  })
  
  output$originalGrade <- renderUI({
    if(!is.null(rv$originalGrade)){
      return(wellPanel(
        p(paste0("The Coleman-Liau grade level for the text below is ",round(rv$originalGrade$Coleman.Liau.grade,digits=2),".")),
        p(paste0("The FORCAST grade level for the text below is ",round(rv$originalGrade$FORCAST.RGL, digits=2),"."))
      ))
    }
  })
  
  output$originalText <- renderText({
    rv$originalText
  })
  
})

## https://en.wikipedia.org/w/api.php?action=query&titles=42&prop=revisions&rvdir=newer&rvlimit=1

## get by revision
### https://en.wikipedia.org/w/api.php?format=xml&action=query&prop=extracts&revids=300107
