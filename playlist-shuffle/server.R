
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {
  
  playlist <- read.csv('data/playlist.csv')
  
  newTable <- eventReactive(input$go, {
    if(input$device=='a' || input$device == 'b'){
      samp <- playlist[sample(nrow(playlist), input$songs), ]
    } else if(input$device=='c'){
      # preference for repeating artist
      samp <- playlist[sample(nrow(playlist), 1), ]
      while(nrow(samp) < input$songs){
        targetArtist <- as.character(samp[nrow(samp),]$Artist)
        possibleAdds <- subset(playlist, !Track %in% samp$Track)
        counts <- table(possibleAdds$Artist)
        print(counts)
        nonTargetArtistCount <- nrow(possibleAdds) - counts[names(counts)==targetArtist][[targetArtist]]
        targetArtistCount <- nrow(possibleAdds) - nonTargetArtistCount
        sumProb <- nonTargetArtistCount + targetArtistCount * 4
        probAdd <- sapply(possibleAdds$Artist, function(a){
          if(a==targetArtist){
            return( 4 / sumProb )
          } else {
            return( 1 / sumProb )
          }
        })
        nextSong <- possibleAdds[sample(nrow(possibleAdds), 1, prob = probAdd),]
        samp <- rbind(samp,nextSong)
      }
    } else {
      # preference for changing artists
      samp <- playlist[sample(nrow(playlist), 1), ]
      while(nrow(samp) < input$songs){
        targetArtist <- as.character(samp[nrow(samp),]$Artist)
        possibleAdds <- subset(playlist, !Track %in% samp$Track)
        counts <- table(possibleAdds$Artist)
        print(counts)
        nonTargetArtistCount <- nrow(possibleAdds) - counts[names(counts)==targetArtist][[targetArtist]]
        targetArtistCount <- nrow(possibleAdds) - nonTargetArtistCount
        sumProb <- nonTargetArtistCount * 10 + targetArtistCount
        probAdd <- sapply(possibleAdds$Artist, function(a){
          if(a==targetArtist){
            return( 1 / sumProb )
          } else {
            return( 10 / sumProb )
          }
        })
        nextSong <- possibleAdds[sample(nrow(possibleAdds), 1, prob = probAdd),]
        samp <- rbind(samp,nextSong)
      }
    }
    
    return(samp)
  })

  output$table <- renderTable(newTable(), include.rownames=FALSE)

})
