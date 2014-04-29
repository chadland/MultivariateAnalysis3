library(shiny)
list.of.beer <- readRDS("results.rds")
names(list.of.beer )[1]<-paste("BeerConsumned")
names(list.of.beer )[2]<-paste("BeerRecommended")
names(list.of.beer )[3]<-paste("CorrelationPCA1")

shinyServer(function(input, output) {
  
  output$table1 <- renderTable({ 
    unsorted.recommendations <- list.of.beer[list.of.beer$BeerConsumned==input$var, ]
    sorted.recommendations <- unsorted.recommendations[order(-unsorted.recommendations$CorrelationPCA1),]  
    head(sorted.recommendations[,2:3],5)
  })
})
