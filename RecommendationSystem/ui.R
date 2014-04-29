library(shiny)
list.of.beer <- readRDS("results.rds")
beer.vector <- as.vector(unique(list.of.beer$beer1))

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("Beer Recommendation App"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select a beer that you like and recommendations will be listed"),
      
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = sort(beer.vector),
                  selected = "60 Minute IPA")
  
      
    ),
    
    mainPanel(
      tableOutput("table1")
      )
  )
))