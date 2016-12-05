
library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Sentiment Analysis"),
  
  textOutput("currentTime"),
  h4("Tweets:"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      dataTableOutput('tweets_table')
      
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      sidebarPanel(
        plotOutput("positive_wordcloud")
      ),
      sidebarPanel(
        plotOutput("negative_wordcloud")
      ),
      sidebarPanel(
        plotOutput("neutral_wordcloud")
      )
    )
    
  )
))