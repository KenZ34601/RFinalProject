library(shiny)
#install shiny incubator using dev tools
#library(shinyIncubator)

shinyUI(fluidPage(
  
  
  headerPanel("Twitter Sentiment Analysis on SS Candidates"),
  
  # Getting User Inputs
  
  sidebarPanel(
    
    wellPanel(
      textInput("entity1", "Candidate 1: ","Rex Tillerson"),
      textInput ("entity2","Candidate 2: ","Mitt Romney"),
      HTML
      ("<div style='font-size: 10px;font-weight: bold'> Enter the tweet tags that you want '#'</div>")
    )  ,
    wellPanel(
      sliderInput("maxTweets","Number of recent tweets to use for analysis:",min=10,max=1000,value=200,step=1), # The max can, of course, be increased
      actionButton(inputId='actb',icon =icon("twitter"), label="Analyze!")
    )
    
  ),
  
  mainPanel(
    tabsetPanel(
      
      #Output from tab 1 ----So a box plot to see the distribution of scores of sentiments 
      tabPanel("Sentiment Analysis", plotOutput("sentiboxplot"), HTML
               ("<div> This plot shows the distribution of positive/negative sentiments about each candidate. Note that tweets were cleaned before this analysis was performed. For each tweet, a net score of positive and negative sentiments are computed and this plot shows the distribution of scores.A higher sentiment score suggests more positive (or a less negative) discussion of that candidate than the other.</div>"),
               tableOutput("sentiheadtable"),tableOutput("sentitailtable"),id="test"),
      
      #Output from tab 2 - Word clouds - with some html tags
      
      tabPanel("Word Clouds",h2(textOutput("entity1wc")),plotOutput("entity1wcplot"),h2(textOutput("entity2wc")),plotOutput("entity2wcplot")),
      
      #Output from tabs 3 and 4, map of tweets locations
      tabPanel("Tweets Map (US)", plotOutput("tweetmap")),
      #Output from tabs 5 and 6, the raw tweets
      tabPanel("Candidate A Raw tweets",tableOutput("tableentity1")),
      tabPanel("Candidate B Raw tweets",tableOutput("tableentity2"))
    )
  )
  
))