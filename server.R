#install all libraries
#install.packages("ggmap",twitteR","stringr","ROAuth", "ply","dplyr", "reshape", "tm", "RJSONIO", "wordcloud", "gridExtra", "shiny")
#load libraries
library(twitteR)
library(stringr)
library(ROAuth)
library(plyr)
library(dplyr)
library(ggplot2)
library(reshape)
library(tm)
library(RJSONIO)
library(wordcloud)
library(grid)
library(gridExtra)
#library(shinyIncubator)
library(shiny)
library(maps)
library(ggmap)

# Courtesy to aruizga who provides the framework for sentiment analysis
# https://github.com/aruizga7/Twitter-Bluemix-R-Shiny-App

# Function to create a data frame from tweets
shinyServer(function(input, output,session) {
  #uncomment to load key and secret to connect to API for the first time.
  api_Key <-  "9a9x556XRs6fdMe7NvHOTuiGp"
  api_Secret <- "YXZUrjk34kNpMmqwUGRGx6BMatkRoL3AEXIJ1zVBqewl7JV8Be"
  access_token <- "93328549-52Jw22wcjJv3lansCqDR9sHzaUHJBfQXMbld3rjyV"
  access_token_secret <- "FG09fF2MVSnG5GKtiV9rLfdDBigcXuPZ4virNnjiiXpXA"
  twitCred <- setup_twitter_oauth(api_Key, api_Secret, access_token, access_token_secret)
  save(twitCred, file = "my_oauth.Rdata")
  twitCred<- NULL
  load("my_oauth.Rdata")
  # Function to clean tweets
  CleanTweets <- function(tweets)
  {
    tweets = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets)
    tweets = gsub("@\\w+", "", tweets)
    tweets = gsub("[[:punct:]]", "", tweets)
    tweets = gsub("[[:digit:]]", "", tweets)
    tweets = gsub("http\\w+", "", tweets)
    tweets = gsub("[ \t]{2,}", "", tweets)
    tweets = gsub("^\\s+|\\s+$", "", tweets)
    tweets = gsub("amp", "", tweets)
    # define "tolower error handling" function
    try.tolower = function(x)
    {
      y = NA
      try_error = tryCatch(tolower(x), error=function(e) e)
      if (!inherits(try_error, "error"))
        y = tolower(x)
      return(y)
    }
    
    tweets = sapply(tweets, try.tolower)
    tweets = tweets[tweets != ""]
    names(tweets) = NULL
    return(tweets)
  }

  #Search tweets and create a data frame 
  TweetFrame<-function(searchTerm, maxTweets)
  {
    twtList<-searchTwitter(
      searchTerm,
      n=maxTweets,
      retryOnRateLimit=10,
      lang="en"
      )
    twtList1<- do.call("rbind",lapply(twtList,as.data.frame))
    twtList1$text<-iconv(twtList1$text, 'UTF-8', 'ASCII') #WILL THIS SOLVE THE UTF ENCODING PROBLEM: http://lists.hexdump.org/pipermail/twitter-users-hexdump.org/2013-May/000335.html
    # twtList1$longitude <- as.numeric(twtList1$longitude)
    # twtList1$latitude <- as.numeric(twtlist1$latitude)
    return(twtList1)
    
  }
  
  # function to calculate number of tweets (input is text column, if the entire data frame was submitted, 
  #could've used nrow(), as done at a different place below)
  
  numoftweets<-function(entity1,entity2,entity1entry,entity2entry){
    ent1numtweets<-nrow(entity1)
    ent2numtweets<-nrow(entity2)
    notweets<-c(ent1numtweets,ent2numtweets)
    names(notweets)<-c(entity1entry,entity2entry)
    notweets
  } 
  
  
  # function for word cloud 
  
  wordcloudentity<-function(entitycleantext)
  {
    tweetCorpus<-Corpus(VectorSource(CleanTweets(entitycleantext)))
    tweetTDM<-TermDocumentMatrix(tweetCorpus,control=list(removePunctuation=TRUE,
                                                          stopwords=c(stopwords('english')),
                                                          removeNumbers=TRUE,tolower=TRUE))
    tdMatrix <- as.matrix(tweetTDM) # creating a data matrix
    sortedMatrix<-sort(rowSums(tdMatrix),decreasing=TRUE) # calculate row sum of each term and sort in descending order (high freq to low)
    cloudFrame<-data.frame(word=names(sortedMatrix),freq=sortedMatrix)#extracting names from named list in prev command and binding together into a dataframe with frequencies - called cloudFrame, names in separate columns
    
    wcloudentity<-wordcloud(cloudFrame$word,cloudFrame$freq,max.words=100, colors=brewer.pal(8,"Dark2"),scale=c(8,1), random.order=TRUE)
    print(wcloudentity)
  }
  
  # Scoring sentiment expressed - Breen's algorithm
  # Jeffrey Breen: http://jeffreybreen.wordpress.com/2011/07/04/twitter-text-mining-r-slides/ 
  # via Gaston Sanchez's twitter mining project: https://sites.google.com/site/miningtwitter/questions/sentiment/analysis   
  
  score.sentiment = function(sentences, pos.words, neg.words)
  {
    
    # we got a vector of sentences. plyr will handle a list
    # or a vector as an "l" for us
    # we want a simple array ("a") of scores back, so we use 
    # "l" + "a" + "ply" = "laply":
    scores = laply(sentences, function(sentence, pos.words, neg.words) {
      
      # clean up sentences with R's regex-driven global substitute, gsub():
      sentence = gsub('[[:punct:]]', '', sentence)
      sentence = gsub('[[:cntrl:]]', '', sentence)
      sentence = gsub('\\d+', '', sentence)
      sentence = gsub('\\}', '', sentence)
      # and convert to lower case:
      sentence = tolower(sentence)
      
      # split into words. str_split is in the stringr package
      word.list = str_split(sentence, '\\s+')
      # sometimes a list() is one level of hierarchy too much
      words = unlist(word.list)
      
      # compare our words to the dictionaries of positive & negative terms
      pos.matches = match(words, pos.words, nomatch=NA_integer_)
      neg.matches = match(words, neg.words, nomatch=NA_integer_)
      
      # match() returns the position of the matched term or NA
      # we just want a TRUE/FALSE:
      pos.matches = !is.na(pos.matches)
      neg.matches = !is.na(neg.matches)
      
      # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
      score = sum(pos.matches) - sum(neg.matches)
      
      return(score)
    }, pos.words, neg.words)
    
    scores.df = data.frame(score=scores, text=sentences, size=seq(length(scores)))
    return(scores.df)
  }
  
  #calling the above sentiment scoring function, the text of tweets serve as inputs
  
  sentimentalanalysis<-function(entity1text,entity2text,entity1entry,entity2entry){
    
    # A compiled list of words expressing positive and negative sentiments ----
    #http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html
    # List of words and additional information on the original source from Jeffrey Breen's github site at:
    #https://github.com/jeffreybreen/twitter-sentiment-analysis-tutorial-201107/tree/master/data/opinion-lexicon-English
    
    positivewords=readLines("positive_words.txt")
    negativewords=readLines("negative_words.txt")
    
    #Applying score.sentiment algorithm to cleaned tweets and getting data frames of tweets, net sentiment score for a tweet 
    #(number of positive sentiments minus negative sentiments)
    
    entity1score = score.sentiment(CleanTweets(entity1text),positivewords,negativewords)
    entity2score = score.sentiment(CleanTweets(entity2text),positivewords,negativewords)
    
    # Adding a dummy variable useful for a ggplot
    entity1score$entity = entity1entry
    entity2score$entity = entity2entry

    #combine all of this
    entityscores<-rbind(entity1score,entity2score)
    
  }
  # twitterMap <- function(searchTerm,maxTweets){
  #   #radius default=100
  #   #lat<-runif(n=maxTweets,min=24.446667, max=49.384472)
  #   #long<-runif(n=maxTweets,min=-124.733056, max=-66.949778)
  #   
  #   #generate data fram with random longitude, latitude and chosen radius
  #   coordinates<-as.data.frame(cbind(lat,long))
  #   coordinates$lat<-lat
  #   coordinates$long<-long
  #   #create a string of the lat, long, and radius for entry into searchTwitter()
  #   for(i in 1:length(coordinates$lat)){
  #     coordinates$search.twitter.entry[i]<-toString(c(coordinates$lat[i],
  #                                                     coordinates$long[i]))
  #   }
  #   # take out spaces in the string
  #   coordinates$search.twitter.entry<-gsub(" ","", coordinates$search.twitter.entry ,
  #                                          fixed=TRUE)
  #   
  #   #Search twitter at each location, check how many tweets and put into dataframe
  #   for(i in 1:length(coordinates$lat)){
  #     coordinates$number.of.tweets[i]<-
  #       length(searchTwitter(searchString=searchtext,n=100,geocode=coordinates$search.twitter.entry[i]))
  #   }
    #making the US map
    # all_states <- map_data("state")
    # #plot all points on the map
    # p <- ggplot()
    # p <- p + geom_polygon( data=all_states, aes(x=long, y=lat, group = group),colour="grey",     fill=NA )
    # 
    # p<-p + geom_point( data=coordinates, aes(x=long, y=lat,color=number.of.tweets
    # )) + scale_size(name="# of tweets")
    # p
  #}
  # geoanalysis<-function(entity1usrs,entity2usrs,entity1entry,entity2entry){
  #   #look up user screennames to get information like geolocations
  #   users1 <- lookupUsers(entity1usrs)
  #   users2 <- lookupUsers(entity2usrs)
  #   users1_df <- twListToDF(users1)
  #   users2_df <- twListToDF(users2)
  #   # Adding a dummy variable useful for a ggplot
  #   users1_df$entity = entity1entry
  #   users2_df$entity = entity2entry
  #   #combine usr information
  #   entityusers<-rbind(users1_df,users2_df)
  # }
  userinfo<-function(usernames){
    #extract twitter user information
    users<-lookupUsers(usernames)
    #convert into a dataframe
    users_df<-twListToDF(users)
    #extract location information by using $location
    entitymap<-users_df$location
  }
  
  geoanalysis<-function(entity1map,entity2map,entity1entry,entity2entry){
    # entity1map <- na.omit(entity1map)
    # entity2map <- na.omit(entity2map)
    lon<-c(geocode(as.character(entity1map))$lon,geocode(as.character(entity2map))$lon)
    lat<-c(geocode(as.character(entity1map))$lat,geocode(as.character(entity2map))$lat)
    entity<-c(rep(entity1entry, length(lon)),rep(entity2entry, length(lon)))
    # Adding entity1entry and entity2entry  useful for a ggplot
    entityusers<-data.frame(lon,lat)
    #entityusers<-na.omit(entityusers$lat)
  }
  # Time for execution
  
  # Reading in values for the two entities
  entity1<-reactive({
    if(input$actb>=0){
      withProgress(
        message = 'Calculation in progress',
        value = 0, {
          # Number of times we'll go through the loop
          n <- 15
          for (i in 1:n) {
            incProgress(1/n, detail = paste("This may take a while", i))
            # Pause for 0.1 seconds to simulate a long computation.
            Sys.sleep(0.1)
          }
        }
      )
    }
    entity1<-TweetFrame(input$entity1, input$maxTweets)
  })

  #entity 2
  entity2<-reactive({
    if(input$actb>=0){
      withProgress(
        message = 'Calculation in progress',
        value = 0, {
          # Number of times we'll go through the loop
          n <- 15
          for (i in 1:n) {
            incProgress(1/n, detail = paste("This may take a while", i))
            # Pause for 0.1 seconds to simulate a long computation.
            Sys.sleep(0.1)
          }
        }
      )
    }
    entity2<-TweetFrame(input$entity2, input$maxTweets)
  })
  
  #Reading in map coordinates for two entities
  # Reading in values for the two entities
  entity1map<-reactive({
    if(input$actb>=0){
      withProgress(
        message = 'Calculation in progress',
        value = 0, {
          # Number of times we'll go through the loop
          n <- 15
          for (i in 1:n) {
            incProgress(1/n, detail = paste("This may take a while", i))
            # Pause for 0.1 seconds to simulate a long computation.
            Sys.sleep(0.1)
          }
        }
      )
    }
    entity1map<-userinfo(entity1()$screenName)
  })

  #entity 2
  entity2map<-reactive({
    if(input$actb>=0){
      withProgress(
        message = 'Calculation in progress',
        value = 0, {
          # Number of times we'll go through the loop
          n <- 15
          for (i in 1:n) {
            incProgress(1/n, detail = paste("This may take a while", i))
            # Pause for 0.1 seconds to simulate a long computation.
            Sys.sleep(0.1)
          }
        }
      )
    }
    entity2map<-userinfo(entity2()$screenName)
  })
  #Creating sentiment scores
  entityscores<-reactive({
    if(input$actb>=0){
      withProgress(
        message = 'Calculation in progress',
        value = 0, {
          # Number of times we'll go through the loop
          n <- 15
          for (i in 1:n) {
            # Increment the progress bar, and update the detail text.
            incProgress(1/n, detail = paste("This may take a while", i))
            # Pause for 0.1 seconds to simulate a long computation.
            Sys.sleep(0.1)
          }
        }
      )
    }
    entityscores<-sentimentalanalysis(entity1()$text,entity2()$text,input$entity1,input$entity2)
  })
  
  # # Create dataframe for the geolocations of the users
  # #Creating maps for each candidates
  # entitymaps<-reactive({
  #   if(input$actb>=0){
  #     withProgress(
  #       message = 'Calculation in progress',
  #       value = 0, {
  #         # Number of times we'll go through the loop
  #         n <- 15
  #         for (i in 1:n) {
  #           # Increment the progress bar, and update the detail text.
  #           incProgress(1/n, detail = paste("This may take a while", i))
  #           # Pause for 0.1 seconds to simulate a long computation.
  #           Sys.sleep(0.1)
  #         }
  #       }
  #     )
  #   }
  #   entitymaps<-geoanalysis(entity1map(),entity2map(),input$entity1,input$entity2)
  # })
  #Creating geolocation dataframe
  userloc<-reactive({
    if(input$actb>=0){
      withProgress(
        message = 'Calculation in progress',
        value = 0, {
          # Number of times we'll go through the loop
          n <- 15
          for (i in 1:n) {
            # Increment the progress bar, and update the detail text.
            incProgress(1/n, detail = paste("This may take a while", i))
            # Pause for 0.1 seconds to simulate a long computation.
            Sys.sleep(0.1)
          }
        }
      )
    }
    userloc<-geoanalysis(entity1map(),entity2map(),input$entity1,input$entity2)
  })
  #Preparing the output in a series of tabs
  
  #tab 1  - number of tweets for the two entities and also plotting the probability of arrival of a new tweet 
  #within a particular time t
  
  #number of tweets
  output$notweets<-renderPrint({
    if(input$actb>=0){
      withProgress(
        message = 'Calculation in progress',
        value = 0, {
          # Number of times we'll go through the loop
          n <- 15
          for (i in 1:n) {
            # Increment the progress bar, and update the detail text.
            incProgress(1/n, detail = paste("This may take a while", i))
            # Pause for 0.1 seconds to simulate a long computation.
            Sys.sleep(0.1)
          }
        }
      )
    }  
    numoftweets(entity1(),entity2(),input$entity1,input$entity2)
  })
  
  
  #tab 1: Not all chatter may be good. So a box plot to see the distribution of scores of sentiments 
  
  output$sentiboxplot<-renderPlot({
    cutoff <- data.frame(yintercept=0, cutoff=factor(0))
    if(input$actb>=0){
      withProgress(
        message = 'Calculation in progress',
        detail = 'This may take a while...',
        value = 0, {
          # Number of times we'll go through the loop
          n <- 15
          for (i in 1:n) {
            incProgress(1/n)
            Sys.sleep(0.1)
          }
        }
      )
    }
    sentiboxplot<-ggplot(entityscores(),aes(size, score))+
      facet_grid(. ~ entity)+
      geom_point(color = "black",size = 2, alpha = 1/2)+
      geom_smooth(method = "loess",se=FALSE,col='red',size=1.5, alpha = 0.7)+
      geom_hline(aes(yintercept=yintercept, linetype=cutoff), data=cutoff)+
      xlab('Tweet number')+
      ylab('Sentiment Score')+
      theme_bw()
    
    sentihist<-ggplot(entityscores(), aes(x=score, color=size, fill=entity))+
      geom_histogram(binwidth=0.5, position="dodge")+
      facet_grid(. ~ entity)+
      scale_x_continuous(breaks =  )+
      labs(title="Sentiment Histogram for Each Candidate")+
      xlab('Sentiment Score')+
      ylab('No. of Tweets')+
      theme(legend.position="bottom",axis.text.x=element_text(face="bold",size=10),
            plot.margin=unit(c(.25,0,0.25,0), "cm"))+
      theme(axis.text.y = element_text(color="black"))+
      theme(axis.text.x = element_text(color="black"))
    grid.arrange(sentiboxplot, sentihist)
    #print(sentiboxplot)
  })
  
  # getting a feel for how sentiments were scored by scanning 4 tweets per entity and sentiment scores - data frame entity scores shown
  output$sentiheadtable<-renderTable({tab<-head(entityscores(),4)})
  output$sentitailtable<-renderTable({tab<-tail(entityscores(),4)})
  
  #tab 2 - Word Clouds to highlight terms used in tweets associated with the two entities
  output$entity1wc<-renderText({
    
    input$entity1})
  output$entity1wcplot<-renderPlot({
    if(input$actb>=0){
      withProgress(
        message = 'Calculation in progress',
        detail = 'This may take a while...',
        value = 0, {
          # Number of times we'll go through the loop
          n <- 15
          for (i in 1:n) {
            incProgress(1/n)
            Sys.sleep(0.1)
          }
        }
      )
    }
    wordcloudentity(entity1()$text)})
  
  output$entity2wc<-renderText({input$entity2})
  output$entity2wcplot<-renderPlot({
    if(input$actb>=0){
      withProgress(
        message = 'Calculation in progress',
        detail = 'This may take a while...',
        value = 0, {
          # Number of times we'll go through the loop
          n <- 15
          for (i in 1:n) {
            incProgress(1/n)
            Sys.sleep(0.1)
          }
        }
      )
    }
    wordcloudentity(entity2()$text)})
  #tab  3: location of tweets for both Candidates
  # output$mapentity1<-renderPlot({
  #   all_states <- map_data("state")
  #   #plot all points on the map
  #   if(input$actb>=0){
  #     withProgress(
  #       message = 'Calculation in progress',
  #       detail = 'This may take a while...',
  #       value = 0, {
  #         # Number of times we'll go through the loop
  #         n <- 15
  #         for (i in 1:n) {
  #           incProgress(1/n)
  #           Sys.sleep(0.1)
  #         }
  #       }
  #     )
  #   }
  #   mapentity1<-ggplot() +
  #     geom_point(data=as.data.frame(entitymaps()), aes(x=lon, y=lat, color=number.of.tweets, fill = entity))+
  #     scale_size(name="# of tweets")
  # })

  output$tweetmap<-renderPlot({
    map.data <- map_data("state")
    points <- data.frame(x=as.numeric(userloc()$lon),
                         y=as.numeric(userloc()$lat))
    points <- points[points$y > 25,]
    points <- points[points$x < -66,]
    if(input$actb>=0){
      withProgress(
        message = 'Calculation in progress',
        detail = 'This may take a while...',
        value = 0, {
          # Number of times we'll go through the loop
          n <- 15
          for (i in 1:n) {
            incProgress(1/n)
            Sys.sleep(0.1)
          }
        }
      )
    }
    tweetmap <- ggplot(map.data)+
      geom_map(aes(map_id=region),
               map=map.data,
               fill="white",
               color="steelblue",size=0.25)+
      expand_limits(x=map.data$long,y=map.data$lat)+
      theme(axis.line=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            axis.title=element_blank(),
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            plot.background=element_blank(),
            plot.margin=unit(0*c(-1.5,-1.5,-1.5,-1.5),"lines"))+
      geom_point(data=points,
                 aes(x=x,y=y),size=1,
                 alpha=1/5,color="darkblue")
    print(tweetmap)
  })

  #tab  4: Raw tweets of Candidate A
  output$tableentity1 <- renderTable({tab<-entity1()[1]})
  
  #tab 5: Raw tweets of Candidate B
  
  output$tableentity2<-renderTable({tab<-entity2()[1]})
  
})