#install.packages(c("devtools", "rjson", "bit64", "httr"))
library(RCurl)
library(twitteR)
library(ROAuth)
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
api_Key <-  "9a9x556XRs6fdMe7NvHOTuiGp"
api_Secret <- "YXZUrjk34kNpMmqwUGRGx6BMatkRoL3AEXIJ1zVBqewl7JV8Be"
#access_token <- " 93328549-OxQuPRMhHNNPNEwQAfJlzWsR6eC0AWXJA0NdXTRr3"
#access_token_secret <- "70NCiCJIbQGpOsexYHAZuuAJ3IARJ7QnyuFc9IIaqRBGm"
#setup_twitter_oauth(api_Key,api_Secret,access_token,access_token_secret)
twitCred <- OAuthFactory$new(consumerKey = api_Key, consumerSecret = api_Secret, 
                             requestURL = requestURL, accessURL = accessURL, authURL = authURL)
twitCred$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
save(twitCred, file = "my_oauth.Rdata")
# 
# twitterMap <- function(searchtext,locations,radius){
#   require(ggplot2)
#   require(maps)
#   require(twitteR)
#   #radius from randomly chosen location
#   radius=radius
#   lat<-runif(n=locations,min=24.446667, max=49.384472)
#   long<-runif(n=locations,min=-124.733056, max=-66.949778)
#   #generate data fram with random longitude, latitude and chosen radius
#   coordinates<-as.data.frame(cbind(lat,long,radius))
#   coordinates$lat<-lat
#   coordinates$long<-long
#   #create a string of the lat, long, and radius for entry into searchTwitter()
#   for(i in 1:length(coordinates$lat)){
#     coordinates$search.twitter.entry[i]<-toString(c(coordinates$lat[i],
#                                                     coordinates$long[i],radius))
#   }
#   # take out spaces in the string
#   coordinates$search.twitter.entry<-gsub(" ","", coordinates$search.twitter.entry ,
#                                          fixed=TRUE)
#   
#   #Search twitter at each location, check how many tweets and put into dataframe
#   for(i in 1:length(coordinates$lat)){
#     coordinates$number.of.tweets[i]<-
#       length(searchTwitter(searchString=searchtext,n=1000,geocode=coordinates$search.twitter.entry[i]))
#   }
#   #making the US map
#   all_states <- map_data("state")
#   #plot all points on the map
#   p <- ggplot()
#   p <- p + geom_polygon( data=all_states, aes(x=long, y=lat, group = group),colour="grey",     fill=NA )
#   
#   p<-p + geom_point( data=coordinates, aes(x=long, y=lat,color=number.of.tweets
#   )) + scale_size(name="# of tweets")
#   p
# }
#   