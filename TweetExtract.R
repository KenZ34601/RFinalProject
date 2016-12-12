# to to https://dev.twitter.com
# log in with your twitter account
#
# create new application
# 
# use your website or http://www.kenzhou.weebler.com
# fill in the blanks
# click Create
#
# don't close the page -- you'll need to come back for
# your API key etc
#
# install twitteR, devtools, rjson, bit64, httr
#
# restart your R session
#



library(devtools)
library(twitteR)

api_Key <-  "9a9x556XRs6fdMe7NvHOTuiGp"
api_Secret <- "YXZUrjk34kNpMmqwUGRGx6BMatkRoL3AEXIJ1zVBqewl7JV8Be"
access_token <- "93328549-52Jw22wcjJv3lansCqDR9sHzaUHJBfQXMbld3rjyV"
access_token_secret <- "FG09fF2MVSnG5GKtiV9rLfdDBigcXuPZ4virNnjiiXpXA"
setup_twitter_oauth(api_Key, api_Secret, access_token, access_token_secret)

# [1] "Using direct authentication"
# Use a local file ('.httr-oauth'), to cache OAuth access credentials between R sessions?
# 
# 1: Yes
# 2: No
# 
# Selection: 1
# Adding .httr-oauth to .gitignore
N=100  # tweets to request from each query
S=200  # radius in miles
lats=c(38.9,40.7,37.8,39,37.4,28,30,42.4,48,36,32.3,33.5,34.7,33.8,37.2,41.2,46.8,
       46.6,37.2,43,42.7,40.8,36.2,38.6,35.8,40.3,43.6,40.8,44.9,44.9)

lons=c(-77,-74,-122,-105.5,-122,-82.5,-98,-71,-122,-115,-86.3,-112,-92.3,-84.4,-93.3,
       -104.8,-100.8,-112, -93.3,-89,-84.5,-111.8,-86.8,-92.2,-78.6,-76.8,-116.2,-98.7,-123,-93)

#cities=DC,New York,San Fransisco,Colorado,Mountainview,Tampa,Austin,Boston,
#       Seatle,Vegas,Montgomery,Phoenix,Little Rock,Atlanta,Springfield,
#       Cheyenne,Bisruk,Helena,Springfield,Madison,Lansing,Salt Lake City,Nashville
#       Jefferson City,Raleigh,Harrisburg,Boise,Lincoln,Salem,St. Paul

tweetsSS=do.call(rbind,lapply(1:length(lats), function(i) searchTwitter('#SecretaryOfState',
                                                                      lang="en",n=N,resultType="recent",
                                                                      geocode=paste(lats[i],lons[i],paste0(S,"mi"),sep=","),
                                                                      since='2016-11-08')))


# Transform tweets list into a data frame
tweets_geolocated.df <- twListToDF(tweetsSS)
users <- lookupUsers(tweets_geolocated.df$screenName)
users_df <- twListToDF(users)
#table(users_df[1:10, 'location'])

################################################
library(streamR)
twitCred<- NULL
load("my_oauth.Rdata")
filterStream(
  "tweetsSS.json",
  track='#SecretaryOfState',
  locations=c(-125,25,-66,50),
  timeout=30,
  oauth=twitCred
  )

filterStream(
  "tweetsJH.json",
  track=c("secretary of state", "Huntsman"),
  locations=c(-125,25,-66,50),
  timeout=300,
  oauth=twitCred
  )
tweets.df <- parseTweets("tweetsJH.json", verbose=FALSE)

c(length(grep("secretary of state", tweets.df$text, ignore.case=TRUE)),
  length(grep("Huntsman", tweets.df$text, ignore.case=TRUE)))


###########################################


library(ggplot2)
library(grid)

map.data <- map_data("state")
points <- data.frame(x=as.numeric(tweets.df$lon),
                     y=as.numeric(tweets.df$lat))


points <- points[points$y>25,] 
ggplot(map.data)+
  geom_map(aes(map_id=region),
           map=map.data,
           fill="white",
           color="grey20",size=0.25)+
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

