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

tweetsSS <- searchTwitter(
  '#SecretaryOfState',
  n = 4000,
  lang="en",
  since='2016-11-08'
  )

# Transform tweets list into a data frame
tweets_geolocated.df <- twListToDF(tweetsSS)

#local <- tweets_geolocated.df$longitude[tweets_geolocated.df$longitude!= NA] 

##### ggmap
library(rgdal)
library(ggmap)
library(ggplot2)
library(scales)
library(ggthemes)

# get US county shape file
# https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
us_county <- readOGR(dsn=".", layer="cb_2015_us_county_5m") #S4
county_shape <- spTransform(us_county, CRS("+proj=longlat +datum=WGS84")) #S4
county_shape <- fortify(county_shape) # list
county_shape$group <- as.numeric(as.character(county_shape$id))

USmap <- get_map("United States", zoom=4)
ggmap(USmap) + geom_polygon(aes(x=long, y=lat, group=group,fill=group), 
                            size=.2,color='green', 
                            data=county_shape, alpha=.3)

#library(RgoogleMaps)
#library(ggmap)
#library(ggplot2)
#library(maptools)
#library(sp)


#geocode("fenway")

#qmap(location = "fenway",zoom=12)
