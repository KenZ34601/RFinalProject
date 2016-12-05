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


