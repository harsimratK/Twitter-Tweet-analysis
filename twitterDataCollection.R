#install.packages(“twitteR”)
library(twitteR)
library(rtweet)
library(RCurl)
library(RJSONIO)
library(plyr)

consumerKey <-"LoZrYtYvURTNFR0XnephZriij" 
consumerSecret <-"qM7dgHlphBdQlI83cCmES6s1wIfv3fQjUXlZ6DeLN8EViMkuu2"
twitterAccessToken <-"3003234523-bJGhuUv8IBuFuDWfIUYepMrxYyQBtz6RoZa4LFi"
twitterAccessTokenSecret <-"OCu3veQq0Q1cxftCJW3vmUdBoqi0At36XEzcLXVqZXCka"

#set up to authenticate
setup_twitter_oauth(consumerKey ,consumerSecret,twitterAccessToken,twitterAccessTokenSecret)
#date yy/mm/dd






users<-RTweets_threeTags$username
usr_df <- lookup_users(users)
N<-lookup_users(users, parse = TRUE, token = NULL)

tweets_1 <- twitteR::searchTwitter("#vapelyfe", n=5000, lang="en")
tweeta_1.df <-twListToDF(tweets_1)                                  
write.csv(tweeta_1.df, file="tweetFile_1.csv")

userInfo1 <- lookupUsers(tweeta_1.df$screenName)  # Batch lookup of user info
userFrame <- twListToDF(userInfo1)
#userFrame[is.na(userFrame)] <- " "
#locatedUsers1 <- !is.na(userFrame$location)
locatedUsers1 <- (userFrame$location)
locations_1 <- geocode(locatedUsers1$)

write.csv(locations_1, file="tweetlocation_1.csv")




tweets_2 <- twitteR::searchTwitter("#vapefam", n=5000, lang="en")
tweeta_2.df <-twListToDF(tweets_2)                                  
write.csv(tweeta_2.df, file="tweetFile_2.csv") 

tweets_3 <- twitteR::searchTwitter("#vapelife", n=15000, lang="en")
tweeta_3.df <-twListToDF(tweets_3)                                  
write.csv(tweeta_3.df, file="tweetFile_3.csv")

#########################
Page_tweets<-userTimeline("juulvapor",n=5000)
tweetsP.df <- twListToDF(Page_tweets)
write.csv(tweetsP.df, file="TweetsOnPage.csv")


Page_tweets2<-userTimeline("PAXvapor",n=5000)
tweetsPax.df <- twListToDF(Page_tweets2)
write.csv(tweetsPax.df, file="TweetsOnPagePax.csv")

###################################

#install.packages("leaflet") 
#install.packages("maps")
library(leaflet)
library(maps)

mymap<-read.csv("C:/Users/savi0/TwitterVapeData/final_lat_longi.csv", stringsAsFactors = FALSE)
#stringAsFactors = FALSE means to keep the information as it is and not convert it into factors.

m <- leaflet(mymap) %>% addTiles()
m %>% addCircles(lng = ~lon, lat = ~lat, popup = mymap$type, weight = 8, radius = 40, color = "#fb3004", stroke = TRUE, fillOpacity = 0.8)
#############################################
## plot time series of tweets
ts_plot(tweeta_1.df, by="days") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #vapelyfe "
  )

ts_plot(tweeta_2.df, by="days") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #vapefam "
  )

ts_plot(tweeta_3.df, by="days") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #vapelife "
  )
###############################################################
library(RCurl)
library(RJSONIO)
library(plyr)
library(dismo)
library(maps)
library(ggplot2)
library(XML)
library(RJSONIO)
tweets_4 <- twitteR::searchTwitter("#vapefam", n=6, lang="en" )
tweeta_4.df <-twListToDF(tweets_4) 
userInfo <- lookupUsers(tweeta_4.df$screenName)  # Batch lookup of user info
userFramex <- twListToDF(userInfo) 

locatedUsers <- !is.na(userFrame$location)
locations1 <- geocode(locatedUsers)
with(locations, plot(longitude, latitude))






worldMap <- map_data("world")  # Easiest way to grab a world map shapefile

zp1 <- ggplot(worldMap)
zp1 <- zp1 + geom_path(aes(x = longitude, y = latitude, group = group),  # Draw map
                       colour = gray(2/3), lwd = 1/3)
zp1 <- zp1 + geom_point(data = locations,  # Add points indicating users
                        aes(x = longitude, y = latitude),
                        colour = "RED", alpha = 1/2, size = 1)
zp1 <- zp1 + coord_equal()  # Better projections are left for a future post
zp1 <- zp1 + theme_minimal()  # Drop background annotations
print(zp1)
  