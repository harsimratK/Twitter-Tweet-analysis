library(twitteR)
library(rtweet)
library(RCurl)
library(RJSONIO)
library(plyr)
library(ggmap)
library(ggplot2)
consumerKey <-"LoZrYtYvURTNFR0XnephZriij" 
consumerSecret <-"qM7dgHlphBdQlI83cCmES6s1wIfv3fQjUXlZ6DeLN8EViMkuu2"
twitterAccessToken <-"3003234523-bJGhuUv8IBuFuDWfIUYepMrxYyQBtz6RoZa4LFi"
twitterAccessTokenSecret <-"OCu3veQq0Q1cxftCJW3vmUdBoqi0At36XEzcLXVqZXCka"

#set up to authenticate
setup_twitter_oauth(consumerKey ,consumerSecret,twitterAccessToken,twitterAccessTokenSecret)
#date yy/mm/dd

All_tweets_tags<-read.csv("C:/Users/savi0/TwitterVapeData/Tweets_threeTags.csv",stringsAsFactors = FALSE)
View(All_tweets)

userInfo1 <- lookupUsers(All_tweets_tags$screenName)  # Batch lookup of user info
userFrame <- twListToDF(userInfo1)
userFrame[is.na(userFrame)] <- " "
locatedUsers1 <-userFrame %>% na.omit()
locatedUsers1 <- (userFrame$location)
View(locatedUsers2)
a2 <- as.character(locatedUsers2$x)
NewFile<-geocode(a2)

write.csv(NewFile, file="final_lat_longi.csv")

library(leaflet)
library(maps)

mymap<-read.csv("final_lat_longi.csv", stringsAsFactors = FALSE)
#stringAsFactors = FALSE means to keep the information as it is and not convert it into factors.

m <- leaflet(mymap)%>%addTiles()
m %>% addCircles(lng = ~lon, lat = ~lat, popup = mymap$type, weight = 8, radius = 40, color = "#fb3004", stroke = TRUE, fillOpacity = 0.8)
#############################################
write.csv(locatedUsers1, file="work.csv")

#manually deleted empty rows
locatedUsers2<-read.csv("work.csv")
library(dplyr)

locations_3 <- geocode(locatedUsers2$x)
locatedUsers<-na.omit(locatedUsers1)
write.csv(locations_1, file="tweetlocation_final.csv")
