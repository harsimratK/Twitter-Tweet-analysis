library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(tm)
library(textcat)

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

#system("ls ../input")

# Any results you write to the current directory are saved as output.
removeURL <- function(x) {gsub("http[[:alnum:]]*", "", x)}
removeNonAlpha<-function(x) gsub("[^a-zA-Z]"," ",x)
removeUserMention<-function(x) gsub("@\\w+","",x)

All_tweets<-read.csv("C:/Users/savi0/TwitterVapeData/tweetFile_Combined#tags.csv",stringsAsFactors = FALSE)
View(All_tweets)
#Get only tweets in english
All_tweets$language=textcat(All_tweets$text)
All_tweets<-subset(All_tweets,language=="english") #10456 tweets
All_tweets<-All_tweets[,!names(All_tweets)=="language"]

All_tweets$tweets<-removeNonAlpha(All_tweets$text)
corpus<-Corpus(VectorSource(All_tweets$tweets))
corpus<-tm_map(corpus,tolower)
corpus<-tm_map(corpus,PlainTextDocument)text
corpus<-tm_map(corpus,removeWords,c(stopwords("english")))
corpus<-tm_map(corpus,removeWords,c("isi"))
corpus <- tm_map(corpus, content_transformer(removeURL)) 
corpus<-tm_map(corpus,content_transformer(removeUserMention))

corpus<-tm_map(corpus,removePunctuation)
corpus<-tm_map(corpus,stemDocument)

write.csv(corpus, "clean_all.csv")

dtm<-DocumentTermMatrix(corpus)
m = as.matrix(dtm)
d <- dist(m, method = "euclidean")
kfit <- kmeans(m, 2)
#plot – need library cluster
library(cluster)
clusplot(m, kfit$cluster)




wf = rowSums(m)
m1 = m[wf>quantile(wf,probs=0.9), ]
m1 = m1[,colSums(m1)!=0]
m1[m1 > 1] = 1
m1dist = dist(m1, method="binary")
clus1 = hclust(m1dist, method="ward")
plot(clus1, cex=0.5)
#View(m)
m1$label[order.dendrogram(as.dendrogram(clus1 ))]



sparse<-removeSparseTerms(dtm,0.99)

kmeans_20<-kmeans(x=sparse,centers=20,iter.max=100)

for (i in 1:20) {
  cat(paste("cluster ", i, ": ", sep = ""))
  s <- sort(kmeans_20$centers[i, ], decreasing = T)
  cat(names(s)[1:5], "\n")
  
}
kmeans_20
######attempt to create cluster graph
#kmeans_20$cluster <- as.factor(kmeans_20$cluster)
#ggplot(kmeans_20$cluster) + geom_point()

#library(cluster)
#d = daisy(sparse)
#clusplot(d, kmeans_20$cluster, main='2D representation of the Cluster solution')
#sparse$nrow, sparse$ncol         

#############################Sentiment Score

library(syuzhet)

mysentiment<-get_nrc_sentiment((All_tweets$tweets))
Sentimentscores<-data.frame(colSums(mysentiment[,]))
names(Sentimentscores)<-"Score"
SentimentScores<-cbind("sentiment"=rownames(Sentimentscores),Sentimentscores)
rownames(SentimentScores)<-NULL
ggplot(data=SentimentScores,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Total sentiment based on scores")
##########attempt 2- try agin
plot(
  syuzhet_vector, 
  type="h", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

##########################
#WordCloud

library(wordcloud)
library(RColorBrewer)
#draw the word cloud
set.seed(1234)
wordcloud(corpus, scale=c(5,0.5), max.words=1000,
          random.order=FALSE, rot.per=0.35,
          use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))

##############################################################


library(rtweet)
users<-RTweets_threeTags$username
N<-lookup_users(users, parse = TRUE, token = NULL)
``