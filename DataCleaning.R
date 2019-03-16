library(readr)
CopytweetFile_1 <- read_csv("tweetFile_Combined#tags.csv")
View(CopytweetFile_1)
str(CopytweetFile_1)

some_tweets = CopytweetFile_1$text
#View(some_tweets)
some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "",some_tweets)
some_txt = gsub("@\\w+", "", some_txt)
some_txt = gsub("[[:punct:]]", "", some_txt)
some_txt = gsub("[[:digit:]]", "", some_txt)
some_txt = gsub("http\\w+", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)
some_txt = gsub("[^[:graph:]]", " ", some_txt)
View(some_txt)
write.csv(some_txt, file="CleanTweets_A.csv")


#convert all text to lower case\
library(h2o)
toLower(some_txt, keep_acronyms = TRUE)
Lower_case_text <-data.frame(tolower(as.matrix(some_txt)))
write.csv(Lower_case_text, file="Lower_case_text")

##remove hashtags
Lower_case_text1 = gsub("vapelife", "", Lower_case_text$tolower.as.matrix.some_txt..)
View(Lower_case_text1)
Lower_case_text1 = gsub("vapelyfe", "", Lower_case_text1)
Lower_case_text_f = gsub("vapefam", "", Lower_case_text1)
#View(some_txt)
#write.csv(Lower_case_text1, file="removed")
####################x
#text to Cporpus
library(corpus)
library(tm)
#corpus = Corpus(VectorSource(some_txt))
corpus = Corpus(VectorSource(Lower_case_text_f))
corpus = tm_map(corpus, removeWords, stopwords("english"))
#Lcorpus = tm_map(corpus,PlainTextDocument)
myCorpus = tm_map(corpus, PlainTextDocument, lazy = T)
yup<-DocumentTermMatrix(myCorpus)
dtm = DocumentTermMatrix(myCorpus) 
inspect(Lcorpus)
y<-content(Lcorpus)
inspect(y)
#Basic Twitter Data Analysis

corpusN_df = as.data.frame(as.matrix(Lcorpus,stringsAsFactors=FALSE))
#dput(dtm)
Ncorpus =tm_map(corpus,stemDocument)
dtm<-DocumentTermMatrix(Ncorpus)
inspect(Ncorpus)
View(CORX)
corpus_df = as.data.frame(CORX)
###############################################################
library(wordcloud)
library(RColorBrewer)
library(SnowballC)

wordcloud(CORX, max.words = 100, random.order = FALSE)
wordcloud(CORX, scale=c(5,0.5), max.words=500,
          random.order=FALSE, rot.per=0.35,
          use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
corpus_df = as.data.frame(CORX)
#######################################################
dtm <- DocumentTermMatrix(Ncorpus)
#View(dtm)
#m <- as.matrix(dtm)
#v <- sort(rowSums(m),decreasing=TRUE)
#d <- data.frame(word = names(v),freq=v)
#head(d, 15)

###################################
some_tweets$unstem = corpus_df$text
neg <- scan('C:/Users/savi0/TwitterVapeData/negative-words.txt', what='character',
            comment.char=';')
pos <- scan('C:/Users/savi0/TwitterVapeData/positive-words.txt', what='character',
            comment.char=';')

Dataset <- some_tweets$unstem
View(Dataset)
Dataset <- as.factor(Dataset)
score.sentiment <- function(sentences, pos.words, neg.words,
                            .progress='none'){
  require(plyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, pos.words, neg.words){
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence)
    sentence <- gsub('\\d+', "", sentence)
    sentence <- tolower(sentence)
    word.list <- str_split(sentence, '\\s+')
    words <- unlist(word.list)
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}

scores <- score.sentiment(Dataset, pos.words, neg.words,
                          .progress='text')

###work on it
some_tweets$scores = scores$score

