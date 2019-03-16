library(tidyverse) # general utility & workflow functions
library(tidytext) # tidy implimentation of NLP methods
library(topicmodels) # for LDA topic modelling 
library(tm) # general text mining functions, making document term matrixes
library(SnowballC) # for stemming

reviews <- read_csv("C:/Users/savi0/TwitterVapeData/tweetFile_Combined#tags.csv")
some_tweets = reviews$text
View(some_tweets)
some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "",some_tweets)
some_txt = gsub("@\\w+", "", some_txt)
some_txt = gsub("[[:punct:]]", "", some_txt)
some_txt = gsub("[[:digit:]]", "", some_txt)
some_txt = gsub("http\\w+", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)
some_txt = gsub("[^[:graph:]]", " ", some_txt)

reviews$text=some_txt

reviews$language=textcat(reviews$text)
reviews<-subset(reviews,language=="english")
reviews<-reviews[,!names(reviews)=="language"]

View(reviews)
top_terms_by_topic_LDA <- function(reviews$text, # should be a columm from a dataframe
                                   plot = T, # return a plot? TRUE by defult
                                   number_of_topics = 4) # number of topics (4 by default)
{    
  # create a corpus (type of object expected by tm) and document term matrix
  Corpus <- Corpus(VectorSource(reviews$text)) # make a corpus object
  DTM <- DocumentTermMatrix(Corpus) # get the count of words/document
  
  # remove any empty rows in our document term matrix (if there are any 
  # we'll get an error when we try to run our LDA)
  unique_indexes <- unique(DTM$i) # get the index of each unique value
  DTM <- DTM[unique_indexes,] # get a subset of only those indexes
  
  # preform LDA & get the words/topic in a tidy text format
  lda <- LDA(DTM, k =6, control = list(seed = 1234))
  topics <- tidy(lda, matrix = "beta")
  
  
  
  
  # get the top ten terms for each topic
  top_terms <- topics  %>% # take the topics data frame and..
    group_by(topic) %>% # treat each topic as a different group
    top_n(10, beta) %>% # get the top 10 most informative words
    ungroup() %>% # ungroup
    arrange(topic, -beta) # arrange words in descending informativeness
  
  # if the user asks for a plot (TRUE by default)
  if(plot == T){
    # plot the top ten terms for each topic in order
    top_terms %>% # take the top terms
      mutate(term = reorder(term, beta)) %>% # sort terms by beta value 
      ggplot(aes(term, beta, fill = factor(topic))) + # plot beta by theme
      geom_col(show.legend = FALSE) + # as a bar plot
      facet_wrap(~ topic, scales = "free") + # which each topic in a seperate plot
      labs(x = NULL, y = "Beta") + # no x label, change y label 
      coord_flip() # turn bars sideways
  }else{ 
    # if the user does not request a plot
    # return a list of sorted terms instead
    return(top_terms)
  
}
  top_terms_by_topic_LDA(reviews$text, number_of_topics = 2)4
  
  reviewsCorpus <- Corpus(VectorSource(reviews$text)) 
  reviewsDTM <- DocumentTermMatrix(reviewsCorpus)
  reviewsDTM_tidy <- tidy(reviewsDTM)  
  custom_stop_words <- tibble(word = c("and", "the", "you", "vapelife", "for", "can", "vape", "vapefam"))
  reviewsDTM_tidy_cleaned <- reviewsDTM_tidy %>% # take our tidy dtm and...
    anti_join(stop_words, by = c("term" = "word")) %>% # remove English stopwords and...
    anti_join(custom_stop_words, by = c("term" = "word"))   
  
  cleaned_documents <- reviewsDTM_tidy_cleaned %>%
    group_by(document) %>% 
    mutate(terms = toString(rep(term, count))) %>%
    select(document, terms) %>%
    unique()
  
  top_terms_by_topic_LDA(cleaned_documents$terms, number_of_topics = 2)
  
  
  
  top_terms_by_topic_tfidf <- function(text_df, text_column, group_column, plot = T){
    # name for the column we're going to unnest_tokens_ to
    # (you only need to worry about enquo stuff if you're
    # writing a function using using tidyverse packages)
    group_column <- enquo(group_column)
    text_column <- enquo(text_column)
    
    # get the count of each word in each review
    words <- text_df %>%
      unnest_tokens(word, !!text_column) %>%
      count(!!group_column, word) %>% 
      ungroup()
    
    # get the number of words per text
    total_words <- words %>% 
      group_by(!!group_column) %>% 
      summarize(total = sum(n))
    
    # combine the two dataframes we just made
    words <- left_join(words, total_words)
    
    # get the tf_idf & order the words by degree of relevence
    tf_idf <- words %>%
      bind_tf_idf(word, !!group_column, n) %>%
      select(-total) %>%
      arrange(desc(tf_idf)) %>%
      mutate(word = factor(word, levels = rev(unique(word))))
    
    if(plot == T){
      # convert "group" into a quote of a name
      # (this is due to funkiness with calling ggplot2
      # in functions)
      group_name <- quo_name(group_column)
      
      # plot the 10 most informative terms per topic
      tf_idf %>% 
        group_by(!!screenName) %>% 
        top_n(10) %>% 
        ungroup %>%
        ggplot(aes(word, tf_idf, fill = as.factor(group_name))) +
        geom_col(show.legend = FALSE) +
        labs(x = NULL, y = "tf-idf") +
        facet_wrap(reformulate(group_name), scales = "free") +
        coord_flip()
    }else{
      # return the entire tf_idf dataframe
      return(tf_idf)
    }
  }
  
  top_terms_by_topic_tfidf(text_df = reviews, # dataframe
                           text_column = text, # column with text
                           group_column = deceptive, # column with topic label
                           plot = T) 
  
  
  
  
  
  top_terms_by_topic_tfidf(text_df = reviews, # dataframe
                           text_column = text, # column with text
                           group_column = polarity, # column with topic label
                           plot = T) 
  
  library(graph)
  library(RGraphics)
  plot(dtm, term=freq.terms, corThreashold=0.12,weighting=T)  
  
  
  library(tm)
  freq.terms<-findFreqTerms(dtm,lowfreq =250)
inspect(freq.terms)  

topic<- topics(lda,1)
topics<-data.frame(date=(reviews$created),topic)
qplot(date, ..count..,data=topics,geom="density",fill=term[topic],position="stack")
  
  
  
  
  
  
  
  
  
  
  
  
