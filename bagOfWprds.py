# -*- coding: utf-8 -*-
"""
Created on Thu May 03 01:08:49 2018

@author: savi0
"""

import os
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.ensemble import RandomForestClassifier
#from KaggleWord2VecUtility import KaggleWord2VecUtility
import pandas as pd
import numpy as np
from bs4 import BeautifulSoup
import re
tsv = open('C:/Users/savi0/TwitterVapeData/labeledTrainData.tsv', 'r')
fileContent =  tsv.read()
tsv_file='labeledTrainData.tsv'
csv_table=pd.read_table(tsv_file,sep='\t')
csv_table.to_csv('new_name.csv',index=False)

train = pd.read_csv('new_name.csv')
test = pd.read_csv("C:/Users/savi0/TwitterVapeData/CleanTweets.csv" )
 vectorizer = CountVectorizer(analyzer = "word",   \
                             tokenizer = None,    \
                             preprocessor = None, \
                             stop_words = None,   \
                             max_features = 5000)
clean_train_reviews = [] 

 
class KaggleWord2VecUtility(object):
    """KaggleWord2VecUtility is a utility class for processing raw HTML text into segments for further learning"""

    @staticmethod
    def review_to_wordlist( review, remove_stopwords=False ):
        # Function to convert a document to a sequence of words,
        # optionally removing stop words.  Returns a list of words.
        #
        # 1. Remove HTML
        review_text = BeautifulSoup(review).get_text()
        #
        # 2. Remove non-letters
        review_text = re.sub("[^a-zA-Z]"," ", review_text)
        #
        # 3. Convert words to lower case and split them
        words = review_text.lower().split()
        #
        # 4. Optionally remove stop words (false by default)
       
        #
        # 5. Return a list of words
        return(words)

    # Define a function to split a review into parsed sentences
    @staticmethod
    def review_to_sentences( review, tokenizer, remove_stopwords=False ):
        # Function to split a review into parsed sentences. Returns a
        # list of sentences, where each sentence is a list of words
        #
        # 1. Use the NLTK tokenizer to split the paragraph into sentences
        raw_sentences = tokenizer.tokenize(review.decode('utf8').strip())
        #
        # 2. Loop over each sentence
        sentences = []
        for raw_sentence in raw_sentences:
            # If a sentence is empty, skip it
            if len(raw_sentence) > 0:
                # Otherwise, call review_to_wordlist to get a list of words
                sentences.append( KaggleWord2VecUtility.review_to_wordlist( raw_sentence, \
                  remove_stopwords ))
        #
        # Return the list of sentences (each sentence is a list of words,
        # so this returns a list of lists
        return sentences
len(train['tweets'])
for i in xrange( 0, len(train['review'])):
        clean_train_reviews.append(" ".join(KaggleWord2VecUtility.review_to_wordlist(train['review'][i], True)))

train_data_features = vectorizer.fit_transform(clean_train_reviews)    
np.asarray(train_data_features)
forest = RandomForestClassifier(n_estimators = 100)
forest = forest.fit( train_data_features, train["sentiment"] )
clean_test_tweets = []
for i in xrange(0,len(test['tweets'])):
        clean_test_tweets.append(" ".join(KaggleWord2VecUtility.review_to_wordlist(test['tweets'][i], True)))
test_data_features = vectorizer.transform(clean_test_tweets)
np.asarray(test_data_features)        

result = forest.predict(test_data_features)        

output = pd.DataFrame( data={ "sentiment":result} )        
output.to_csv( 'Bag_of_Words_model2.csv')
    print "Wrote results to Bag_of_Words_model.csv"        
        
len(test['tweets']) 
        
        
        
        
        
        
        
        
        