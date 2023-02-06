# install.packages(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


source("src/SupportFunctions.R")
# Introduction

# Packages Used
library(readr)
library(dplyr)
library(pander)
library(tm)
library(SnowballC)
library(textstem)
library(magrittr)

library(tidytext)
library(tm)
library(topicmodels)
library(tidyverse)


# Data Cleaning


## Loading dataset
phone_data <- read_csv("data/mobile-phones-20191226-items.csv")
phone_review_data <- read_csv("data/mobile-phones-20191226-reviews.csv")

## Merging both datasets to retrieve phone names 
phone_reviews_df <- merge(phone_review_data, phone_data, by="asin")

## Dropping unnecessary columns
cols_not_required <- c("name", "verified",  "helpfulVotes", "title.y", "url", "image",
                      "rating.y", "reviewUrl", "totalReviews", "price", "originalPrice")

phone_reviews_df <- phone_reviews_df[,!names(phone_reviews_df) %in% cols_not_required]

## Transforming columns
str(phone_reviews_df)

colnames(phone_reviews_df)[colnames(phone_reviews_df) == 'body'] <- 'review'
colnames(phone_reviews_df)[colnames(phone_reviews_df) == 'asin'] <- 'id' #renaming col

phone_reviews_df$date <- as.Date(phone_reviews_df$date, format ="%B %d,%Y" ) #formatting data type

## Removing rows with missing values 
phone_reviews_df <- phone_reviews_df %>% na.omit()

## Data Preview
pandoc.table(phone_reviews_df[1:3,], style = 'grid')

##  Table of brands shows 10 brands 
# Apple, ASUS, Google, HUAWEI, Motorola, Nokia, OnePlus, Samsung, Sony, Xiaomi
table(phone_reviews_df$brand)

# Table of Ratings
table(phone_reviews_df$rating.x)

# Text Preprocessing 

## Review cleaning
phone_corpus <- VCorpus(VectorSource(phone_reviews_df$review))
phone_corpus <- phone_corpus %>% 
  tm_map(content_transformer(tolower)) %>% #makes words lowercase
  tm_map(removeWords, stopwords()) %>% # remove stopwords
  tm_map(removePunctuation) %>% # remove punctuation
  tm_map(removeNumbers) %>%
  tm_map(PlainTextDocument) %>%
  tm_map(stripWhitespace) # remove whitespaces

## Content preview
phone_corpus[[5]]$content
phone_corpus[[50]]$content

## Create a Document Term Matrix
phone_dtm <- DocumentTermMatrix(phone_corpus)

## Frequent words appearing at least 10 times
frequent_review <- findFreqTerms(phone_dtm, 20)
head(frequent_review,20)

## More text cleaning
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

phone_corpus <-tm_map(phone_corpus, toSpace,"[^[:alnum:]]")# Removing special characters
phone_corpus <- tm_map(phone_corpus, stripWhitespace)

phone_ <- sapply(phone_corpus, function(x){x$content})

phone_1_lem <- lemmatize_strings(phone_)

phone_reviews_df %<>% mutate(reviewText = phone_1_lem)

# Final Data Preview
phone_corpus_2 <- VCorpus(VectorSource(phone_reviews_df$reviewText))

phone_corpus_2 <- tm_map(phone_corpus_2, stemDocument)

## Create a Document Term Matrix
phone_dtm_2 <- DocumentTermMatrix(phone_corpus_2)

## Frequent words appearing at least 10 times
frequent_review_2 <- findFreqTerms(phone_dtm_2, 30)
head(frequent_review_2,20)

phone_vec_2 <- sapply(phone_corpus_2, function(x){x$content})

phone_reviews_df %<>% mutate(reviewText = phone_vec_2)


# Binary Classification

## For binary classification purpose
# 1 for great reviews (4 or 5) and 0 for bad reviews (1 or 2)
# Ratings of 3 will be excluded as they might be considered neutral

phone_reviews_df_bi <- phone_reviews_df %>% filter(rating.x != 3) %>% 
  mutate(rating_new = if_else(rating.x >= 4, 1, 0))

## Splitting data into train and test data



# Topic Modeling --------------------
# create a document term matrix to clean
reviewsCorpus <- Corpus(VectorSource(phone_reviews_df_bi$review)) 
reviewsDTM <- DocumentTermMatrix(reviewsCorpus)

# convert the document term matrix to a tidytext corpus
reviewsDTM_tidy <- tidy(reviewsDTM)

# I'm going to add my own custom stop words that I don't think will be
# very informative in phone reviews
custom_stop_words <- tibble(word = c("phone", "smartphone"))

# remove stopwords
reviewsDTM_tidy_cleaned <- reviewsDTM_tidy %>% # take our tidy dtm and...
  anti_join(stop_words, by = c("term" = "word")) %>% # remove English stopwords and...
  anti_join(custom_stop_words, by = c("term" = "word")) # remove my custom stopwords

# reconstruct cleaned documents (so that each word shows up the correct number of times)
cleaned_documents <- reviewsDTM_tidy_cleaned %>%
  group_by(document) %>% 
  mutate(terms = toString(rep(term, count))) %>%
  select(document, terms) %>%
  unique()

# check out what the cleaned documents look like (should just be a bunch of content words)
# in alphabetic order
head(cleaned_documents)




# plot top ten terms in the phone reviews by topic
top_terms_by_topic_LDA(cleaned_documents$terms, number_of_topics = 5)
