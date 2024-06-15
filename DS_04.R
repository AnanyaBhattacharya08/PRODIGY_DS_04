library(rtweet)

# Authenticate with Twitter API
create_token(
  app = "your_app_name",
  consumer_key = "your_consumer_key",
  consumer_secret = "your_consumer_secret",
  access_token = "your_access_token",
  access_secret = "your_access_secret"
)

# Search for tweets based on a keyword
tweets <- search_tweets(q = "your_keyword", n = 1000)

#loading the necessary libraries
library(tidyverse)
library(tm)
library(stringr)
library(tidytext)
library(syuzhet)
library(ggplot2)

#searching for tweets
tweets<-search_tweets(q="artificial intelligence",n=1000)

#cleaning the data 
clean_tweets <- tweets %>%
  select(text) %>%
  mutate(text = str_replace_all(text, "[^[:graph:]]", " ")) %>%  # Remove emojis and non-graphic characters
  mutate(text = str_replace_all(text, "https?://\\S+\\s?", "")) %>%  # Remove URLs
  mutate(text = str_replace_all(text, "#\\S+", "")) %>%  # Remove hashtags
  mutate(text = str_replace_all(text, "@\\S+", "")) %>%  # Remove mentions
  mutate(text = str_replace_all(text, "\\bRT\\b", "")) %>%  # Remove RT (retweet)
  mutate(text = str_replace_all(text, "\\s+", " ")) %>%  # Remove extra white spaces
  mutate(text = str_trim(text))  # Trim leading and trailing white spaces

#check the structure of clean_tweets
str(clean_tweets)

#performing sentiment analysis
sentiment_scores <- get_sentiment(clean_tweets$text, method = "syuzhet")
clean_tweets$sentiment_score <- sentiment_scores

#visualization of sentiment patterns
clean_tweets$time <- as.Date(tweets$created_at)

ggplot(clean_tweets, aes(x = time, y = sentiment_scores)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Sentiment Analysis of Tweets Over Time",
       x = "Time",
       y = "Sentiment Score")

#preparing histogram of sentiment scores
ggplot(clean_tweets, aes(x = sentiment_scores)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Distribution of Sentiment Scores",
       x = "Sentiment Score",
       y = "Frequency")
