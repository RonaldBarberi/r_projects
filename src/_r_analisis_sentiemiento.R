# Created on Thu Nov 03 20:41:48 2024
# @author: Ronald.Barberi

#%% Imported Libraries

library(openxlsx)
library(dplyr)
library(tidytext)
library(ggplot2)
library(stopwords)

#%% Create Class

AnalysisDataSet <- function(varPathData, varPathExport) {
  init <- list(
    varPathData = varPathData,
    df = NULL
  )
  class(init) <- 'EstructuredDF'
  return(init)
}

structured_data.EstructuredDF <- function(obj) {
  df <- read.csv(obj$varPathData, sep = '|')
  columnas <- c(
    'index',
    'X_id',
    'id',
    'time',
    'username',
    'name',
    'place',
    'likes_count',
    'hashtags',
    'link',
    'retweet',
    'near',
    'geo........'
  )
  df <- df[ , !(names(df) %in% columnas)]
  df <- na.omit(df)
  date_types <- sapply(df, class)
  print(date_types)
  
  obj$df <- df
  return(obj)
}

sentiment_analysis.EstructuredDF <- function(obj) {
  # Filter tweets more retuit
  top_retweeted <- obj$df %>%
    arrange(desc(retweets_count)) %>%
    filter(retweets_count > 100)
  
  print("Contenido de top_retweeted:")
  print(head(top_retweeted))
  
  # Stopwords in Spanish y tokenizar ti id temp
  stop_words_es <- data.frame(word = stopwords("es"))
  tweets_tokens <- top_retweeted %>%
    mutate(tweet_id = row_number()) %>%
    unnest_tokens(word, tweet) %>%
    anti_join(stop_words_es, by = "word")
  
  print("Contenido de tweets_tokens después de la tokenización:")
  print(head(tweets_tokens))
  
  # Frequently asked words analysis
  frequent_words <- tweets_tokens %>%
    count(word, sort = TRUE)
  print("Palabras más frecuentes:")
  print(head(frequent_words, 10))
  
  # Lexicón to feeling and analysis
  sentiments <- get_sentiments("bing")
  sentiment_by_tweet <- tweets_tokens %>%
    inner_join(sentiments, by = "word") %>%
    group_by(tweet_id) %>%
    summarise(sentiment_score = sum(ifelse(sentiment == "positive", 1, -1))) %>%
    arrange(desc(sentiment_score))
  
  # Results
  print("Resultados del análisis de sentimiento:")
  print(head(sentiment_by_tweet))
  
  # Grafics
  ggplot(frequent_words %>% top_n(10), aes(x = reorder(word, n), y = n)) +
    geom_col() +
    coord_flip() +
    labs(title = "Palabras más comunes en los tweets más retuiteados",
         x = "Palabra", y = "Frecuencia")
  
  ggplot(sentiment_by_tweet, aes(x = reorder(tweet_id, sentiment_score), y = sentiment_score)) +
    geom_col() +
    coord_flip() +
    labs(title = "Sentimiento de los tweets más retuiteados",
         x = "Tweet ID", y = "Puntaje de Sentimiento")
}

main <- function() {
  varPathData <- 'C:/Users/USER/OneDrive/Escritorio/github/r_projects/data/vacunasXFrase.csv'
  
  dataProcessor <- AnalysisDataSet(varPathData)
  
  dataProcessor <- structured_data.EstructuredDF(dataProcessor)
  sentiment_analysis.EstructuredDF(dataProcessor)
  print('Proceso finalizado.')
}

main()
