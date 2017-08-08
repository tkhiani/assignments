library(twitteR) ### for fetching the tweets
library(plyr) ## for breaking the data into manageable pieces
library(ROAuth) # for R authentication
library(stringr) # for string processing
library(ggplot2) # for plotting the results
library(tidytext) # for working with words and text - http://tidytextmining.com/tfidf.html#the-bind_tf_idf-function
library(dplyr) # Manipulating data - group by, filter...
library(wordcloud) # create a word cloud
library(reshape2) # needed to create a matrix to feed into comparision cloud
library(tidyr)

# Set up connection with twitter
# reqURL <- "https://api.twitter.com/oauth/request_token"
# accessURL <- "http://api.twitter.com/oauth/access_token"
# authURL <- "http://api.twitter.com/oauth/authorize"
# api_key <- "nxfx9x6daXOwBxHdzqOd6sHu3"
# api_secret <- "xMEUQTX0R5ubBByEm3Uy7AhZVu5CQB6o8KA3P6uHTZUU1zf7EZ"
# access_token <- "135514978-wLEdLDPw4VslSCGx0KbwNuEZWY9a1tHuVibnf6OJ"
# access_token_secret <- "CoLphpfwhaD384qfDqEGq0Px6lYvO1wZhfqwD1C8iCu8s"
# setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

# Read the twitter handle
# feeLootTweets = searchTwitter('#SchoolFeeLoot', n=2500)

# Convert to a data frame so that we can manipuate it
# feeLootTweets = sapply(feeLootTweets, function(t) t$getText() )
# feeLootTweets <- data_frame(text = feeLootTweets)
# write.csv(feeLootTweets, file = "./learning/FeeLootTweet - TwitterAnalysis/feeLootTweets.csv")

feeLootTweets <- read.csv("./learning/FeeLootTweet - TwitterAnalysis/feeLootTweets.csv")
feeLootTweets$text <- as.character(feeLootTweets$text)

# create tokens for each word and remove stop words
wordsInTweet <- unnest_tokens(feeLootTweets, word, text)
wordsInTweet <- anti_join(wordsInTweet, stop_words)

# determine the sentiment for the words and plot the charts
# We can use bing(binary), AFINN(sentiment score), nrc(finer categories)
wordsWithSentiments <- wordsInTweet %>%
  inner_join(get_sentiments("bing")) %>%
  dplyr::count(word, sentiment, sort = TRUE)

# Create a word cloud and segregate into +ve and -ve tweets
wordsWithSentiments <- data.frame(wordsWithSentiments)
wordsWithSentiments <- top_n(wordsWithSentiments, 100, n)

wordsWithSentiments %>% 
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",x = NULL) +
  coord_flip()

wordcloud(words = wordsWithSentiments$word, wordsWithSentiments$n )

wordsWithSentiments %>% 
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"))

# identifying the sentiment using the entire sentence


# lets work with groups of words


# we can create topics similar to clusters with the only difference that they are not unique
# Ideally meant for when you have more than one document 
wordsWithSentiments$document <- "#SchoolFeeLoot"
wordsMatrix <- cast_dtm(wordsWithSentiments, document, word, n)
library(topicmodels)
wordsLda <- LDA(wordsMatrix, k = 3, control = list(seed = 1234))

word_topics <- tidy(wordsLda, matrix = "beta")

top_terms <- word_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
