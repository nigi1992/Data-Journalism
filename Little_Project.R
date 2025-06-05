
# Block Seminar
# Data Journalism
# Little Project: Trump Tweets


########## Importing & shortening of data ##############

# Set individual directory:
setwd("C:/Users/nadja/Documents/Data Journalism/Little Project")

# Import Trump Tweets before and in office:
Trump_tweets_bf_office <- read.csv("C:/Users/nadja/Documents/Data Journalism/Little Project/realDonaldTrump_bf_office.csv")
Trump_tweets_in_office <- read.csv("C:/Users/nadja/Documents/Data Journalism/Little Project/realDonaldTrump_in_office.csv")

#Trump_tweets_bf_office <- read.csv('/Users/nicolaswaser/New-project-GitHub-first/R/Data Journalism/Data Sets for Projects/realDonaldTrump_bf_office.csv')
#Trump_tweets_in_office <- read.csv('/Users/nicolaswaser/New-project-GitHub-first/R/Data Journalism/Data Sets for Projects/realDonaldTrump_in_office.csv')
  
# View time window:
range(Trump_tweets_bf_office$Time, na.rm = TRUE) # " 2009-05-04 13:54" " 2017-01-19 22:24"
range(Trump_tweets_in_office$Time, na.rm = TRUE) # " 2017-01-20 06:31" " 2021-01-08 23:44"

# Shorten Trump_tweets_bf_office to 4 years to make it comparable with Trump_tweets_in_office:
Trump_tweets_bf_office <- Trump_tweets_bf_office[Trump_tweets_bf_office$Time >= as.POSIXct("2013-01-11 01:00") & Trump_tweets_bf_office$Time <= as.POSIXct("2017-01-19 22:24"), ]


############# Additional cleaning #################

# Load corresponding packages:
library(jsonlite)
library(tibble)
library(rvest)
library(dplyr)
library(stringr)
library(polite)

# Adding new column with cleaned text for the analysis:
Trump_tweets_bf_office <- Trump_tweets_bf_office |>
  filter(!Tweet.Text %in% c("", " ", "___")) |>
  mutate(Tweet.Text.Cleaned = gsub("[[:punct:]]", "", Tweet.Text)) |>
  mutate(Tweet.Text.Cleaned = tolower(Tweet.Text.Cleaned)) |>
  mutate(Tweet.Text.Cleaned = str_trim(Tweet.Text.Cleaned)) |>
  mutate(Tweet.Text.Cleaned = str_squish(Tweet.Text.Cleaned)) |>
  mutate(Tweet.Text.Cleaned = gsub("\\n", " ", Tweet.Text.Cleaned)) |>
  filter(nchar(Tweet.Text.Cleaned) > 0)

Trump_tweets_in_office <- Trump_tweets_in_office |>
  filter(!Tweet.Text %in% c("", " ", "___")) |>
  mutate(Tweet.Text.Cleaned = gsub("[[:punct:]]", "", Tweet.Text)) |>
  mutate(Tweet.Text.Cleaned = tolower(Tweet.Text.Cleaned)) |>
  mutate(Tweet.Text.Cleaned = str_trim(Tweet.Text.Cleaned)) |>
  mutate(Tweet.Text.Cleaned = str_squish(Tweet.Text.Cleaned)) |>
  mutate(Tweet.Text.Cleaned = gsub("\\n", " ", Tweet.Text.Cleaned)) |>
  filter(nchar(Tweet.Text.Cleaned) > 0)


########## Construct the text corpus ##########

# Load package:
#install.packages("quanteda")
library(quanteda)

# Construct corpus:
Bf_office_corpus <- corpus(Trump_tweets_bf_office, text_field = "Tweet.Text.Cleaned")
In_office_corpus <- corpus(Trump_tweets_in_office, text_field = "Tweet.Text.Cleaned")

# Check data:
head(docvars(Bf_office_corpus))
head(docvars(In_office_corpus))

Bf_office_corpus[1:3]
In_office_corpus[1:3]


##########################################
########### Analysis part ################
##########################################

########### Key words in context: Tariffs ################

### Without tokens:

# example media tweets:
tweet_about_media = str_detect(Bf_office_corpus, "CNN|media|fake news")
# Count the number of tweets about media:
sum(tweet_about_media) # 431
# Show the first 5 tweets about media:
Bf_office_corpus[tweet_about_media][1:5]

## tweets about tariffs:

# before office:
tweet_about_tariffs_bf = str_detect(Bf_office_corpus, "tariff|tariffs|trade|trade war|protectionis|deficit|surplus|barrier|imbalance|NAFTA|nafta")
# Count the number of tweets about tariffs:
sum(tweet_about_tariffs_bf) # 142
# Show the first 5 tweets about tariffs:
Bf_office_corpus[tweet_about_tariffs_bf][1:5]

# in office:
tweet_about_tariffs_in = str_detect(In_office_corpus, "tariff|tariffs|trade|trade war|protectionis|deficit|surplus|barrier|imbalance|NAFTA|nafta")
# Count the number of tweets about tariffs:
sum(tweet_about_tariffs_in) # 611
# Show the first 5 tweets about tariffs:
In_office_corpus[tweet_about_tariffs_in][1:5]

# Plot Number of Tweets about Tariffs before and in Office
library(ggplot2)
ggplot(data = data.frame(
  Period = c("Before Office", "In Office"),
  Count = c(sum(tweet_about_tariffs_bf), sum(tweet_about_tariffs_in))
)) +
  geom_bar(aes(x = Period, y = Count), stat = "identity", fill = "steelblue") +
  labs(title = "Number of Tweets about Tariffs",
       x = "Period",
       y = "Count of Tweets") +
  theme_bw()
ggsave("trump_tariffs_tweets_plot_simple.png", width = 8, height = 6, dpi = 300)




### With simple tokens:

tokens_bf <- tokens(Bf_office_corpus)
tokens_in <- tokens(In_office_corpus)

kwic(tokens_bf, pattern = "tariff", window = 5)
kwic(tokens_in, pattern = "tariff", window = 5)

kwic(tokens_bf, pattern = "tariffs", window = 5)
kwic(tokens_in, pattern = "tariffs", window = 5)

kwic(tokens_bf, pattern = "tariff*", window = 5)
kwic(tokens_in, pattern = "tariff*", window = 5)


# search for "tariffs" in context
kwic_tariffs_bf <- kwic(tokens_bf, pattern = "tariff*", window = 5)
kwic_tariffs_in <- kwic(tokens_in, pattern = "tariff*", window = 5)

# view results
head(kwic_tariffs_bf)
head(kwic_tariffs_in)

# creating a document-feature matrix
dfm_bf <- dfm(tokens_bf)
dfm_in <- dfm(tokens_in)

# count occurrences
topfeatures(dfm_select(dfm_bf, pattern = "tariff*"))
topfeatures(dfm_select(dfm_in, pattern = "tariff*"))


### With cleaned tokens:

head(Bf_office_tokens)
head(In_office_tokens)

# DFM (document-feature matrix) 
Bf_office_dfm <- dfm(Bf_office_tokens)
In_office_dfm <- dfm(In_office_tokens)

str(Bf_office_dfm)
str(In_office_dfm)


# search for "tariffs" in context
kwic(Bf_office_tokens, pattern = "tariff", window = 5)
kwic(In_office_tokens, pattern = "tariff", window = 5)

kwic(Bf_office_tokens, pattern = "tariffs", window = 5)
kwic(In_office_tokens, pattern = "tariffs", window = 5)

kwic(Bf_office_tokens, pattern = "tariff*", window = 5)
kwic(In_office_tokens, pattern = "tariff*", window = 5)


Bf_office_kwic <- kwic(Bf_office_tokens, pattern = "tariff*", window = 5)
In_office_kwic <- kwic(In_office_tokens, pattern = "tariff*", window = 5)

# view results
head(Bf_office_kwic)
head(In_office_kwic)

topfeatures(dfm_select(Bf_office_dfm, pattern = "tariff*"))
topfeatures(dfm_select(In_office_dfm, pattern = "tariff*"))



### Use more than one keyword for tariffs:

patterns <- c("tariff*", "trade", "protectionis*", "deficit", "surplus", "barrier", "imbalance", "nafta")
# search for multiple keywords in context

Bf_office_kwic_multi <- kwic(Bf_office_tokens, pattern = patterns, window = 5, valuetype = "fixed")
In_office_kwic_multi <- kwic(In_office_tokens, pattern = patterns, window = 5, valuetype = "fixed")

# remove rows with duplicate docnames
Bf_office_kwic_multi <- Bf_office_kwic_multi[!duplicated(Bf_office_kwic_multi$docname), ]
In_office_kwic_multi <- In_office_kwic_multi[!duplicated(In_office_kwic_multi$docname), ]

# view results
head(Bf_office_kwic_multi)
head(In_office_kwic_multi)

# Count occurrences of each keyword in the context
topfeatures(dfm_select(Bf_office_dfm, pattern = patterns))
topfeatures(dfm_select(In_office_dfm, pattern = patterns))


patterns <- c("tariff", "trade", "protectionist", "deficit", "surplus", "barrier", "nafta")

# Count keyword frequencies manually for each dataset
counts_before <- colSums(dfm_select(Bf_office_dfm, pattern = patterns, selection = "keep", valuetype = "fixed"))
counts_in <- colSums(dfm_select(In_office_dfm, pattern = patterns, selection = "keep", valuetype = "fixed"))

# Merge counts into a tidy data frame
keyword_freq <- data.frame(
  Keyword = patterns,
  Count_Before = counts_before[patterns],
  Count_In = counts_in[patterns]
)

# Reshape for ggplot
library(tidyr)
keyword_freq_long <- keyword_freq |>
  pivot_longer(cols = c(Count_Before, Count_In), names_to = "Period", values_to = "Count")

# Plot
ggplot(keyword_freq_long, aes(x = Keyword, y = Count, fill = Period)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Count_Before" = "steelblue", "Count_In" = "firebrick")) +
  labs(title = "Occurrences of Trade-Related Keywords in Trump's Tweets",
       x = "Keyword", y = "Count", fill = "Time Period") +
  theme_bw()
ggsave("trump_tariff_keywords_plot.png", width = 10, height = 6, dpi = 300)

#Bf_office_kwic_multi <- kwic(Bf_office_tokens, pattern = c("tariff*", "trade", "protectionis", "deficit", "surplus", "barrier", "imbalance", "NAFTA", "nafta"), window = 5)
#In_office_kwic_multi <- kwic(In_office_tokens, pattern = c("tariff*", "trade", "protectionis", "deficit", "surplus", "barrier", "imbalance", "NAFTA", "nafta"), window = 5)


########### Flesch score ################

# Load package:
#install.packages("quanteda.textstats")
library(quanteda.textstats)

# Generate flesch scores for before and in office:
flesch_scores_bf <- textstat_readability(Bf_office_corpus, measure = "Flesch")
tweets_bf_readability_scores <- as.numeric(flesch_scores_bf$Flesch)

flesch_scores_in <- textstat_readability(In_office_corpus, measure = "Flesch")
tweets_in_readability_scores <- as.numeric(flesch_scores_in$Flesch)

# Calculate the mean for before and in office:
mean_flesch_bf <- mean(tweets_bf_readability_scores, na.rm = TRUE)
print(paste("The mean Flesch score for the tweets before office is:", mean_flesch_bf))
# = 27.5971178655206

mean_flesch_in <- mean(tweets_in_readability_scores, na.rm = TRUE)
print(paste("The mean Flesch score for the tweets in office is:", mean_flesch_in))
# = 32.0513290073956


## Interpretation:
# The standard of the score is between 60 and 70.
# This means that trumps tweets are harder to read.
# The tweets do not have a high readability score.
# There is small improvement from before office
# to when he was in office. But both scores are
# still low. However, we need to consider that internet
# language in general probably would not get a high
# score from the flesh score.


########### Lexical diversity ################

# First, we need to construct the token object (tokens) from the corpus:
# (for both data sets)
Bf_office_tokens <- tokens(Bf_office_corpus, 
                        what = c("word"),
                        remove_separators = TRUE,
                        include_docvars = TRUE,
                        ngrams = 1L,
                        remove_numbers = FALSE, 
                        remove_punct = FALSE,
                        remove_symbols = FALSE, 
                        remove_hyphens = FALSE)


In_office_tokens <- tokens(In_office_corpus, 
                           what = c("word"),
                           remove_separators = TRUE,
                           include_docvars = TRUE,
                           ngrams = 1L,
                           remove_numbers = FALSE, 
                           remove_punct = FALSE,
                           remove_symbols = FALSE, 
                           remove_hyphens = FALSE)

# Checking:
head(Bf_office_tokens)
head(In_office_tokens)

# Then we need to further clean the tokens:
# (for example like removing stopwords, stemming, empty tokens, etc.)
Bf_office_tokens <- Bf_office_tokens |>
  tokens_remove(pattern = "^[[:punct:]]+$", valuetype = "regex", padding = TRUE) |>
  tokens_tolower() |>
  tokens_remove(stopwords("en"), padding = TRUE) |>
  tokens_wordstem(language = "english") |>
  tokens_remove(pattern = "")

In_office_tokens <- In_office_tokens |>
  tokens_remove(pattern = "^[[:punct:]]+$", valuetype = "regex", padding = TRUE) |>
  tokens_tolower() |>
  tokens_remove(stopwords("en"), padding = TRUE) |>
  tokens_wordstem(language = "english") |>
  tokens_remove(pattern = "")

# Checking:
Bf_office_tokens[1:5]
In_office_tokens[1:5]

# Then we need to construct a DFM (document-feature matrix) out of the token objects:
Bf_office_dfm <- dfm(Bf_office_tokens)
In_office_dfm <- dfm(In_office_tokens)

# Checking:
str(Bf_office_dfm)
str(In_office_dfm)


## Lexical diversity:
Bf_office_lexical_diversity <- textstat_lexdiv(Bf_office_dfm, measure = "all") # Needs 15-20 minutes to work through it.
In_office_lexical_diversity <- textstat_lexdiv(In_office_dfm, measure = "all") # Needs 15-20 minutes to work through it.

# Calculate TTR for the texts in the DFM: 
mean_ttr_bf <- mean(Bf_office_lexical_diversity$TTR, na.rm = TRUE)
mean_ttr_bf # = 0.9815356

mean_ttr_in <- mean(In_office_lexical_diversity$TTR, na.rm = TRUE)
mean_ttr_in # = 0.963751

## Interpretation:

# A high score indicates a high lexical diversity.
# A low score indicates more repetition or limited variation in word use.
# The score is between 0 and 1.
# Both scores are high. This means that the Trump tweets have a high lexical diversity
# and therefore less repetition and different kinds of words.


########### Sentiment score ################

# Load packages:
library(dplyr)
library(sentimentr)

# Get the sentences:
sentences_bf <- get_sentences(Trump_tweets_bf_office$Tweet.Text.Cleaned)
sentences_in <- get_sentences(Trump_tweets_in_office$Tweet.Text.Cleaned)

# Perform sentiment analysis:
sentiment_bf <- sentiment_by(sentences_bf)
head(sentiment_bf)

sentiment_in <- sentiment_by(sentences_in)
head(sentiment_in)

# Summary of sentiment scores:
summary(sentiment_bf$ave_sentiment) 
summary(sentiment_in$ave_sentiment)

# In summary, the sentiment scores in our data both vary
# from more than -1 to around 2 which means that there
# are positive tweets found and also negative ones.
# But there seems to be more positivity than negativity.
# The mean and median show that the average tweet falls 
# somewhere in the range of neutral to slightly positive.


########### Plotting sentiment scores with wordclouds ################




# Wordcloud of positive and negative words:
install.packages("RColorBrewer")
install.packages("topicmodels")
install.packages("tm")
library(RColorBrewer)
library(wordcloud)
library(topicmodels)
library(NLP)
library(tm)

# Example:

# Create a word cloud of positive words:
#positive_words <- sentiment$element_id[sentiment$ave_sentiment > 0]
#positive_text <- cleaning_files_news$text[positive_words]
#wordcloud(words = unlist(strsplit(positive_text, "\\s+")), min.freq = 5, scale=c(3,0.5), colors="green")

# Create a word cloud of negative words:
#negative_words <- sentiment$element_id[sentiment$ave_sentiment < 0]
#negative_text <- cleaning_files_news$text[negative_words]
#wordcloud(words = unlist(strsplit(negative_text, "\\s+")), min.freq = 5, scale=c(3,0.5), colors="red")

# Application:

# Summary of sentiment scores:
summary(sentiment_bf$ave_sentiment) 
summary(sentiment_in$ave_sentiment)

## Before office:

# Create a word cloud of positive words before office:
positive_words_bf <- sentiment_bf$element_id[sentiment_bf$ave_sentiment > 0]
positive_text_bf <- Trump_tweets_bf_office$Tweet.Text.Cleaned[positive_words_bf]
png("trump_wordcloud_positive_bf_office.png", width = 1200, height = 800, res = 150)
#wordcloud(words = unlist(strsplit(positive_text_bf, "\\s+")), min.freq = 5, scale=c(3,0.5), colors="green")
wordcloud(words = unlist(strsplit(positive_text_bf, "\\s+")), min.freq = 10, scale=c(10,0.5), colors=colorRampPalette(c("green4", "green3", "green2", "green1", "greenyellow"))(100))
dev.off() # Close the device to save the image

# Create a word cloud of negative words before office:
negative_words_bf <- sentiment_bf$element_id[sentiment_bf$ave_sentiment < 0]
negative_text_bf <- Trump_tweets_bf_office$Tweet.Text.Cleaned[negative_words_bf]
png("trump_wordcloud_negative_bf_office.png", width = 1200, height = 800, res = 150)
#wordcloud(words = unlist(strsplit(negative_text_bf, "\\s+")), min.freq = 5, scale=c(3,0.5), colors="red")
wordcloud(words = unlist(strsplit(negative_text_bf, "\\s+")), min.freq = 10, scale=c(10,0.5), colors=colorRampPalette(c("red4", "red3", "red2", "red1", "lightcoral"))(100))
dev.off() # Close the device to save the image


## In office:

# Create a word cloud of positive words in office:
positive_words_in <- sentiment_in$element_id[sentiment_in$ave_sentiment > 0]
positive_text_in <- Trump_tweets_in_office$Tweet.Text.Cleaned[positive_words_in]
png("trump_wordcloud_positive_in_office.png", width = 1200, height = 800, res = 150)
#wordcloud(words = unlist(strsplit(positive_text_in, "\\s+")), min.freq = 5, scale=c(3,0.5), colors="green")
wordcloud(words = unlist(strsplit(positive_text_in, "\\s+")), min.freq = 10, scale=c(10,0.5), colors=colorRampPalette(c("green4", "green3", "green2", "green1", "greenyellow"))(100))
dev.off() # Close the device to save the image

# Create a word cloud of negative words in office:
negative_words_in <- sentiment_in$element_id[sentiment_in$ave_sentiment < 0]
negative_text_in <- Trump_tweets_in_office$Tweet.Text.Cleaned[negative_words_in]
png("trump_wordcloud_negative_in_office.png", width = 1200, height = 800, res = 150)
#wordcloud(words = unlist(strsplit(negative_text_in, "\\s+")), min.freq = 5, scale=c(3,0.5), colors="red")
wordcloud(words = unlist(strsplit(negative_text_in, "\\s+")), min.freq = 10, scale=c(10,0.5), colors=colorRampPalette(c("red4", "red3", "red2", "red1", "lightcoral"))(100))
dev.off() # Close the device to save the image


########### Dictionary with curse words ################

# Defining a dictionary with the most used curse words and good words in english:
trump_tweets_dict <- dictionary(list(
  positive = c("love", "awesome", "beautiful", "kind", "happy", "excellent", "fantastic", "amazing",
               "wonderful", "brilliant", "great", "positive", "incredible", "friendly", "delightful",
               "graceful", "joyful", "radiant", "charming", "generous"),
  negative = c("fuck", "shit", "bitch", "asshole", "bastard", "dick", "piss", "cunt", "damn",
               "crap", "prick", "slut", "whore", "cock", "twat", "motherfucker", "bullshit",
               "ass", "faggot", "douche")))

# Construct DFM with the dictionary:
Bf_office_dfm_dict <- dfm_lookup(Bf_office_dfm, dictionary = trump_tweets_dict)
In_office_dfm_dict <- dfm_lookup(In_office_dfm, dictionary = trump_tweets_dict)

# Make it a data frame for easier analysis:
custom_sentiment_bf <- convert(Bf_office_dfm_dict, to = "data.frame")
custom_sentiment_in <- convert(In_office_dfm_dict, to = "data.frame")

# Checking:
print(custom_sentiment_bf)
summary(custom_sentiment_bf)

print(custom_sentiment_in)
summary(custom_sentiment_in)

# There is not much to find.
# If something is found then it is positive words and not negative ones.


# Summarizing the positive and negative words:
custom_sentiment_bf %>%
  summarise(positive = sum(positive, na.rm = TRUE),
            negative = sum(negative, na.rm = TRUE))

custom_sentiment_in %>%
  summarise(positive = sum(positive, na.rm = TRUE),
            negative = sum(negative, na.rm = TRUE))

# As Df frequency tables
word_freq_bf <- textstat_frequency(Bf_office_dfm_dict, n = NULL) %>%
  mutate(period = "Before Office")

word_freq_in <- textstat_frequency(In_office_dfm_dict, n = NULL) %>%
  mutate(period = "In Office")

# Combine and tag sentiment
word_freq_all <- bind_rows(word_freq_bf, word_freq_in) %>%
  mutate(sentiment = case_when(
    feature %in% trump_tweets_dict[["positive"]] ~ "Positive",
    feature %in% trump_tweets_dict[["negative"]] ~ "Negative",
    TRUE ~ "Other"
  )) %>%
  filter(sentiment != "Other")



## Plotting:
library(dplyr)
library(ggplot2)

## Plotting neg. vs. pos. words before and in office
# Step 1: Summarize sentiment counts
sentiment_counts <- bind_rows(
  custom_sentiment_bf %>%
    summarise(positive = sum(positive, na.rm = TRUE),
              negative = sum(negative, na.rm = TRUE)) %>%
    mutate(period = "Before Office"),
  
  custom_sentiment_in %>%
    summarise(positive = sum(positive, na.rm = TRUE),
              negative = sum(negative, na.rm = TRUE)) %>%
    mutate(period = "In Office")
)

# Step 2: Reshape data for ggplot
sentiment_long <- sentiment_counts %>%
  tidyr::pivot_longer(cols = c("positive", "negative"),
                      names_to = "sentiment",
                      values_to = "count")

# Step 3: Plot
ggplot(sentiment_long, aes(x = period, y = count, fill = sentiment)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("positive" = "steelblue", "negative" = "firebrick")) +
  labs(
    title = "Positive vs Negative Words in Trump Tweets",
    x = "Period",
    y = "Word Count",
    fill = "Sentiment"
  ) +
  theme_bw(base_size = 14)
# Step 4: Save the plot
ggsave("trump_sentiment_frequency_plot_simple.png", width = 8, height = 6, dpi = 300)



## Plotting most frequent neg. and pos. words 

# Extract the dictionary words present in the DFMs
dict_words <- unlist(trump_tweets_dict)
dict_words <- dict_words[dict_words %in% featnames(dfm_bf) | dict_words %in% featnames(dfm_in)]

# Subset the DFMs to include only dictionary words
dfm_bf_dict <- dfm_select(dfm_bf, pattern = dict_words)
dfm_in_dict <- dfm_select(dfm_in, pattern = dict_words)

# Calculate word frequencies
freq_bf <- textstat_frequency(dfm_bf_dict) %>%
  mutate(period = "Before Office")

freq_in <- textstat_frequency(dfm_in_dict) %>%
  mutate(period = "In Office")

# Combine the frequency data and add sentiment labels
freq_combined <- bind_rows(freq_bf, freq_in) %>%
  mutate(sentiment = case_when(
    feature %in% trump_tweets_dict$positive ~ "Positive",
    feature %in% trump_tweets_dict$negative ~ "Negative"
  ))

# Plot the frequencies using ggplot2
ggplot(freq_combined, aes(x = reorder(feature, frequency), y = frequency, fill = sentiment)) +
  geom_bar(stat = "identity") +
  facet_wrap(~period, scales = "free_y") +
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "steelblue", "Negative" = "firebrick")) +
  labs(
    title = "Frequency of Sentiment Words in Trump's Tweets",
    x = "Word",
    y = "Frequency",
    fill = "Sentiment"
  ) +
  theme_bw(base_size = 14)
# Save the plot
ggsave("trump_sentiment_word_frequency_plot.png", width = 10, height = 8, dpi = 300)


########### Top topics: structural topic model ################

# Implementation:
library(stm)

library(lubridate)
Trump_tweets_bf_office$Time_Coarse <- floor_date(as.POSIXct(Trump_tweets_bf_office$Time), unit = "month")
Trump_tweets_in_office$Time_Coarse <- floor_date(as.POSIXct(Trump_tweets_in_office$Time), unit = "month")

# Converting the DFM to a DTM:
Bf_office_dfm_stm <- 
  Bf_office_dfm |> 
  dfm_trim(, sparsity = 0.999) |>
  convert(, to = "stm")


In_office_dfm_stm <- 
  In_office_dfm |> 
  dfm_trim(, sparsity = 0.999) |>
  convert(, to = "stm")


# Building the STM Model:
k = 5 # number of topics

stm_model_bf <- 
  stm(documents = Bf_office_dfm_stm$documents,
  vocab = Bf_office_dfm_stm$vocab,
  K = k, # this is the number of topics
  #prevalence = ~ Time,
  #prevalence = ~ s(as.numeric(Time)),
  data = Bf_office_dfm_stm$meta,
  max.em.its = 1500,
  init.type = "Spectral")
# This needs a little bit of time.

stm_model_in <- 
  stm(documents = In_office_dfm_stm$documents,
      vocab = In_office_dfm_stm$vocab,
      K = k, # this is the number of topics
      #prevalence = ~ Time,
      #prevalence = ~ s(as.numeric(Time)),
      data = In_office_dfm_stm$meta,
      max.em.its = 1500,
      init.type = "Spectral")
# This needs a little bit of time.

# Labeling Topics:
labelTopics(stm_model_bf)

#Topic 1 Top Words:
#Highest Prob: get, go, peopl, one, want, know, never 
#FREX: go, birthday, fire, never, nbc, enough, cant 
#Lift: birthday, premier, schneiderman, albert, einstein, episod, omarosa 
#Score: birthday, go, peopl, one, get, never, cant 
#Topic 2 Top Words:
#Highest Prob: just, good, think, obama, big, work, poll 
#FREX: poll, hard, luck, jeb, cours, fox, news 
#Lift: 1000, abc, bird, succeed, ugli, alexsalmond, chris 
#Score: luck, think, poll, good, obama, big, just 
#Topic 3 Top Words:
#Highest Prob: realdonaldtrump, trump, thank, great, presid, donald, run 
#FREX: thank, presid, donald, run, mr, pleas, 2016 
#Lift: admir, ass, brandiglanvill, cute, genius, mikeandmik, style 
#Score: realdonaldtrump, happi, thank, trump, presid, run, donald 
#Topic 4 Top Words:
#Highest Prob: ä, make, america, us, now, see, back 
#FREX: join, deal, carolina, togeth, south, book, read 
#Lift: achiev, americafirst, copi, icymi, imwithyou, indiana, korea 
#Score: ä, america, make, us, howardstern, back, now 
#Topic 5 Top Words:
#Highest Prob: like, time, new, look, via, job, dont 
#FREX: crook, york, fail, lie, pay, ad, global 
#Lift: global, maralago, nytim, 3rd, berni, judgement, sander 
#Score: 3rd, new, hillari, bad, time, via, like 


labelTopics(stm_model_in)

#Topic 1 Top Words:
#Highest Prob: democrat, countri, us, year, work, good, much 
#FREX: whitehous, countri, secur, power, fund, terribl, use 
#Lift: danscavino, jasoninthehous, oh, ignor, jimjordan, repdougcollin, melania 
#Score: danscavino, democrat, countri, us, work, whitehous, good 
#Topic 2 Top Words:
#Highest Prob: presid, trump, american, vote, job, 🄠, like 
#FREX: 🄠, american, poll, run, schiff, presid, person 
#Lift: harass, 🄠, absente, incom, admin, bishop, mayb 
#Score: harass, presid, trump, 🄠, american, vote, job 
#Topic 3 Top Words:
#Highest Prob: realdonaldtrump, thank, now, time, fake, make, today 
#FREX: china, dollar, thank, militari, famili, time, stop 
#Lift: thanksgiv, patrol, china, twitter, die, minist, account 
#Score: thanksgiv, now, today, thank, time, realdonaldtrump, make 
#Topic 4 Top Words:
#Highest Prob: rt, amp, news, republican, go, senat, hous 
#FREX: law, washington, polic, republican, peac, hoax, can 
#Lift: drain, fisa, rudygiuliani, peac, chuck, dossier, page 
#Score: rt, amp, drain, republican, can, go, say 
#Topic 5 Top Words:
#Highest Prob: great, peopl, state, get, just, want, america 
#FREX: never, way, night, impeach, peopl, two, book 
#Lift: transit, improv, hate, becom, night, manag, never 
#Score: great, transit, peopl, get, never, state, america 


## Plotting:
png("top_topics_bf_plot.png", width = 2000, height = 1500, res = 100)
plot(stm_model_bf, type = "summary")
dev.off()

png("top_topics_in_plot.png", width = 2000, height = 1500, res = 100)
plot(stm_model_in, type = "summary")
dev.off()

  
############### End of R-File ##################