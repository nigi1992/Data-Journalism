
# Block Seminar
# Data Journalism
# Little Project: Trump Tweets


########## Importing & shortening of data ##############

# Set individual directory:
setwd("C:/Users/nadja/Documents/Data Journalism/Little Project")

# Import Trump Tweets before and in office:
Trump_tweets_bf_office <- read.csv("C:/Users/nadja/Documents/Data Journalism/Little Project/realDonaldTrump_bf_office.csv")
Trump_tweets_in_office <- read.csv("C:/Users/nadja/Documents/Data Journalism/Little Project/realDonaldTrump_in_office.csv")

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
# There is small improvement between before office
# and  when he was in office. But both scores are
# still low. But we need to consider that internet
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
# The mean and median show that the average tweet is neutral to
# slightly positive.


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

# Create a word cloud of positive words:
positive_words <- sentiment$element_id[sentiment$ave_sentiment > 0]
positive_text <- cleaning_files_news$text[positive_words]
wordcloud(words = unlist(strsplit(positive_text, "\\s+")), min.freq = 5, scale=c(3,0.5), colors="green")

# Create a word cloud of negative words:
negative_words <- sentiment$element_id[sentiment$ave_sentiment < 0]
negative_text <- cleaning_files_news$text[negative_words]
wordcloud(words = unlist(strsplit(negative_text, "\\s+")), min.freq = 5, scale=c(3,0.5), colors="red")






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


## Plotting:













########### Top topics: structural topic model ################

# Implementation:
library(stm)

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
  prevalence = ~ Time,
  data = Bf_office_dfm_stm$meta,
  max.em.its = 1500,
  init.type = "Spectral")
# This needs a little bit of time.

stm_model_in <- 
  stm(documents = In_office_dfm_stm$documents,
      vocab = In_office_dfm_stm$vocab,
      K = k, # this is the number of topics
      prevalence = ~ Time,
      data = In_office_dfm_stm$meta,
      max.em.its = 1500,
      init.type = "Spectral")
# This needs a little bit of time.

# Labeling Topics:
labelTopics(stm_model_bf)
labelTopics(stm_model_in)

# Using Estimated Predicted Probabilities for each Topic
pre_prob1 <- estimateEffect(1:k ~ s(Time), 
                           stm_model_bf,
                           meta = select(stm_model_bf$meta, Time), 
                           uncertainty = "Global")  

pre_prob2 <- estimateEffect(1:k ~ s(Time), 
                           stm_model_in,
                           meta = select(stm_model_in$meta, Time), 
                           uncertainty = "Global")  







# Plotting:


# Preparations to be able to save the following graph as an EMF in a Word document
library(officer)
library(devEMF)

stm_results <- tempfile(fileext = ".emf")
emf(file = stm_results, width = 8, height = 8)

par(mfrow = c(5, 1), mar = c(3, 4, 1, .5))
for (i in 1:k) {
  plot.estimateEffect(pre_prob, 
                      "mth_sc_19", 
                      method = "continuous", 
                      topics = i, 
                      model = z, 
                      xaxt = "n", 
                      printlegend = FALSE, 
                      xlab = "mth_sc_19",
                      main = paste0("TOPIC ",i,": ",l_f[i] ))
  axis(1, 
       at = seq(0,60,by = 12),
       labels = seq(2019,2024,by = 1))
}

# Closing the device and finalizing the file
dev.off()

# Export the graph
read_docx() %>% 
  body_add_img(src = stm_results, width = 8, height = 8) %>% 
  print(target = "stm_results.docx") %>% 
  invisible()







  
############### End of R-File ##################