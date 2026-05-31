# Live Demo Script
# Decision Making with Sentiment Analysis in R
# Mustafa Asım Ruhi
#
# How to use:
# 1) Open this file in RStudio.
# 2) Click "Source" to run everything at once.
# 3) For presentation practice, you can also run section by section with Cmd + Enter.

# -------------------------------------------------------------------------
# 0. Load packages
# -------------------------------------------------------------------------

required_packages <- c("tidyverse", "janitor", "tidytext", "scales")

missing_packages <- required_packages[!required_packages %in% rownames(installed.packages())]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(tidyverse)
library(janitor)
library(tidytext)
library(scales)

# -------------------------------------------------------------------------
# 1. Import data
# -------------------------------------------------------------------------

url <- paste0(
  "https://raw.githubusercontent.com/",
  "ruchitgandhi/Twitter-Airline-Sentiment-Analysis/",
  "master/Tweets.csv"
)

tweets_raw <- read_csv(url, show_col_types = FALSE)

cat("\n--- 1. Raw dataset structure ---\n")
glimpse(tweets_raw)

# -------------------------------------------------------------------------
# 2. Clean and select relevant variables
# -------------------------------------------------------------------------

tweets <- tweets_raw |>
  clean_names() |>
  select(
    airline_sentiment,
    negativereason,
    airline,
    text,
    retweet_count
  )

cat("\n--- 2. Cleaned dataset structure ---\n")
glimpse(tweets)

# -------------------------------------------------------------------------
# 3. General sentiment distribution
# -------------------------------------------------------------------------

sentiment_distribution <- tweets |>
  count(airline_sentiment) |>
  mutate(share = percent(n / sum(n)))

cat("\n--- 3. Sentiment distribution ---\n")
print(sentiment_distribution)

# -------------------------------------------------------------------------
# 4. Sentiment distribution by airline
# -------------------------------------------------------------------------

sentiment_by_airline <- tweets |>
  count(airline, airline_sentiment) |>
  group_by(airline) |>
  mutate(share = n / sum(n)) |>
  ungroup()

cat("\n--- 4. Sentiment by airline table ---\n")
print(sentiment_by_airline)

sentiment_by_airline |>
  ggplot(aes(x = airline, y = share, fill = airline_sentiment)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Sentiment Distribution by Airline",
    x = "Airline",
    y = "Share of tweets",
    fill = "Sentiment"
  ) +
  theme_minimal()

# -------------------------------------------------------------------------
# 5. Negative reasons
# -------------------------------------------------------------------------

negative_reasons <- tweets |>
  filter(airline_sentiment == "negative", !is.na(negativereason)) |>
  count(negativereason, sort = TRUE)

cat("\n--- 5. Negative reasons table ---\n")
print(negative_reasons)

negative_reasons |>
  slice_max(n, n = 8) |>
  ggplot(aes(x = reorder(negativereason, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Main reasons behind negative airline tweets",
    x = "Negative reason",
    y = "Number of tweets"
  ) +
  theme_minimal()

# -------------------------------------------------------------------------
# 6. Text mining and tokenization
# -------------------------------------------------------------------------

tweet_words <- tweets |>
  mutate(tweet_id = row_number()) |>
  select(tweet_id, airline, airline_sentiment, text) |>
  unnest_tokens(word, text) |>
  anti_join(stop_words, by = "word") |>
  filter(
    str_detect(word, "[a-z]"),
    !str_detect(word, "^http"),
    !str_detect(word, "^t.co")
  )

cat("\n--- 6. Most frequent words after tokenization ---\n")
tweet_words |>
  count(word, sort = TRUE) |>
  head(20) |>
  print()

# -------------------------------------------------------------------------
# 7. Lexicon-based sentiment analysis with Bing
# -------------------------------------------------------------------------

bing <- get_sentiments("bing")

word_sentiments <- tweet_words |>
  inner_join(bing, by = "word")

cat("\n--- 7. Positive and negative word counts with Bing lexicon ---\n")
word_sentiments |>
  count(sentiment, sort = TRUE) |>
  print()

# -------------------------------------------------------------------------
# 8. Most frequent positive and negative sentiment words
# -------------------------------------------------------------------------

top_sentiment_words <- word_sentiments |>
  count(sentiment, word, sort = TRUE) |>
  group_by(sentiment) |>
  slice_max(n, n = 8) |>
  ungroup()

cat("\n--- 8. Top sentiment words ---\n")
print(top_sentiment_words)

top_sentiment_words |>
  mutate(word = reorder(word, n)) |>
  ggplot(aes(x = word, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip() +
  labs(
    title = "Most frequent positive and negative sentiment words",
    x = "Word",
    y = "Frequency"
  ) +
  theme_minimal()

# -------------------------------------------------------------------------
# 9. Lexicon-based sentiment score by airline
# -------------------------------------------------------------------------

lexicon_airline_sentiment <- word_sentiments |>
  count(airline, sentiment) |>
  pivot_wider(
    names_from = sentiment,
    values_from = n,
    values_fill = 0
  ) |>
  mutate(
    lexicon_score = positive - negative,
    total_sentiment_words = positive + negative,
    lexicon_score_per_100_words = 100 * lexicon_score / total_sentiment_words
  ) |>
  arrange(lexicon_score_per_100_words)

cat("\n--- 9. Lexicon-based sentiment score by airline ---\n")
print(lexicon_airline_sentiment)

lexicon_airline_sentiment |>
  ggplot(
    aes(
      x = reorder(airline, lexicon_score_per_100_words),
      y = lexicon_score_per_100_words
    )
  ) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Lexicon-based sentiment score by airline",
    x = "Airline",
    y = "Sentiment score per 100 words"
  ) +
  theme_minimal()

# -------------------------------------------------------------------------
# 10. From analysis to managerial action
# -------------------------------------------------------------------------

decision_table <- negative_reasons |>
  mutate(
    managerial_action = case_when(
      negativereason == "Customer Service Issue" ~
        "Improve support capacity and service recovery.",
      negativereason == "Late Flight" ~
        "Improve delay communication and punctuality.",
      negativereason == "Cancelled Flight" ~
        "Strengthen rebooking and compensation process.",
      negativereason == "Lost Luggage" ~
        "Improve baggage tracking.",
      negativereason == "Bad Flight" ~
        "Improve in-flight experience.",
      TRUE ~
        "Investigate this category further."
    )
  )

cat("\n--- 10. Decision table ---\n")
decision_table |>
  head(10) |>
  print()

# -------------------------------------------------------------------------
# 11. In-class exercise from the presentation
# -------------------------------------------------------------------------

customer_comments <- tibble(
  text = c(
    "The flight was delayed and the service was terrible",
    "The staff was helpful and the flight was comfortable",
    "My luggage was lost and nobody helped me"
  )
)

exercise_result <- customer_comments |>
  unnest_tokens(word, text) |>
  anti_join(stop_words, by = "word") |>
  inner_join(get_sentiments("bing"), by = "word") |>
  count(sentiment)

cat("\n--- 11. Exercise result ---\n")
print(exercise_result)

cat("\nExercise question:\n")
cat("Which managerial action would be most suitable based on the negative words in the comments?\n\n")
cat("A) Reduce the number of customer support channels\n")
cat("B) Improve delay communication and luggage recovery processes\n")
cat("C) Ignore negative comments because they are unstructured data\n")
cat("D) Only focus on positive words\n\n")
cat("Correct answer: B) Improve delay communication and luggage recovery processes\n")

# -------------------------------------------------------------------------
# End of live demo
# -------------------------------------------------------------------------

cat("\nLive demo completed successfully.\n")
