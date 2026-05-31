
# ============================================================
# Live Demo: Decision Making with Sentiment Analysis in R
# ============================================================

# Install packages if needed
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(janitor)) install.packages("janitor")
if (!require(tidytext)) install.packages("tidytext")
if (!require(scales)) install.packages("scales")

# Load packages
library(tidyverse)
library(janitor)
library(tidytext)
library(scales)

# ------------------------------------------------------------
# 1. Import the dataset
# ------------------------------------------------------------

url <- "https://raw.githubusercontent.com/ruchitgandhi/Twitter-Airline-Sentiment-Analysis/master/Tweets.csv"

tweets_raw <- read_csv(url, show_col_types = FALSE)

glimpse(tweets_raw)

# ------------------------------------------------------------
# 2. Clean and select relevant columns
# ------------------------------------------------------------

tweets <- tweets_raw |>
  clean_names() |>
  select(
    airline_sentiment,
    negativereason,
    airline,
    text,
    retweet_count
  )

glimpse(tweets)

# ------------------------------------------------------------
# 3. Sentiment distribution
# ------------------------------------------------------------

tweets |>
  count(airline_sentiment) |>
  mutate(share = percent(n / sum(n)))

# ------------------------------------------------------------
# 4. Sentiment distribution by airline
# ------------------------------------------------------------

sentiment_by_airline <- tweets |>
  count(airline, airline_sentiment) |>
  group_by(airline) |>
  mutate(share = n / sum(n)) |>
  ungroup()

sentiment_by_airline

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

# ------------------------------------------------------------
# 5. Main negative reasons
# ------------------------------------------------------------

negative_reasons <- tweets |>
  filter(airline_sentiment == "negative", !is.na(negativereason)) |>
  count(negativereason, sort = TRUE)

negative_reasons

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

# ------------------------------------------------------------
# 6. Tokenization
# ------------------------------------------------------------

tweet_words <- tweets |>
  mutate(tweet_id = row_number()) |>
  select(tweet_id, airline, airline_sentiment, text) |>
  unnest_tokens(word, text) |>
  anti_join(stop_words, by = "word")

tweet_words |>
  count(word, sort = TRUE) |>
  head(20)

# ------------------------------------------------------------
# 7. Lexicon-based sentiment analysis with Bing
# ------------------------------------------------------------

bing <- get_sentiments("bing")

word_sentiments <- tweet_words |>
  inner_join(bing, by = "word")

word_sentiments |>
  count(sentiment, sort = TRUE)

# ------------------------------------------------------------
# 8. Most frequent positive and negative words
# ------------------------------------------------------------

top_sentiment_words <- word_sentiments |>
  count(sentiment, word, sort = TRUE) |>
  group_by(sentiment) |>
  slice_max(n, n = 8) |>
  ungroup()

top_sentiment_words

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

# ------------------------------------------------------------
# 9. Sentiment score by airline
# ------------------------------------------------------------

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

lexicon_airline_sentiment

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
    y = "Score per 100 words"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# 10. From analysis to managerial actions
# ------------------------------------------------------------

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

decision_table |>
  head(10)

# ------------------------------------------------------------
# 11. In-class exercise
# ------------------------------------------------------------

customer_comments <- tibble(
  text = c(
    "The flight was delayed and the service was terrible",
    "The staff was helpful and the flight was comfortable",
    "My luggage was lost and nobody helped me"
  )
)

# Count positive and negative words
customer_comments |>
  unnest_tokens(word, text) |>
  anti_join(stop_words, by = "word") |>
  inner_join(get_sentiments("bing"), by = "word") |>
  count(sentiment)

# Show which words were matched as positive or negative
customer_comments |>
  unnest_tokens(word, text) |>
  anti_join(stop_words, by = "word") |>
  inner_join(get_sentiments("bing"), by = "word") |>
  select(word, sentiment) |>
  arrange(sentiment, word)

