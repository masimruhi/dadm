# Live demo: Decision Making with Sentiment Analysis in R
# Mustafa Asım Ruhi

# 1. Load packages --------------------------------------------------------

library(tidyverse)
library(janitor)
library(tidytext)
library(scales)

# 2. Import data ----------------------------------------------------------

url <- "https://raw.githubusercontent.com/ruchitgandhi/Twitter-Airline-Sentiment-Analysis/master/Tweets.csv"

tweets_raw <- read_csv(url, show_col_types = FALSE)

glimpse(tweets_raw)

# 3. Clean and select relevant variables ---------------------------------

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

# 4. Quick overview: sentiment distribution -------------------------------

tweets |>
  count(airline_sentiment) |>
  mutate(share = percent(n / sum(n)))

# 5. Decision question: Why are customers unhappy? ------------------------

negative_reasons <- tweets |>
  filter(airline_sentiment == "negative", !is.na(negativereason)) |>
  count(negativereason, sort = TRUE)

negative_reasons

# 6. Visualize the main negative reasons ---------------------------------

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

# 7. Text mining: turn tweets into words ---------------------------------

tweet_words <- tweets |>
  mutate(tweet_id = row_number()) |>
  select(tweet_id, airline, airline_sentiment, text) |>
  unnest_tokens(word, text) |>
  anti_join(stop_words, by = "word")

tweet_words |>
  count(word, sort = TRUE) |>
  head(20)

# 8. Lexicon-based sentiment analysis ------------------------------------

bing <- get_sentiments("bing")

word_sentiments <- tweet_words |>
  inner_join(bing, by = "word")

word_sentiments |>
  count(sentiment, sort = TRUE)

# 9. Most frequent sentiment words ---------------------------------------

word_sentiments |>
  count(sentiment, word, sort = TRUE) |>
  group_by(sentiment) |>
  slice_max(n, n = 8) |>
  ungroup()

top_sentiment_words |>
  mutate(word = reorder(word, n)) |>
  ggplot(aes(x = word, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip() +
  labs(x = "Word", y = "Frequency") +
  theme_minimal()

# 10. Translate analysis into managerial actions --------------------------

decision_table <- negative_reasons |>
  mutate(
    managerial_action = case_when(
      negativereason == "Customer Service Issue" ~ "Improve support capacity and service recovery.",
      negativereason == "Late Flight" ~ "Improve delay communication and punctuality.",
      negativereason == "Cancelled Flight" ~ "Strengthen rebooking and compensation process.",
      negativereason == "Lost Luggage" ~ "Improve baggage tracking.",
      negativereason == "Bad Flight" ~ "Improve in-flight experience.",
      TRUE ~ "Investigate this category further."
    )
  )

decision_table |>
  head(10)
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

---
library(tidyverse)
library(tidytext)

customer_comments <- tibble(
  text = c(
    "The flight was delayed and the service was terrible",
    "The staff was helpful and the flight was comfortable",
    "My luggage was lost and nobody helped me"
  )
)

customer_comments |>
  unnest_tokens(word, text) |>
  anti_join(stop_words, by = "word") |>
  inner_join(get_sentiments("bing"), by = "word") |>
  count(sentiment)

