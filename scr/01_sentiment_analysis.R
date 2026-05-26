# Decision Making with Sentiment Analysis in R
# Mustafa Asim
# Data Analysis for Decision-Making

if (!require(pacman)) install.packages("pacman")
suppressMessages(pacman::p_unload(all))
pacman::p_load(
  tidyverse,
  janitor,
  tidytext,
  lubridate,
  scales,
  knitr,
  ggrepel
)

rm(list = ls())

folders <- c("scr", "ori", "dta", "fig", "tab", "qmd", "doc", "lit", "tmp", "exercises")
for (folder in folders) dir.create(folder, showWarnings = FALSE)

url <- "https://raw.githubusercontent.com/ruchitgandhi/Twitter-Airline-Sentiment-Analysis/master/Tweets.csv"

tweets_raw <- readr::read_csv(url, show_col_types = FALSE)

tweets <- tweets_raw |>
  janitor::clean_names() |>
  select(
    airline_sentiment,
    negativereason,
    airline,
    retweet_count,
    text,
    tweet_created
  ) |>
  mutate(
    tweet_created = lubridate::ymd_hms(tweet_created),
    airline_sentiment = factor(
      airline_sentiment,
      levels = c("negative", "neutral", "positive")
    )
  )

# Basic inspection
glimpse(tweets)
summary(tweets)

# Sentiment distribution by airline
sentiment_by_airline <- tweets |>
  count(airline, airline_sentiment) |>
  group_by(airline) |>
  mutate(share = n / sum(n)) |>
  ungroup()

plot_sentiment_airline <- ggplot(
  sentiment_by_airline,
  aes(x = airline, y = share, fill = airline_sentiment)
) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Customer sentiment differs across airlines",
    subtitle = "Share of negative, neutral and positive tweets",
    x = "Airline",
    y = "Share of tweets",
    fill = "Sentiment"
  ) +
  theme_minimal()

plot_sentiment_airline

ggsave(
  "fig/sentiment_by_airline.png",
  plot = plot_sentiment_airline,
  width = 8,
  height = 5,
  dpi = 300
)

# Main reasons for negative sentiment
negative_reasons <- tweets |>
  filter(airline_sentiment == "negative", !is.na(negativereason)) |>
  count(negativereason, sort = TRUE)

plot_negative_reasons <- negative_reasons |>
  slice_max(n, n = 8) |>
  ggplot(aes(x = reorder(negativereason, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Main reasons behind negative customer sentiment",
    x = "Negative reason",
    y = "Number of tweets"
  ) +
  theme_minimal()

plot_negative_reasons

ggsave(
  "fig/negative_reasons.png",
  plot = plot_negative_reasons,
  width = 8,
  height = 5,
  dpi = 300
)

# Decision table: top three complaint reasons per airline
decision_table <- tweets |>
  filter(airline_sentiment == "negative", !is.na(negativereason)) |>
  count(airline, negativereason, sort = TRUE) |>
  group_by(airline) |>
  slice_max(n, n = 3) |>
  ungroup()

# Tokenization and lexicon-based sentiment analysis
tweets_words <- tweets |>
  mutate(tweet_id = row_number()) |>
  select(tweet_id, airline, airline_sentiment, text) |>
  tidytext::unnest_tokens(word, text) |>
  anti_join(tidytext::stop_words, by = "word") |>
  filter(
    str_detect(word, "[a-z]"),
    !str_detect(word, "^http"),
    !str_detect(word, "^t.co")
  )

bing_lexicon <- tidytext::get_sentiments("bing")

word_sentiments <- tweets_words |>
  inner_join(bing_lexicon, by = "word")

top_sentiment_words <- word_sentiments |>
  count(sentiment, word, sort = TRUE) |>
  group_by(sentiment) |>
  slice_max(n, n = 10) |>
  ungroup()

plot_top_sentiment_words <- top_sentiment_words |>
  mutate(word = tidytext::reorder_within(word, n, sentiment)) |>
  ggplot(aes(x = word, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free_y") +
  tidytext::scale_x_reordered() +
  coord_flip() +
  labs(
    title = "Most frequent positive and negative words",
    subtitle = "Lexicon-based sentiment analysis using the Bing dictionary",
    x = "Word",
    y = "Frequency"
  ) +
  theme_minimal()

plot_top_sentiment_words

ggsave(
  "fig/top_sentiment_words.png",
  plot = plot_top_sentiment_words,
  width = 8,
  height = 5,
  dpi = 300
)

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

plot_lexicon_airline <- ggplot(
  lexicon_airline_sentiment,
  aes(
    x = reorder(airline, lexicon_score_per_100_words),
    y = lexicon_score_per_100_words
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Lexicon-based sentiment score by airline",
    subtitle = "Positive words minus negative words per 100 sentiment words",
    x = "Airline",
    y = "Sentiment score"
  ) +
  theme_minimal()

plot_lexicon_airline

ggsave(
  "fig/lexicon_sentiment_by_airline.png",
  plot = plot_lexicon_airline,
  width = 8,
  height = 5,
  dpi = 300
)

# Translate complaint categories into managerial actions
decision_actions <- negative_reasons |>
  mutate(
    managerial_action = case_when(
      negativereason == "Customer Service Issue" ~ "Increase service capacity and improve service recovery.",
      negativereason == "Late Flight" ~ "Improve punctuality monitoring and delay communication.",
      negativereason == "Can't Tell" ~ "Manually review ambiguous complaints before acting.",
      negativereason == "Cancelled Flight" ~ "Strengthen rebooking and compensation processes.",
      negativereason == "Lost Luggage" ~ "Improve baggage tracking and recovery communication.",
      negativereason == "Bad Flight" ~ "Monitor and improve the in-flight customer experience.",
      negativereason == "Flight Booking Problems" ~ "Optimize booking interface and booking support.",
      negativereason == "Flight Attendant Complaints" ~ "Train cabin crew communication and service recovery.",
      negativereason == "longlines" ~ "Improve airport queue and counter management.",
      negativereason == "Damaged Luggage" ~ "Improve luggage handling and compensation workflow.",
      TRUE ~ "Investigate this complaint category further."
    )
  )

# Export tables for checking and presentation use
write_csv(tweets, "dta/tweets_clean.csv")
write_csv(tweets_words, "dta/tweets_words.csv")
write_csv(word_sentiments, "dta/word_sentiments.csv")
write_csv(sentiment_by_airline, "tab/sentiment_by_airline.csv")
write_csv(negative_reasons, "tab/negative_reasons.csv")
write_csv(decision_table, "tab/decision_table.csv")
write_csv(top_sentiment_words, "tab/top_sentiment_words.csv")
write_csv(lexicon_airline_sentiment, "tab/lexicon_airline_sentiment.csv")
write_csv(decision_actions, "tab/decision_actions.csv")
