# ============================================================
# Student Exercise: Lexicon-Based Sentiment Analysis in R
# ============================================================

# Install packages if needed
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(tidytext)) install.packages("tidytext")

# Load packages
library(tidyverse)
library(tidytext)

# 1. Create three short customer comments
customer_comments <- tibble(
  text = c(
    "The flight was delayed and the service was terrible",
    "The staff was helpful and the flight was comfortable",
    "My luggage was lost and nobody helped me"
  )
)

# 2. Count positive and negative words
sentiment_count <- customer_comments |>
  unnest_tokens(word, text) |>
  anti_join(stop_words, by = "word") |>
  inner_join(get_sentiments("bing"), by = "word") |>
  count(sentiment)

print(sentiment_count)

# 3. Show which words were matched as positive or negative
matched_words <- customer_comments |>
  unnest_tokens(word, text) |>
  anti_join(stop_words, by = "word") |>
  inner_join(get_sentiments("bing"), by = "word") |>
  select(word, sentiment) |>
  arrange(sentiment, word)

print(matched_words)

# 4. Exercise question
cat("\nQuestion:\n")
cat("Which managerial action would be most suitable based on the negative words in the comments?\n\n")
cat("A) Reduce the number of customer support channels\n")
cat("B) Improve delay communication and luggage recovery processes\n")
cat("C) Ignore negative comments because they are unstructured data\n")
cat("D) Only focus on positive words\n")