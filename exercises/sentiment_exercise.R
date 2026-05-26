# In-class exercise: Decision Making with Sentiment Analysis in R
# Run the code and answer the question at the end.

if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, tidytext)

customer_comments <- tibble(
  text = c(
    "The flight was delayed and the service was terrible",
    "The staff was helpful and the flight was comfortable",
    "My luggage was lost and nobody helped me"
  )
)

result <- customer_comments |>
  unnest_tokens(word, text) |>
  anti_join(stop_words, by = "word") |>
  inner_join(get_sentiments("bing"), by = "word") |>
  count(sentiment)

result

# Question:
# Which managerial action would be most suitable based on the negative words?
# A) Reduce the number of customer support channels
# B) Improve delay communication and luggage recovery processes
# C) Ignore negative comments because they are unstructured data
# D) Only focus on positive words
