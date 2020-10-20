library(tidyverse)
library(tidytext)

# Jane Austen

library(janeaustenr)

tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number()) %>% 
  ungroup() %>%
  unnest_tokens(word, text)

jane_austen_sentiment <- tidy_books %>%
  inner_join(get_sentiments("bing"), by="word") %>%
  count(book, index=linenumber %/% 80, sentiment) %>%  # Count group members
  pivot_wider(names_from=sentiment, values_from=n, values_fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  theme_minimal() +  
  facet_wrap(~book, ncol = 2, scales = "free_x") +
  labs(title="Jane Austen's books")
