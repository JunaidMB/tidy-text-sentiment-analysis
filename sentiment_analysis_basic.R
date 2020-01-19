library(tidytext)
library(textdata)
library(tidyverse)
library(janeaustenr)
library(stringr)
library(wordcloud)
library(reshape2)

# Austen books sentiment analysis
tidy_books <- austen_books() %>% 
  group_by(book) %>% 
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>% 
  ungroup() %>% 
  unnest_tokens(word, text)

## Use the NRC lexicon for sentiment analysis

nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

tidy_books %>% 
  filter(book == "Emma") %>% 
  inner_join(nrc_joy) %>% 
  count(word, sort = TRUE)

# Use Bing for sentiment analysis
jane_austen_sentiment <- tidy_books %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(book, index = linenumber %/% 80, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

# Plot sentiment scores
ggplot(data = jane_austen_sentiment, mapping = aes(x = index, y = sentiment, fill = book)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~book, ncol = 2, scales = "free_x")
  
# Comparing the 3 sentiment dictionaries
## Examine sentiment changes across the narrative arcs of Pride and Prejudice
pride_prejudice <- tidy_books %>% 
  filter(book == "Pride & Prejudice")

pride_prejudice 

## Use inner_join to calculate sentiment in different ways
afinn <- pride_prejudice %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 80) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(pride_prejudice %>% 
                            inner_join(get_sentiments("bing")) %>% 
                            mutate(method = "Bing et al."),
                          pride_prejudice %>% 
                            inner_join(get_sentiments("nrc") %>% 
                            filter(sentiment %in% c("positive", "negatice"))) %>% 
                            mutate(method = "NRC")) %>% 
  count(method, index = linenumber %/% 80, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

# We now have an estimate of the net sentiment (positive - negative) in each chunk of the novel text for each sentiment lexicon.
# Visualise 
bind_rows(afinn, bing_and_nrc) %>% 
  ggplot(mapping = aes(x = index, y = sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

# Diving deeper: Most Common Positive and Negative words
# We can analyse word counts that contribute to each sentiment. We can see how much each word contributed to each sentiment.
bing_word_counts <- tidy_books %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  ungroup()

# Visualise in ggplot2 
bing_word_counts %>% 
  group_by(sentiment) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(mapping = aes(x = word, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()


# Wordcloud
## Single wordcloud
tidy_books %>% 
  anti_join(stop_words) %>% 
  count(word) %>% 
  with(wordcloud(word, n, max.words = 100))

## Comparison cloud broken by sentiment
tidy_books %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  acast(word ~ sentiment, value.var = "n", fill = 0) %>% 
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)




