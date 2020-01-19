library(tidytext)
library(textdata)
library(tidyverse)
library(janeaustenr)
library(stringr)
library(wordcloud)
library(reshape2)
library(ggplot2)
library(gutenbergr)
library(cowplot)

# Tidying Jane Austen's works
original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

## Unnest tokens
tidy_books <- original_books %>% 
  unnest_tokens(word, text)

## Remove stop words
data(stop_words)

tidy_books <- tidy_books %>% 
  anti_join(stop_words)

## Examine sentiment changes across the narrative arcs of Pride and Prejudice
pride_prejudice <- tidy_books %>% 
  filter(book == "Pride & Prejudice")

pride_prejudice

# Monitor sentiment
afinn_pp <- pride_prejudice %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 80) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

 
ggplot(data = afinn_pp, mapping = aes(x = index, y = sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Line by Line \n Sentiment Scores")

afinn_pp_lagged_sent <- afinn_pp %>% 
  mutate(lagged_sentiment_1 = lag(sentiment, n = 1),
         lagged_diff = sentiment - lagged_sentiment_1,
         two_std_dev = 2 * sqrt(var(afinn_pp_lagged_sent$sentiment)),
         sentiment_change_flag = if_else(abs(lagged_diff) >= two_std_dev, 1, 0))

afinn_pp_cp <- afinn_pp_lagged_sent %>% 
  filter(sentiment_change_flag == 1)

pride_prejudice_windex <- pride_prejudice %>% 
  mutate(index = linenumber %/% 80)

# Isolate times on and before changepoint days

index_1 <- afinn_pp_cp$index
index_2 <- afinn_pp_cp$index - 1
filt_ind <- unique(c(index_1, index_2))

afinn_pp_filt <- afinn_pp_lagged_sent %>% 
  filter(index %in% filt_ind)

head(afinn_pp_filt)
head(pride_prejudice_windex)

pp_sent <- pride_prejudice_windex %>% 
  inner_join(select(afinn_pp_filt, index, sentiment), by = "index") %>% 
  arrange(index)

pp_sent_cat <- pp_sent %>% 
  mutate(sentiment_cat_flag = if_else(sentiment >= 0, 1, 0),
         word = str_extract(word, "[a-z']+")) %>% 
  arrange(index, sentiment)

# Count words
## Plot word frequencies - top 5
pp_sent_cat_wrds <- pp_sent_cat %>% 
  count(word, index, sentiment_cat_flag, sort = TRUE) %>% 
  mutate(word = reorder(word, n)) 

pp_sent_cat_wrds_subset <- pp_sent_cat_wrds %>% 
  arrange(index, word, sentiment_cat_flag, n) %>% 
  arrange(index, sentiment_cat_flag, desc(n)) %>% 
  group_by(index, sentiment_cat_flag) %>% 
  mutate(row_number = row_number()) %>% 
  filter(row_number <= 5)

pp_sent_cat_wrds_subset_cp1 <- pp_sent_cat_wrds_subset %>% 
  filter(index %in% c(7, 8))

ggplot(data = pp_sent_cat_wrds_subset_cp1, mapping = aes(x = word, y = n)) + 
  geom_col() + 
  xlab(NULL) + 
  coord_flip() +
  facet_grid(index ~ sentiment_cat_flag)
