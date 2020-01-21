devtools::install_github("bradleyboehmke/harrypotter")
library(tidyverse)
library(stringr)
library(tidytext)
library(harrypotter)
library(tm)

# Basic Sentiment analysis
# Convert all novels into a tibble which has each word by chapter by book
titles <- c("Philosopher's Stone",
            "Chamber of Secrets",
            "Prisoner of Azkaban",
            "Goblet of Fire",
            "Order of the Pheonix",
            "Half-Blood Prince",
            "Deathly Hallows")

books <- list(philosophers_stone, chamber_of_secrets, prisoner_of_azkaban,
              goblet_of_fire, order_of_the_phoenix, half_blood_prince,
              deathly_hallows)

series <- tibble()

for (i in seq_along(titles)) {
  clean <- tibble(chapter = seq_along(books[[i]]),
                  text = books[[i]]) %>% 
    unnest_tokens(word, text) %>% 
    mutate(book = titles[i]) %>% 
    select(book, everything())
  
  series <- rbind(series, clean)
}

# set factor to keep books in order of publication
series$book <- factor(series$book, levels = rev(titles))

# Remove stop words
series_go <- series %>% 
  anti_join(stop_words)

# Examine sentiment changes across the chapter 1
philston <- filter(series, book == "Philosopher's Stone")

# Add an index
philston <- philston %>% 
  group_by(chapter) %>% 
  mutate(index = row_number())

# Monitor sentiment using afinn sentiment lexicon
philston_afinn <- philston %>% 
  inner_join(get_sentiments("afinn")) %>% # Will remove some words
  group_by(chapter) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN") %>% 
  arrange(chapter)

# Plot sentiment by chapter
ggplot(data = philston_afinn, mapping = aes(x = chapter, y = sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Sentiment Scores by Chapter")

# Compare how sentiment varies by chapter - flag sentiment change if overall sentiment changes from positive to negative
philston_afinn_lagged <- philston_afinn %>% 
  mutate(lagged_sentiment_1 = lag(sentiment, n = 1),
         lagged_diff = sentiment - lagged_sentiment_1,
         sentiment_change_flag = if_else((sentiment < 0 & lagged_sentiment_1 > 0) | (sentiment > 0 & lagged_sentiment_1 < 0), 1, 0))


## Comparison cloud broken by sentiment
philson_sent_labeled <- philston %>% 
  inner_join(get_sentiments("afinn")) %>% 
  mutate(sentiment = if_else(value >= 0, "positive", "negative")) 

## Make a list of per chapter wordcloud
cpt_wc <- list()

for (i in unique(philson_sent_labeled$chapter)) {
  
  filtered_tbl <- filter(philson_sent_labeled, chapter == i)
  
  filtered_tbl_wrd_count <- filtered_tbl %>% 
    count(word, sentiment, sort = TRUE) %>% 
    acast(word ~ sentiment, value.var = "n", fill = 0)
  
  cpt_wc[[i]] <- filtered_tbl_wrd_count
  names(cpt_wc)[i] <- str_c("chapter ", i)
  
  
}

# Plot comparison cloud
comparison.cloud(cpt_wc[['chapter 5']], colors = c("red", "green"),
                 max.words = 50)

# Filter list for sentiment changing chapters
filter(philston_afinn_lagged, sentiment_change_flag == 1)$chapter
cpt_wc[str_c("chapter ", filter(philston_afinn_lagged, sentiment_change_flag == 1)$chapter)]

cpt_wc[['chapter 5']]
