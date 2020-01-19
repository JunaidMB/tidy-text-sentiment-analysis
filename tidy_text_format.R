library(tidyverse)
library(tidytext)
library(janeaustenr)
library(stringr)
library(ggplot2)
library(gutenbergr)

# Tidy Text Formatting

text <- c("Because I could not stop for Death -",
          "He kindly stopped for me - ",
          "The Carriage held but just Ourselves",
          "and Immortality")

# Turn character vector into a dataframe
text_df <- tibble(line = 1:4, text = text)
text_df

# Break the text into individual tokens 
text_df_unnested <- text_df %>% 
  unnest_tokens(word, text)

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

tidy_books %>% 
  count(word, sort = TRUE)

## Plot word frequencies
tidy_books %>% 
  count(word, sort = TRUE) %>% 
  filter( n > 600) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) + 
  geom_col() + 
  xlab(NULL) + 
  coord_flip()

# Word Frequencies across different texts
## Download HG Wells' works using Project Gutenberg ID numbers
hgwells <- gutenberg_download(c(35, 36, 5230, 159))

tidy_hgwells <- hgwells %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

## Most common words in the novels of H.G. Wells
tidy_hgwells %>% 
  count(word, sort = TRUE)

## Download Bronte sisters' works
bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))

tidy_bronte <- bronte %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

## What are the most common words in these novels of the Brontë sisters?
tidy_bronte %>% 
  count(word, sort = TRUE)

## Compute frequency for each word for the works of Jane Austen, Bronte sisters and H.G. Wells
frequency <- bind_rows(mutate(tidy_bronte, author = "Bronte Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"),
                       mutate(tidy_books, author = "Jane Austen")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>% 
  count(author, word) %>% 
  group_by(author) %>% 
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(author, proportion) %>% 
  gather(author, proportion, "Bronte Sisters": "H.G. Wells")

## See how correlated the word frequencies are
cor.test(data = frequency[frequency$author == "Bronte Sisters",],
         ~ proportion + `Jane Austen`)

cor.test(data = frequency[frequency$author == "H.G. Wells",],
         ~ proportion + `Jane Austen`)


