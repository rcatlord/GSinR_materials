## Web scraping and tidy text

library(tidyverse) ; library(rvest) ; library(tidytext) ; library(wordcloud2)

#### Download and parse the html web page
web_page <- read_html("https://www.theguardian.com/books/2008/sep/20/fiction")

#### Find the html nodes that match the CSS selector
web_nodes <- html_nodes(web_page, ".js-article__body p")

#### Extract the text from the html nodes
web_text <- html_text(web_nodes)

#### Or using %>%
web_text <- read_html("https://www.theguardian.com/books/2008/sep/20/fiction") %>% 
  html_nodes(".js-article__body p") %>% 
  html_text()

#### Convert to a data frame
text_df <- data_frame(text = web_text)

#### Trim the text
text_df <- text_df %>% 
  slice(1:(grep("this is water.\"", text)))

#### Remove any blank lines
text_df <- text_df %>% 
  filter(nzchar(text))

#### Add paragraph numbers
text_df <- text_df %>% 
  mutate(paragraph = row_number())

#### Or using %>%
text_df <- web_text %>% 
  data_frame(text = .) %>% 
  slice(1:(grep("this is water.\"", text))) %>% 
  mutate(paragraph = row_number())

#### Unnest values in text column into a word column
words <- text_df %>% 
  unnest_tokens(word, text)

#### Remove 'stop words'
words <- words %>% 
  anti_join(stop_words, by = "word")

#### Most common words
words %>% 
  count(word, sort = TRUE)

#### Bigrams
bigram <- text_df %>%
  unnest_tokens(word, text, token = "ngrams", n = 2) %>% # tokenise into word pairs
  separate(word, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word, # remove 'stop words'
         !word2 %in% stop_words$word) %>%
  unite(word, word1, word2, sep = " ")

#### Frequency of bigrams
bigram %>%
  count(word, sort=TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "grey", alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = NULL, y = "Number of mentions",
       title = "2-word combinations in Foster Wallace's commencement speech") +
  theme_minimal()

#### Frequency of positive words
words %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  filter(sentiment == "positive") %>%
  count(word) %>%
  wordcloud2(size = 0.7, fontFamily = "RobotoCondensed-Regular", 
             color = rep(c('orange', 'skyblue'), length.out=nrow(.)))

####n Frequency of negative words
words %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  filter(sentiment == "negative") %>% 
  count(word) %>% 
  wordcloud2(size = 0.7, fontFamily = "RobotoCondensed-Regular", 
             color = rep(c('black', 'grey'), length.out=nrow(.)))

#### Distribution of positive and negative words
words %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup() %>% 
  filter(n > 1) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  labs(x = NULL, y = NULL, fill = "Sentiment") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position="bottom")
