create_wordcloud <- function(data, comment, filter = NULL, ...){
  ## input:
  # data = a data frame containing each comment in a row
  # comment = variable containing the comments
  # ... = further arguments to be passed to wordcloud()
  library(tidytext)
  library(wordcloud)
  library(tidyverse)
  data(stop_words)
  wordfilter <- tibble(word = filter)
  
  data_unnest <- data %>% 
    unnest_tokens(input = {{comment}}, output = word) %>%
    mutate(word = str_extract(word, "[a-z']+")) %>%  # removes underscores, numbers etc
    filter(!is.na(word)) %>% 
    anti_join(stop_words)
  if(length(wordfilter) > 0){
    data_unnest <- data_unnest %>% 
      anti_join(wordfilter)
  }
  data_unnest <- data_unnest %>% 
    count(word, sort = TRUE)
  plot <- wordcloud(words = data_unnest$word, freq = data_unnest$n, ...)
  return(plot)
}