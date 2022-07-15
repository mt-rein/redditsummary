hist_sentiment <- function(data, comment, id, time, filter = NULL){
  # data = a data frame containing each comment in a row
  # comment = variable containing the comments
  # time = the time variable
  # filter 
  
  library(tidyverse)
  library(tidytext)

  wordfilter <- tibble(word = filter)
  data_unnest <- data %>% 
    unnest_tokens(word, {{comment}}) %>% 
    mutate(word = str_extract(word, "[a-z']+")) %>% 
    filter(!is.na(word))
  if(length(wordfilter) > 0){
    data_unnest <- data_unnest %>% 
      anti_join(wordfilter)
  }
    
  sentiment <- data_unnest %>%
    inner_join(get_sentiments("afinn")) %>%
    group_by({{id}}) %>%
    summarise(sentiment = mean(value, na.rm = TRUE))
  
  data <- data %>% 
    inner_join((sentiment))
  
  plot <- data %>% 
    ggplot(aes(x = {{time}}, y = sentiment)) +
    geom_point() +
    geom_smooth(se = F)
  return(plot)
}
