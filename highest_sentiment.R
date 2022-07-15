highest_sentiment <- function(data, comment, id, vars, n = 5, sorting = "positive", filter = NULL){
  # data = a data frame containing each comment in a row
  # comment = variable containing the comments
  # id = variable indicating the comment IDs
  # vars = additional comment-level variables to be shown in output (e.g., author, ...)
  # n = number of comments to be shown
  # sorting = should the most positive (= "positive") or most negative (= "negative") comments be shown? Default = "positive"
  # filter = a word filter to remove words that should not be considered during sentiment analysis
  
  library(tidyverse)
  library(tidytext)
  library(kableExtra)

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
  
  if(sorting != "negative" & sorting != "positive"){
    stop("Please select a sorting method ('negative' or 'positive')")
  }
  if(sorting == "negative"){
    output <- data %>% 
      inner_join((sentiment)) %>% 
      arrange(sentiment) %>% 
      head(n = n) %>%
      select({{id}}, {{comment}}, sentiment, {{vars}}) %>%
      kable(caption = "Most salient comments") %>%
      kable_styling()
  }
  if(sorting == "positive"){
    output <- data %>% 
      inner_join((sentiment)) %>% 
      arrange(desc(sentiment)) %>% 
      head(n = n) %>%
      select({{id}}, {{comment}}, sentiment, {{vars}}) %>%
      kable(caption = "Most salient comments") %>%
      kable_styling()
  }
  return(output)
}

