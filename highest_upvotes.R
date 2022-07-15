highest_upvotes <- function(data, score, comment, vars, n = 5){
  ## input:
  # data = a data frame containing each comment in a row
  # score = variable containing the scores
  # comment = variable containing the comments
  # vars = additional comment-level variables to be shown in output (e.g., author, ...)
  # number of comments to be shown
  library(kableExtra)
  library(tidyverse)
  output <- data %>% 
    arrange(desc({{score}})) %>% 
    head(n = n) %>% 
    select(score, comment, {{vars}}) %>% 
    kable(caption = "Most upvoted comments") %>% 
    kable_styling()
  return(output)
}