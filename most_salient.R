most_salient <- function(data, comment, id, vars, n = 5){
  ## input:
  # data = a data frame containing each comment in a row
  # comment = variable containing the comments
  # id = variable indicating the comment IDs
  # vars = additional comment-level variables to be shown in output (e.g., author, ...)
  # n = number of comments to be shown
  library(kableExtra)
  library(tidyverse)
  library(lexRankr)
  output <- data %>% 
    mutate(id = {{id}}) %>% 
    bind_lexrank(text = comment, doc_id = id, level = "sentences") %>% 
    arrange(desc(lexrank)) %>%
    head(n = n) %>%
    select({{comment}}, lexrank, {{vars}}) %>%
    kable(caption = "Most salient comments") %>%
    kable_styling()
  return(output)
}
