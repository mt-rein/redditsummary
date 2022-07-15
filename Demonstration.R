library(tidyverse)
library(lubridate)
library(RedditExtractoR) # used to extract the comments. It can only extract 500 comments, but this is enough for the demonstration purposes

set.seed(1234)
#### load comments, draw one thread randomly (here with seed), clean up dataframe ####
example <- get_thread_content("https://www.reddit.com/r/news/comments/vjpfbh/supreme_court_overturns_roe_v_wade_states_can_ban/")[[2]] %>% # this function gives a list, we select the 2nd element (the comment dataframe)
  select(comment, comment_id, score, author, timestamp) %>% # only select relevant variables
  filter(comment != "[deleted]") %>% # remove deleted comments
  mutate(timestamp = as_datetime(timestamp)) # mutate timestamp into a readable variable

#### histogram of comment frequency across time ####
source("functions/hist_comments.R")

hist_comments(data = example, time = timestamp, bins = 30)


#### histogram of sentiment scores across time ####
source("functions/hist_sentiment.R")

hist_sentiment(data = example, comment = comment, id = comment_id, time = timestamp, filter = NULL)


#### create a word cloud ####
source("functions/create_wordcloud.R")

create_wordcloud(data = example, comment = comment, filter = NULL, max.words = 100, random.order = FALSE)


#### highest upvoted comments ####
source("functions/highest_upvotes.R")

highest_upvotes(data = example, score = score, comment = comment, vars = c("timestamp", "comment_id"))


#### most salient comments (lexrank) ####
source("functions/most_salient.R")

most_salient(data = example, comment = comment, id = comment_id, vars = timestamp)


#### most positive/negative comments (sentiment scores) ####
source("functions/highest_sentiment.R")

highest_sentiment(data = example, comment = comment, id = comment_id, filter = NULL, sorting = "negative")
