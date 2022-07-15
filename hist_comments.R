hist_comments <- function(data, time, bins, ...){
  ## input:
  # data = a data frame containing each comment in a row
  # time = the time variable
  # bins = in how many bins should the observations be aggregated?
  # ... = further arguments to be passed to geom_histogram()
  library(tidyverse)
  plot <- data %>% 
    ggplot() +
    geom_histogram(aes(x = {{time}}), bins = bins, ...)
  return(plot)
}
