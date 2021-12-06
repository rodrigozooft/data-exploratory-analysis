votes_processed %>% 
  summarize(total = n(), percent_yes = mean(vote == 1))
