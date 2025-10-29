library(tidyverse)

getDistance <- function(input_df) {
  # Get receiver x,y
  receiver <- input_df %>%
    filter(player_role == "Targeted Receiver") %>%
    select(x, y)
  
  print(receiver)
  
  input_df <- input_df %>% 
    mutate(dist_from_rec = sqrt((receiver$x - x)^2 + (receiver$y - y)^2))
  
  return(input_df)
}

week1 <- read.csv("data/input_2023_w01.csv") %>%
  filter(game_id == 2023090700, play_id == 194)
getDistance(week1)