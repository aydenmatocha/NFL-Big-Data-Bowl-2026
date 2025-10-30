library(tidyverse)

set_distance_values <- function(input_df) {
  input_df <- get_distance(input_df)
  input_df <- get_total_distance(input_df)
  return(input_df)
}

get_distance <- function(input_df) {
  # Get receiver x,y
  receiver <- input_df %>%
    filter(player_role == "Targeted Receiver") %>%
    select(x, y)
  
  # Assign defenders a variable "distance_from_rec" (the distance from the receiver)
  input_df <- input_df %>% 
    mutate(dist_from_rec = if_else(
      player_side == "Defense", sqrt((receiver$x - x)^2 + (receiver$y - y)^2), NA))
  
  return(input_df)
}

get_total_distance <- function(input_df) {
  total_distance <- input_df %>%
    filter(player_side == "Defense") %>%
    group_by(nfl_id) %>%
    summarize(total_distance = sum(dist_from_rec, na.rm = TRUE))
  
  lowest_id <- total_distance %>% slice_min(total_distance) %>% pull(nfl_id)
  
  input_df <- input_df %>%
    mutate(man_def = if_else(nfl_id == lowest_id, TRUE, FALSE))
  
  return(input_df)
}

week1 <- read.csv("data/input_2023_w01.csv") %>%
  filter(game_id == 2023090700, play_id == 194)

test1 <- get_distance(week1)

test2 <- get_total_distance(test1) %>%
  filter(nfl_id == 54653)
test2