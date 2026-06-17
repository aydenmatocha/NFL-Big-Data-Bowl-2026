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
    select(frame_id, rec_x = x, rec_y = y) %>%
    distinct(frame_id, .keep_all = TRUE)
  
  # Assign defenders a variable "distance_from_rec" (the distance from the receiver)
  input_df <- input_df %>%
    left_join(receiver, by = "frame_id") %>%
    mutate(
      dist_from_rec = if_else(player_side == "Defense",
        sqrt((rec_x - x)^2 + (rec_y - y)^2), NA_real_)
    ) %>%
    select(-rec_x, -rec_y)
  
  return(input_df)
}

get_total_distance <- function(input_df) {
  total_distance <- input_df %>%
    filter(player_side == "Defense") %>%
    group_by(nfl_id) %>%
    summarize(total_distance = sum(dist_from_rec, na.rm = TRUE))
  
  if (nrow(total_distance) == 0) {
    return(NULL)
  }
  
  lowest_id <- total_distance %>% slice_min(total_distance) %>% pull(nfl_id)
  
  input_df <- input_df %>%
    mutate(man_def = if_else(nfl_id == lowest_id, TRUE, FALSE))
  
  return(input_df)
}

