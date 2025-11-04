library(tidyverse)

calc_closing_sep <- function(input_df, throw_frame) {
  init_sep <- input_df %>%
    filter(frame_id == throw_frame, man_def == TRUE) %>%
    pull(dist_from_rec)
  
  print(init_sep)
  
  final_sep <- input_df %>%
    filter(frame_id == max(frame_id), man_def == TRUE) %>%
    pull(dist_from_rec)
  
  print(final_sep)
  
  return(init_sep - final_sep)
}

#week1 <- read.csv("data/input_2023_w01.csv") %>%
#filter(game_id == 2023090700, play_id == 194)

#test1 <- set_distance_values(week1)

#test2 <- calc_closing_sep(test1, 1)
#test2