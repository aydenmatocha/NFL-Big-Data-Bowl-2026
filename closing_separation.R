library(tidyverse)

set_closing_sep <- function(input_df) {
  closing_val <- calc_closing_sep(input_df)
  input_df <- input_df %>% mutate(closing_sep = closing_val)
  
  return(input_df)
}

calc_closing_sep <- function(input_df) {
  init_sep <- get_init_sep(input_df)
  final_sep <- get_final_sep(input_df)
  
  return(init_sep - final_sep)
}

get_init_sep <- function(input_df) {
  throw_frame <- input_df %>% pull(throw_frame) %>% unique()
  
  init_sep <- input_df %>%
    filter(frame_id == throw_frame, man_def == TRUE) %>%
    pull(dist_from_rec)
  
  return(init_sep)
}

get_final_sep <- function(input_df) {
  final_sep <- input_df %>%
    filter(frame_id == max(frame_id), man_def == TRUE) %>%
    pull(dist_from_rec)
  
  return(final_sep)
}

