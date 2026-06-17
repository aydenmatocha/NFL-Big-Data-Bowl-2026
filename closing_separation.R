library(tidyverse)

set_closing_sep <- function(input_df) {
  closing_val <- calc_closing_sep(input_df)
  if (length(closing_val) == 0) return(NULL)
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
  final_frame <- max(input_df$frame_id)
  #print(paste("final_frame:", final_frame))
  #print(paste("man_def at final_frame:", sum(input_df$frame_id == final_frame & input_df$man_def == TRUE)))

  final_sep <- input_df %>%
    filter(frame_id == max(frame_id), man_def == TRUE) %>%
    pull(dist_from_rec)
  
  return(final_sep)
}

