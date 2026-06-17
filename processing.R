library(tidyverse)

# Combine input and output files for a week
combine_week <- function(week) {
  zero <- if (nchar(as.character(week)) == 1) "0" else ""
  
  input_file  <- paste0("data/input_2023_w",  zero, week, ".csv")
  output_file <- paste0("data/output_2023_w", zero, week, ".csv")
  
  input_df <- read.csv(input_file)
  output_df <- read.csv(output_file)
  
  # Get the throw frame (last frame from input) per play
  throw_frames <- input_df %>%
    group_by(game_id, play_id) %>%
    summarize(throw_frame = max(frame_id), .groups = "drop")
  
  # Pull player-level metadata from input to join onto output
  player_meta <- input_df %>%
    distinct(nfl_id, player_name, player_height, player_weight,
             player_birth_date, player_position, player_side, player_role,
             player_to_predict)
  
  # Pull play-level metadata from input to join onto output
  play_meta <- input_df %>%
    distinct(game_id, play_id, play_direction, absolute_yardline_number,
             num_frames_output, ball_land_x, ball_land_y)
  
  # Offset output frame_ids so they continue from where input left off
  output_enriched <- output_df %>%
    left_join(throw_frames, by = c("game_id", "play_id")) %>%
    mutate(frame_id = frame_id + throw_frame) %>%
    left_join(player_meta, by = "nfl_id") %>%
    left_join(play_meta,   by = c("game_id", "play_id")) %>%
    mutate(phase = "post_throw")
  
  # Tag input frames too, and add throw_frame column to both
  input_tagged <- input_df %>%
    left_join(throw_frames, by = c("game_id", "play_id")) %>%
    mutate(phase = "pre_throw")
  
  # Bind — output_enriched now has all the same columns as input_tagged
  combined <- bind_rows(input_tagged, output_enriched) %>%
    arrange(game_id, play_id, nfl_id, frame_id)
  
  return(combined)
}

set_week_values <- function(week) {
  week_df <- combine_week(week)
  
  # Get distance and closing separation values
  week_df <- week_df %>%
    group_by(game_id, play_id) %>%
    group_modify(~ set_distance_values(.x)) %>%
    ungroup()
  
  # Get closing separation values
  
  return(week_df)
}

test1 <- combine_week(1)
test1 <- test1 %>% filter(game_id == 2023090700, play_id == 194)
test1 <- set_distance_values(test1)

test2 <- set_week_values(1)
test2 <- test2 %>% filter(game_id == 2023090700, play_id == 194)