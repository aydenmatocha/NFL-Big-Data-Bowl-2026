library(tidyverse)

# Combine input and output files for a week
combine_week <- function(week) {
  zero <- if (nchar(as.character(week)) == 1) "0" else ""
  
  input_file  <- paste0("data/input_2023_w",  zero, week, ".csv")
  output_file <- paste0("data/output_2023_w", zero, week, ".csv")
  
  input_df <- read.csv(input_file) #%>% filter(player_to_predict == TRUE) # Only keep players in output
  output_df <- read.csv(output_file)
  
  # Get the throw frame (last frame from input) per play
  throw_frames <- input_df %>%
    group_by(game_id, play_id) %>%
    summarize(throw_frame = max(frame_id), .groups = "drop")
  
  # Pull player-level metadata from input to join onto output
  player_meta <- input_df %>%
    distinct(nfl_id, player_name, player_height, player_weight,
             player_birth_date, player_position)
  
  # Pull play-level metadata from input to join onto output
  play_meta <- input_df %>%
    distinct(game_id, play_id, play_direction, absolute_yardline_number,
             num_frames_output, ball_land_x, ball_land_y)
  
  # Pull play-varying player attributes (side/role) to join onto output
  player_play_meta <- input_df %>%
    distinct(game_id, play_id, nfl_id, player_side, player_role)
  
  # Play-level supplementary data (join on game_id + play_id)
  supp_meta <- read.csv("data/supplementary_data.csv") %>%
    select(
      game_id, play_id,
      route_of_targeted_receiver,
      pass_location_type,
      team_coverage_man_zone,
      team_coverage_type,
      pass_length,
      pass_result
    ) %>%
    distinct(game_id, play_id, .keep_all = TRUE)
  
  # Offset output frame_ids so they continue from where input left off
  output_enriched <- output_df %>%
    left_join(throw_frames, by = c("game_id", "play_id")) %>%
    mutate(frame_id = frame_id + throw_frame) %>%
    left_join(player_meta, by = "nfl_id") %>%
    left_join(play_meta,   by = c("game_id", "play_id")) %>%
    left_join(player_play_meta, by = c("game_id", "play_id", "nfl_id")) %>%
    mutate(phase = "post_throw")
  
  # Tag input frames too, and add throw_frame column to both
  input_tagged <- input_df %>%
    left_join(throw_frames, by = c("game_id", "play_id")) %>%
    mutate(phase = "pre_throw")
  
  # Bind — output_enriched now has all the same columns as input_tagged
  combined <- bind_rows(input_tagged, output_enriched) %>%
    arrange(game_id, play_id, nfl_id, frame_id) %>%
    left_join(supp_meta,   by = c("game_id", "play_id"))
  
  return(combined)
}

set_week_values <- function(week) {
  week_df <- combine_week(week)
  
  # Get distance and closing separation values
  week_df <- week_df %>%
    group_by(game_id, play_id) %>%
    group_modify(~ {
      result <- set_distance_values(.x)
      if (is.null(result)) return(tibble())
      result
    }) %>%
    filter(nrow(.) > 0) %>%
    ungroup()
  
  week_df <- week_df %>%
    group_by(game_id, play_id) %>%
    group_modify(~ {
      result <- set_closing_sep(.x)
      if (is.null(result)) return(tibble())
      result
    }) %>%
    ungroup()
  
  return(week_df)
}
