library(tidyverse)
library(ggplot2)
library(gganimate)
library(sportyR)
library(nflreadr)
teams_colors_logos <- nflreadr::load_teams()

animate_bef <- function(week, game, play) {
  zero <- ""
  if (nchar(as.character(week)) == 1) {
    zero <- "0"
  }
  file <- paste("data/input_2023_w", zero, week, ".csv", sep = "")
  week_data <- read.csv(file) %>%
    filter(game_id == game, play_id == play)
  supplementary <- read.csv("data/supplementary_data.csv") %>%
    filter(game_id == game, play_id == play) %>%
    select(game_id, play_id, possession_team, defensive_team, home_team_abbr, visitor_team_abbr)
  
  merged <- week_data %>%
    left_join(supplementary, by = c("game_id", "play_id")) %>%
    filter(player_side == "Defense" | player_role == "Targeted Receiver" | player_role == "Passer") %>%
    mutate(
      team_abbr = case_when(
        player_side == "Offense" ~ possession_team,
        player_side == "Defense" ~ defensive_team,
      )
    ) %>%
    left_join(teams_colors_logos %>% select(team_abbr, team_color, team_color2), by = "team_abbr") %>%
    mutate(color = team_color)
  
  play <- set_distance_values(merged)
  
  man <- play %>% filter(man_def == TRUE)
  rec <- play %>% filter(player_role == "Targeted Receiver")
  
  nfl_field <- geom_football("nfl", x_trans = 60, y_trans = 26.6667)
  print(play %>% filter(player_role == "Targeted Receiver") %>% arrange(-frame_id))
  
  play_anim <- nfl_field +
    geom_point(data = play, aes(x, y), color = play$color, size = 5) +
    geom_segment(data = man, aes(x, y, xend = rec$x, yend = rec$y),color = "black", linewidth = 1.2) +
    geom_label(data = man, aes(x, y + 3, label = round(dist_from_rec, 2)), color = "black", size = 3) +
    transition_time(frame_id)
  
  return(animate(play_anim, fps = 10, nframes = max(play$frame_id, na.rm = TRUE)))
}



animate_aft <- function(week, game, play) {
  zero <- ""
  if (nchar(as.character(week)) == 1) {
    zero <- "0"
  }
  file <- paste("data/input_2023_w", zero, week, ".csv", sep = "")
  week_data_input <- read.csv(file) %>%
    filter(game_id == game, play_id == play) %>%
    select(game_id, play_id, nfl_id, player_side, player_role, ball_land_x, ball_land_y)
  file <- paste("data/output_2023_w", zero, week, ".csv", sep = "")
  week_data_output <- read.csv(file) %>%
    filter(game_id == game, play_id == play)
  supplementary <- read.csv("data/supplementary_data.csv") %>%
    filter(game_id == game, play_id == play) %>%
    select(game_id, play_id, possession_team, defensive_team, home_team_abbr, visitor_team_abbr, pass_result)
  
  #targeted_id <- week_data_input %>%
    #filter(player_role == "Targeted Receiver") %>%
    #pull(nfl_id) %>%
    #unique()
  
  #week_data_output <- week_data_output %>%
    #mutate(player_role = ifelse(nfl_id == targeted_id, "Targeted Receiver", player_role))
  
  merged <- week_data_output %>%
    left_join(week_data_input,
              by = c("game_id", "play_id", "nfl_id")) %>%
    left_join(supplementary, by = c("game_id", "play_id")) %>%
    mutate(
      team_abbr = case_when(
        player_side == "Offense" ~ possession_team,
        player_side == "Defense" ~ defensive_team,
      )
    ) %>%
    left_join(teams_colors_logos %>% select(team_abbr, team_color, team_color2), by = "team_abbr") %>%
    mutate(color = team_color)
  
  play <- set_distance_values(merged)
  
  final_frame <- max(play$frame_id)
  ball_landing <- tibble(
    frame_id = 1:final_frame,
    x = unique(play$ball_land_x),
    y = unique(play$ball_land_y),
  )
  
  man <- play %>% filter(man_def == TRUE)
  rec <- play %>% filter(player_role == "Targeted Receiver")
  
  nfl_field <- geom_football("nfl", x_trans = 60, y_trans = 26.6667)
  
  play_anim <- nfl_field +
    geom_point(data = play, aes(x, y), color = play$color, size = 5) +
    geom_point(data = ball_landing, aes(x, y), shape = 4, color = ball_landing$result_color) +
    geom_segment(data = man, aes(x, y, xend = rec$x, yend = rec$y),color = "black", linewidth = 1.2) +
    geom_label(data = man, aes(x, y + 3, label = round(dist_from_rec, 2)), color = "black", size = 3) +
    transition_time(frame_id)
  
  return(animate(play_anim, fps = 10, nframes = max(play$frame_id, na.rm = TRUE)))
}



animate_full <- function(week, game, play) {
  zero <- ""
  if (nchar(as.character(week)) == 1) {
    zero <- "0"
  }
  file <- paste("data/input_2023_w", zero, week, ".csv", sep = "")
  week_data_input <- read.csv(file) %>%
    filter(game_id == game, play_id == play) %>%
    select(game_id, play_id, nfl_id, frame_id, player_side, player_role, x, y, ball_land_x, ball_land_y)
  file <- paste("data/output_2023_w", zero, week, ".csv", sep = "")
  week_data_output <- read.csv(file) %>%
    filter(game_id == game, play_id == play) %>%
    mutate(frame_id = frame_id + max(week_data_input$frame_id))
  
  output_ids <- unique(week_data_output$nfl_id)
  week_data_input <- week_data_input %>%
    filter(player_role == "Passer" | nfl_id %in% output_ids)
  
  supplementary <- read.csv("data/supplementary_data.csv") %>%
    filter(game_id == game, play_id == play) %>%
    select(game_id, play_id, possession_team, defensive_team, home_team_abbr, visitor_team_abbr, pass_result)
  
  player_roles <- week_data_input %>%
    distinct(nfl_id, player_side, player_role)
  
  week_data_output <- week_data_output %>%
    left_join(player_roles, by = "nfl_id")

  week_data_combined <- bind_rows(week_data_input, week_data_output)
  
  pass_result_val <- supplementary %>%
    pull(pass_result) %>%
    unique()
  
  merged <- week_data_combined %>%
    left_join(supplementary, by = c("game_id", "play_id")) %>%
    mutate(
      team_abbr = case_when(
        player_side == "Offense" ~ possession_team,
        player_side == "Defense" ~ defensive_team,
      )
    ) %>%
    left_join(teams_colors_logos %>% select(team_abbr, team_color, team_color2), by = "team_abbr") %>%
    mutate(color = team_color)

  play <- set_distance_values(merged)
  
  throw_frame <- max(week_data_input$frame_id) + 1
  final_frame <- max(play$frame_id)
  ball_landing <- tibble(
    frame_id = throw_frame:final_frame,
    x = unique(week_data_input$ball_land_x),
    y = unique(week_data_input$ball_land_y),
    pass_result = pass_result_val
  ) %>%
    mutate(result_color = case_when(
      pass_result == "C" ~ "green",
      pass_result == "I" ~ "black",
      pass_result == "IN" ~ "red",
      TRUE ~ "gray"
    ))
  
  man <- play %>% filter(man_def == TRUE)
  rec <- play %>% filter(player_role == "Targeted Receiver")
  
  nfl_field <- geom_football("nfl", x_trans = 60, y_trans = 26.6667)
  
  print(play %>% filter(man_def == TRUE) %>% arrange(frame_id))
  
  play_anim <- nfl_field +
    geom_point(data = play, aes(x, y), color = play$color, size = 5) +
    geom_point(data = ball_landing, aes(x, y), shape = 4, color = ball_landing$result_color) +
    geom_segment(data = man, aes(x, y, xend = rec$x, yend = rec$y),color = "black", linewidth = 1.2) +
    geom_label(data = man, aes(x, y + 3, label = round(dist_from_rec, 2)), color = "black", size = 3) +
    transition_time(frame_id)
  
  return(animate(play_anim, fps = 10, nframes = max(play$frame_id, na.rm = TRUE)))
}

animate_full(1, 2023090700, 194)
#animate_bef(1, 2023090700, 194)
#animate_aft(1, 2023090700, 194)
