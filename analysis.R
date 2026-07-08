library(DT)
library(gt)
library(gtExtras)
library(ggthemes)
library(nflfastR)
library(tidyverse)
library(nflplotR)
library(nflreadr)

player_ids <- nflreadr::load_players() %>%
  semi_join(nflreadr::load_rosters(seasons = 2023), by = "gsis_id") %>%
  select(gsis_id, display_name, position, status) # Maybe position?

rosters_2023 <- nflreadr::load_rosters(seasons = 2023) %>%
  select(gsis_id, team, position) %>%
  distinct(gsis_id, .keep_all = TRUE)

leaderboard <- oof_predictions %>%
  mutate(cse_residual = actual_closing_sep - .pred) %>%
  group_by(def_nfl_id) %>%
  summarize(
    n_plays = n(),
    avg_cse = mean(cse_residual),
    .groups = "drop"
  ) %>%
  left_join(player_names, by = c("def_nfl_id" = "nfl_id")) %>%
  mutate(player_name = if_else(player_name == "Asante Samuel", "Asante Samuel Jr.", player_name)) %>% # Fix for Asante Samuel Jr
  left_join(player_ids, by = c("player_name" = "display_name")) %>%
  left_join(rosters_2023, by = "gsis_id") %>%
  #mutate(avg_cse = round(avg_cse, 2)) %>%
  arrange(desc(avg_cse))

leaderboard_viz <- leaderboard %>% 
  filter(n_plays >= 10) %>%
  select(player_name, gsis_id, team, position.x, n_plays, avg_cse)

cse_range <- range(leaderboard_viz$avg_cse, na.rm = TRUE)
  
# Top 10
top10 <- leaderboard_viz %>%
  slice_max(avg_cse, n = 10) %>%
  gt() %>%
  gt_theme_538() %>%
  tab_header(
    title = "Top 10 Players in Average CSOE",
    subtitle = "2023 Season, Minimum 10 Plays"
  ) %>%
  fmt_number(
    columns = avg_cse,
    decimals = 2
  ) %>%
  gt_nfl_headshots("gsis_id", height = 35) %>%
  gt_nfl_logos("team", height = 35) %>%
  cols_label(player_name = "Player", gsis_id = "", position.x = "pos") %>%
  data_color(
    columns = avg_cse,
    colors = scales::col_numeric(
      palette = c("red", "white", "green"),
      domain = cse_range
    )
  ) 

top10

# Bottom 10
bottom10 <- leaderboard_viz %>%
  slice_min(avg_cse, n = 10) %>%
  gt() %>%
  gt_theme_538() %>%
  tab_header(
    title = "Bottom 10 Players in Average CSOE",
    subtitle = "2023 Season, Minimum 10 Plays"
  ) %>%
  fmt_number(
    columns = avg_cse,
    decimals = 2
  ) %>%
  gt_nfl_headshots("gsis_id", height = 35) %>%
  gt_nfl_logos("team", height = 35) %>%
  cols_label(player_name = "Player", gsis_id = "", position.x = "pos") %>%
  data_color(
    columns = avg_cse,
    colors = scales::col_numeric(
      palette = c("red", "white", "green"),
      domain = cse_range
    )
  ) 

bottom10

