library(tidyverse)
library(ggplot2)
library(gganimate)
library(sportyR)
library(nflreadr)

teams_colors_logos <- nflreadr::load_teams()
head(teams_colors_logos)

# Get CSV Files
week1 <- read.csv("data/input_2023_w01.csv")
supplementary <- read.csv("data/supplementary_data.csv")

merged <- week1 %>%
  left_join(
    supplementary %>%
      select(game_id, play_id, possession_team, defensive_team, home_team_abbr, visitor_team_abbr),
    by = c("game_id", "play_id")
  ) %>%
  mutate(
    team_abbr = case_when(
      player_side == "Offense" ~ possession_team,
      player_side == "Defense" ~ defensive_team,
      TRUE ~ NA_character_
    )
  ) %>%
  left_join(teams_colors_logos %>% select(team_abbr, team_color, team_color2),
            by = "team_abbr") %>%
  mutate(color = team_color)


play <- merged %>%
  filter(game_id == 2023090700, play_id == 194) %>%
  mutate(
    home_or_away = case_when(
      team_abbr == home_team_abbr ~ "home",
      team_abbr == visitor_team_abbr ~ "away",
      TRUE ~ NA_character_
    )
  )
  

nfl_field <- geom_football("nfl", x_trans = 60, y_trans = 26.6667)

play_anim <- nfl_field +
  geom_point(data = play, aes(x, y, color = color), size = 5) +
  transition_time(frame_id)

animate(play_anim, fps = 10, nframes = max(play$frame_id, na.rm = TRUE))