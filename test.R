library(tidyverse)
library(ggplot2)
library(gganimate)
library(sportyR)

# Get CSV File
week1 <- read.csv("data/input_2023_w01.csv")

str(week1)

play <- week1 %>%
  filter(game_id == 2023090700, play_id == 101) %>%
  mutate(color = case_when(
      player_position == "WR" ~ "blue",
      player_position == "TE" ~ "purple",
      player_position == "QB" ~ "black",
      player_position == "CB" ~ "red",
      player_position == "MLB" ~ "white",
      player_position == "SS" ~ "yellow",
      player_position == "FS" ~ "grey"
    )
  )

nfl_field <- geom_football("nfl", x_trans = 60, y_trans = 26.6667)

play_anim <- nfl_field +
  geom_point(data = play, aes(x, y), color = play$color) +
  transition_time(frame_id)

animate(play_anim, fps = 10, nframes = max(play$frame_id, na.rm = TRUE))