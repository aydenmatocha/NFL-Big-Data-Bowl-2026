library(DT)
library(gt)
library(gtExtras)
library(ggthemes)
library(nflfastR)
library(tidyverse)

leaderboard <- oof_predictions %>%
  mutate(cse_residual = actual_closing_sep - .pred) %>%
  group_by(def_nfl_id) %>%
  summarize(
    n_plays = n(),
    avg_cse = mean(cse_residual),
    .groups = "drop"
  ) %>%
  left_join(player_names, by = c("def_nfl_id" = "nfl_id")) %>%
  mutate(avg_cse = round(avg_cse, 2)) %>%
  arrange(desc(avg_cse))

leaderboard2 <- leaderboard %>% 
  filter(n_plays >= 10) %>%
  select(player_name, n_plays, avg_cse)
  

leaderboard3 <- leaderboard2 %>%
  gt() %>%
  gt_theme_538() %>%
  tab_header(
    title = "Testing Title",
    subtitle = "Testing Subtitle"
  )

leaderboard3