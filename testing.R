input_df <- read.csv("data/input_2023_w01.csv")
output_df <- read.csv("data/output_2023_w01.csv")

test2 <- set_week_values(1)
test2 <- test2 %>% filter(game_id == 2023090700, play_id == 194)

# output_players <- output_df %>%
#   distinct(game_id, play_id, nfl_id)
# 
# filtered_input <- input_df %>%
#   semi_join(output_players, by = c("game_id", "play_id", "nfl_id"))
# 
# nrow(input_df)
# nrow(filtered_input)
# filtered_input %>% distinct(player_side)

# test4 <- combine_week(1)
# test4 <- test4 %>% filter(game_id == 2023090700, play_id == 621)
# test4 <- set_distance_values(test4)
# write.csv(test4, "test4.csv", row.names = FALSE)
# write.csv(test1, "test1.csv", row.names = FALSE)

# input_test <- input_df %>% filter(game_id == 2023090700, play_id == 621)
# output_test <- output_df %>% filter(game_id == 2023090700, play_id == 621)
# output_test_players <- output_test %>%
#   distinct(game_id, play_id, nfl_id)
# filtered_input_test <- input_test %>%
#   semi_join(output_test_players, by = c("game_id", "play_id", "nfl_id"))
# distance_test <- set_distance_values(filtered_input_test)
# write.csv(distance_test, "filtered.csv", row.names = FALSE)
