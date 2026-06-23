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

play_summary <- week1 %>%
  group_by(game_id, play_id) %>%
  group_modify(~ {
    # data for defender
    last_frame <- .x %>% pull(throw_frame) %>% unique()
    def <- .x %>% filter(phase == "pre_throw", man_def == TRUE, frame_id == last_frame) # should be 1 row
    off <- .x %>% filter(phase == "pre_throw", player_role == "Targeted Receiver", frame_id == last_frame) # should be 1 row

    tibble(
      # def stats
      def_dist_at_throw = def$dist_from_rec,
      def_s_at_throw = def$s,
      def_a_at_throw = def$a,
      def_dir_at_throw = def$dir,
      def_o_at_throw = def$o,
      
      # off stats
      off_s_at_throw = off$s,
      off_a_at_throw = off$a,
      off_dir_at_throw = off$dir,
      off_o_at_throw = off$o,
      
      # misc
      
      
      #actual closing sep
      actual_closing_sep = unique(.x$closing_sep)
    )
  }) %>%
  ungroup()