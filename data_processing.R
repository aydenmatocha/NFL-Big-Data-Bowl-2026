library(tidyverse)

week1 <- set_week_values(1) %>% mutate (week = 1)
week2 <- set_week_values(2) %>% mutate (week = 2)
week3 <- set_week_values(3) %>% mutate (week = 3)
week4 <- set_week_values(4) %>% mutate (week = 4)
week5 <- set_week_values(5) %>% mutate (week = 5)
week6 <- set_week_values(6) %>% mutate (week = 6)
week7 <- set_week_values(7) %>% mutate (week = 7)
week8 <- set_week_values(8) %>% mutate (week = 8)
week9 <- set_week_values(9) %>% mutate (week = 9)
week10 <- set_week_values(10) %>% mutate (week = 10)
week11 <- set_week_values(11) %>% mutate (week = 11)
week12 <- set_week_values(12) %>% mutate (week = 12)
week13 <- set_week_values(13) %>% mutate (week = 13)
week14 <- set_week_values(14) %>% mutate (week = 14)
week15 <- set_week_values(15) %>% mutate (week = 15)
week16 <- set_week_values(16) %>% mutate (week = 16)
week17 <- set_week_values(17) %>% mutate (week = 17)
week18 <- set_week_values(18) %>% mutate (week = 18)

combined_weeks <- rbind(week1, week2, week3, week4, week5, week6, week7, week8, 
  week9, week10, week11, week12, week13, week14, week15, week16, week17, week18)
# write to csv?

player_names <- combined_weeks %>%
  distinct(nfl_id, player_name)

# Summarize every play given a df
summarize_plays <- function(input_df) {
  play_summary <- input_df %>%
    group_by(game_id, play_id) %>%
    group_modify(~ {
      last_frame <- .x %>% pull(throw_frame) %>% unique()
      # data for defender
      def <- .x %>% filter(phase == "pre_throw", man_def == TRUE, frame_id == last_frame) # should be 1 row
      # data for receiver
      off <- .x %>% filter(phase == "pre_throw", player_role == "Targeted Receiver", frame_id == last_frame) # should be 1 row
      
      tibble(
        # def stats
        def_dist_at_throw = def$dist_from_rec,
        def_s_at_throw = def$s,
        def_a_at_throw = def$a,
        def_dir_at_throw = def$dir,
        def_o_at_throw = def$o,
        def_nfl_id = unique(def$nfl_id),
        
        # off stats
        off_s_at_throw = off$s,
        off_a_at_throw = off$a,
        off_dir_at_throw = off$dir,
        off_o_at_throw = off$o,
        off_nfl_id = unique(off$nfl_id),
        
        # misc
        ball_land_x = unique(.x$ball_land_x),
        ball_land_y = unique(.x$ball_land_y),
        play_direction = unique(.x$play_direction),
        route_of_targeted_receiver = unique(.x$route_of_targeted_receiver),
        pass_location_type = unique(.x$pass_location_type),
        team_coverage_man_zone = unique(.x$team_coverage_man_zone),
        team_coverage_type = unique(.x$team_coverage_type),
        pass_length = unique(.x$pass_length),
        #pass_result = unique(.x$pass_result), # Probably not, but lets see
        
        #actual closing sep
        actual_closing_sep = unique(.x$closing_sep)
      )
    }) %>%
    ungroup()
  return(play_summary)
}

normalize_angle <- function(angle, play_direction) {
  if_else(play_direction == "left", (angle + 180) %% 360, angle)
}

# week1_summary <- summarize_plays(week1)
# week2_summary <- summarize_plays(week2)
# week3_summary <- summarize_plays(week3)
# week4_summary <- summarize_plays(week4)
# week5_summary <- summarize_plays(week5)
# week6_summary <- summarize_plays(week6)
# week7_summary <- summarize_plays(week7)
# week8_summary <- summarize_plays(week8)
# week9_summary <- summarize_plays(week9)
# week10_summary <- summarize_plays(week10)
# week11_summary <- summarize_plays(week11)
# week12_summary <- summarize_plays(week12)
# week13_summary <- summarize_plays(week13)
# week14_summary <- summarize_plays(week14)
# week15_summary <- summarize_plays(week15)
# week16_summary <- summarize_plays(week16)
# week17_summary <- summarize_plays(week17)
# week18_summary <- summarize_plays(week18)
combined_summary <- summarize_plays(combined_weeks) %>% filter(team_coverage_man_zone == "MAN_COVERAGE") %>%
  mutate(
    def_dir_at_throw = normalize_angle(def_dir_at_throw, play_direction),
    def_o_at_throw   = normalize_angle(def_o_at_throw, play_direction),
    off_dir_at_throw = normalize_angle(off_dir_at_throw, play_direction),
    off_o_at_throw   = normalize_angle(off_o_at_throw, play_direction)
  )