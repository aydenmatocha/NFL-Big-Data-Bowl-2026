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