# December 2nd 2020

library(tidyverse)

data  = read.csv("input_2020_12_02.txt", header = FALSE, sep = " ")

answer = data %>%
  select(rule = V1, character = V2, password = V3) %>%
  # Inspiration: https://statisticsglobe.com/extract-substring-before-or-after-pattern-in-r
  mutate(min = as.numeric(sub("-.*", "", rule)),
         max = as.numeric(sub(".*-", "", rule)),
         character = str_remove(character, ":"),
         min_test = str_count(password, character) >= min,
         max_test = str_count(password, character) <= max) %>%
  filter(min_test == TRUE, max_test == TRUE) %>%
  nrow()

answer


#Works but I'm sure this could be done in 2 lines



## PART 2

answer_2 = data %>%
  select(rule = V1, character = V2, password = V3) %>%
  # Inspiration: https://statisticsglobe.com/extract-substring-before-or-after-pattern-in-r
  mutate(position_1 = as.numeric(sub("-.*", "", rule)),
         position_2 = as.numeric(sub(".*-", "", rule)),
         character = str_remove(character, ":"),
         position_1_test = str_sub(password, position_1, position_1) == character,
         position_2_test = str_sub(password, position_2, position_2) == character) %>%
  filter((position_1_test == TRUE | position_2_test == TRUE),
         position_1_test != position_2_test) %>%
  nrow()

answer_2

#Again, this works but it's super long and ugly
  
