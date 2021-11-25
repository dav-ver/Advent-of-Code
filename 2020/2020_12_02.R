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