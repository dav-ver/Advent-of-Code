## December 03 2021 - completed with Katie and Bea

library(tidyverse)
library(assertr)

data = read.fwf(file = "~/R_projects/Advent-of-Code/2021/input_2021_12_03.txt", width = rep(1, 12))

answer = data %>%
  summarise_all(mean) %>%
  round() %>%
  bind_rows(1 - .) %>%
  mutate(number = col_concat(.)) %>%
  select(number) %>%
  mutate(number = strtoi(number, 2)) %>%
  prod()

answer



## PART 2

data_concat = data %>%
  mutate(number = col_concat(.))

keep_mode = function(data, col_num, tie_break, keep_mode) { 
  column = data[[col_num]]
  tie = 2*sum(column) == length(column)
  col_mode = round(sum(column)/length(column))
  data = data %>%
  {if (tie) filter(., .[[col_num]] == tie_break)
   else if (keep_mode) filter(., .[[col_num]] == col_mode)
   else filter(., .[[col_num]] != col_mode)}
  return(data)
  if(nrow(data) == 1) {break}
}

data_o2 = data_concat
for (i in 1:12) {
  data_o2 = keep_mode(data = data_o2, col_num = i, tie_break = 1, keep_mode = T)
}

data_co2 = data_concat
for (i in 1:8) {
  data_co2 = keep_mode(data = data_co2, col_num = i, tie_break = 0, keep_mode = F)
}

answer2 = data_o2 %>%
  bind_rows(data_co2) %>%
  select(number) %>%
  mutate(number = strtoi(number, 2)) %>%
  prod()

answer2