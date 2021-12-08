## December 03 2021 - completed with Katie and Bea

## PART 1

library(tidyverse)
library(assertr)

data = read.fwf(file = "~/R_projects/Advent-of-Code/2021/input_2021_12_03.txt", width = rep(1, 12))

# Mean by column, round, bind to inverse, convert back to single string
# and multiply the integer versions
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

# Function: calculate if 1s and 0s tie, and if not what the modal value is
# Conditional filter on those criteria, break when 1 row remains
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

# Loop through with function for O2 and CO2
data_o2 = data_concat
for (i in 1:12) {
  data_o2 = keep_mode(data = data_o2, col_num = i, tie_break = 1, keep_mode = T)
}

data_co2 = data_concat
for (i in 1:8) {
  data_co2 = keep_mode(data = data_co2, col_num = i, tie_break = 0, keep_mode = F)
}

# Bind two answers together, convert to binary then decimal and multiply
answer2 = data_o2 %>%
  bind_rows(data_co2) %>%
  select(number) %>%
  mutate(number = strtoi(number, 2)) %>%
  prod()

answer2
