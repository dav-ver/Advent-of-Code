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

data2 = data_concat
for (i in 1:12) {
  if (sum(data2[[i]]) == length(data2[[i]])/2) {
    data2 = data2 %>%
      dplyr::filter(data2[[i]] == 1)
  }
  else {
    data2 = data2 %>%
      dplyr::filter(data2[[i]] == round(mean(data2[[i]])))
  }
  if (length(data2[[1]]) == 1) {
    break
  }
}

data3 = data_concat
for (i in 1:12) {
  if (sum(data3[[i]]) == length(data3[[i]])/2) {
    data3 = data3 %>%
      dplyr::filter(data3[[i]] == 0)
  }
  else {
    data3 = data3 %>%
      dplyr::filter(data3[[i]] != round(mean(data3[[i]])))
  }
  if (length(data3[[1]]) == 1) {
    break
  }
}

answer2 = data2 %>%
  bind_rows(data3) %>%
  select(number) %>%
  mutate(number = strtoi(number, 2)) %>%
  prod()

answer2


#attempt 2

data_test = data %>%
  bind_rows(data %>% summarise_all(.funs = ~round(mean(.))))

find_mode = function(data, column_num, tie_result) {
  column = data[[column_num]]
  col_sum = sum(column)
  col_length = length(column)
  if (2*col_sum == col_length) {
    col_mode = tie_result
  }
  else {
    col_mode = round(col_sum/col_length)
  }
  return(col_mode)
  #data = data %>%
  #  filter(column == col_mode)
  #return(data)
}

data_concat_o2 = data_concat
for (i in 1:12) {
  mode = find_mode(data_concat_o2, i, 1)
  data_concat_o2 = data_concat_o2 %>%
    filter(data_concat_o2[[i]] == mode)
  if(nrow(data_concat_o2) == 1) {
    break
  }
}

data_concat_co2 = data_concat
for (i in 1:12) {
  mode = find_mode(data_concat_co2, i, 0)
  data_concat_co2 = data_concat_co2 %>%
    filter(data_concat_co2[[i]] != mode)
  if(nrow(data_concat_co2) == 1) {
    break
  }
}

strtoi(data_concat_o2$number, 2) * strtoi(data_concat_co2$number, 2)




