## December 5th 2021

## PART 1

library(tidyverse)

data = read.csv("input_2021_12_05.txt", sep = ".", header = FALSE) %>%
  mutate(V1 = str_replace_all(V1, " -> ", ",")) %>%
  separate(V1, into = c("x1", "y1", "x2", "y2"), sep = ",", convert = TRUE)

data_straight_lines = data %>%
  filter(x1 == x2 | y1 == y2)

coords = function(row) {
  x1 = row[1]
  y1 = row[2]
  x2 = row[3]
  y2 = row[4]
  x_diff = length(seq(x1, x2))
  y_diff = length(seq(y1, y2))
  # This is not good algebra (since c^2 = a^2 + b^2)
  # But it does tell me how many points I need to calculate per line
  max_diff = max(x_diff, y_diff)
  x_pts = seq(from = x1, to = x2, length.out = max_diff)
  y_pts = seq(y1, y2, length.out = max_diff)
  coords = data.frame(x_pts, y_pts) %>%
    filter(y_pts%%1==0)
  return(coords)
}

answer = data.frame()
for (i in 1:length(data_straight_lines[[1]])) {
  line = coords(as.numeric(data_straight_lines[i,])) %>%
    mutate(line = i)
  answer = bind_rows(answer, line) 
}

answer = answer %>%
  group_by(x_pts, y_pts) %>%
  summarise(total = n()) %>%
  filter(total >= 2) %>%
  nrow()
  
answer


## PART 2

answer2 = data.frame()
for (i in 1:length(data[[1]])) {
  line = coords(as.numeric(data[i,])) %>%
    mutate(line = i)
  answer4 = bind_rows(answer2, line) 
}

answer2 = answer2 %>%
  group_by(x_pts, y_pts) %>%
  summarise(total = n()) %>%
  filter(total >= 2) %>%
  nrow()

answer2

