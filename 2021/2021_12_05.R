## December 5th 2021

## PART 1

library(tidyverse)

data = read.csv("input_2021_12_05.txt", sep = ".", header = FALSE) %>%
  mutate(V1 = str_replace_all(V1, " -> ", ",")) %>%
  separate(V1, into = c("x1", "y1", "x2", "y2"), sep = ",", convert = TRUE)

data_straight_lines = data %>%
  filter(x1 == x2 | y1 == y2)

#Calculcate all coordinates on line between a pair of pts
coords = function(x1, y1, x2, y2) {
  x_diff = length(seq(x1, x2))
  y_diff = length(seq(y1, y2))
  # Not good algebra (as c^2 = a^2 + b^2) but does work out total pts needed
  max_diff = max(x_diff, y_diff)
  x_pts = seq(from = x1, to = x2, length.out = max_diff)
  y_pts = seq(y1, y2, length.out = max_diff)
  coords = data.frame(x_pts, y_pts) %>%
    # Remove non integers. On a grid, this retains only straight and 45° lines
    filter(y_pts%%1==0)
  return(coords)
}

# For a dataframe of paired pts, calculate all coordinates on all lines
all_coords = function(df) {
  answer = data.frame()
  for (i in 1:nrow(df)) {
    line = coords(df[i,1], df[i,2], df[i,3], df[i,4])
    answer = (bind_rows(answer, line))
  }
  # Summarise: how often do pairs appear, how many twice or more?
  answer = answer %>%
    group_by(x_pts, y_pts) %>%
    summarise(total = n()) %>%
    filter(total >= 2) %>%
    nrow()
  return(answer)
}

answer = all_coords(data_straight_lines)
  
answer


## PART 2

answer2 = all_coords(data)

answer2