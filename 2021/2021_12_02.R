# December 2nd 2021

library(tidyverse)

data  = read.csv("input_2021_12_02.txt", header = FALSE, sep = "") %>%
  rename(direction = V1, distance = V2)


vertical = sum(data$distance[data$direction == "down"]) - 
                 sum(data$distance[data$direction == "up"])
                     
horizontal = sum(data$distance[data$direction == "forward"])

answer = vertical * horizontal

answer



## PART 2

data2 = data %>%
  mutate(aim_change = case_when(direction == "down" ~ distance,
                                direction == "up" ~ -distance, 
                                TRUE ~ as.integer(0)),
         aim = cumsum(aim_change),
         depth_change = case_when(direction == "forward" ~ aim*distance,
                                  TRUE ~ as.integer(0)))

vertical = sum(data2$depth_change)

horizontal = sum(data2$distance[data2$direction == "forward"])

answer2 = vertical * horizontal

answer2
                 