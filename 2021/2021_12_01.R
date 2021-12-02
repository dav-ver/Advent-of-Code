## December 1st 2021

library(tidyverse)
library(RcppRoll)

data = read.csv("input_2021_12_01.txt", header = FALSE)

answer = data %>%
  mutate(increase = case_when(V1 > lag(V1, 1) ~ TRUE,
                              TRUE ~ FALSE)) %>%
  summarise(sum(increase)) %>%
  as.numeric()

answer


## PART 2

answer2 = data %>%
  mutate(rolling_sum = roll_sum(V1, n= 3, align = "right", fill = NA),
         increase = case_when(rolling_sum > lag(rolling_sum, 1) ~ TRUE,
                              TRUE ~ FALSE)) %>%
  summarise(sum(increase)) %>%
  as.numeric()

answer2
