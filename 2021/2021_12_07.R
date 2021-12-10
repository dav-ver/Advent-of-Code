## December 7th 2021

## PART 1

library(tidyverse)

data = scan("input_2021_12_07.txt", sep = ",")

min_position = min(data)
max_position = max(data)

# fucniton to find sum of differentials from a given position
fuel_expended = function(new_position, intial_positions) {
  total_fuel = sum(abs(intial_positions - new_position))
  return(total_fuel)
}

# Find sum of differentials (fuel) for each possible new position, select minimum
answer = data.frame(new_position = seq(min_position, max_position)) %>%
  mutate(total_fuel = sapply(new_position, fuel_expended, data)) %>%
  filter(total_fuel == min(total_fuel)) %>%
  select(total_fuel) %>%
  as.numeric()

answer


## PART 2

# function to find differentials, apply "sum factorial" to them, then sum total
fuel_expended2 = function(new_position, intial_positions) {
  total_fuel = abs(intial_positions - new_position)
  #Inspo: https://www.quora.com/Is-there-a-factorial-function-but-for-addition
  total_fuel = (total_fuel * (total_fuel + 1)) / 2
  total_fuel = sum(total_fuel)
  return(total_fuel)
}

answer2 = data.frame(new_position = seq(min_position, max_position)) %>%
  mutate(total_fuel = sapply(new_position, fuel_expended2, data)) %>%
  filter(total_fuel == min(total_fuel)) %>%
  select(total_fuel) %>%
  as.numeric()

answer2