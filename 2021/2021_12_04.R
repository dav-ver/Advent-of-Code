## December 4th 2021

## PART 1

library(tidyverse)

data_balls = scan("input_2021_12_04.txt", nlines = 1, sep = ",")

data_cards = read.csv(file = "input_2021_12_04.txt", header = FALSE, skip = 1) %>%
  mutate(V1 = str_trim(str_replace_all(V1, "  ", " "))) %>%
  separate(V1, into = c("V1", "V2", "V3", "V4", "V5"), sep = " ", convert = TRUE)

# Find row at centre of 5 consecutive hits (within a card - modulo tests this)
five_down_id = function (column) {
  roll_sums = roll_sum(column, n = 5, align = "center", fill = 0)
  winning_row_down = which(roll_sums == 5*999)
  winning_row_down = winning_row_down[which(winning_row_down %% 5 == 3)]
    return(winning_row_down)
}

# Find row with sum equal to 5 * the hit value (999)
five_along_id = function(data) {
  data$row_sum = rowSums(data)
  winning_row_along = which(data$row_sum == 5*999)
  data = data %>% select(-row_sum)
    return(winning_row_along)
}

# Isolate bingo card containing the winning row
select_winning_card = function (data, row_id) {
  start_row = ((row_id %/% 5) * 5) + 1
  card = data[start_row:(start_row+4), ]
  return(card)
}

# Apply above functions to every row and column until bingo row occurs
for (i in data_balls) {
  data_cards[data_cards == i] = 999
  winning_row = c(unname(unlist(lapply(data_cards[,1:5], five_down_id))),
                  five_along_id(data_cards))
  if (length(winning_row) > 0) { break }
}

# Use bingo row (5 across row, or middle of 5 down) to calculate answer
winning_card = select_winning_card(data_cards, winning_row)
winning_card[winning_card == 999] = 0
answer = sum(colSums(winning_card)) * i

answer


## PART 2

data_cards = read.csv(file = "input_2021_12_04.txt", header = FALSE, skip = 1) %>%
  mutate(V1 = str_trim(str_replace_all(V1, "  ", " "))) %>%
  separate(V1, into = c("V1", "V2", "V3", "V4", "V5"), sep = " ", convert = TRUE)

winning_order_cards = c()
for (i in data_balls) {
  data_cards[data_cards == i] = 999
  winning_rows = c(unname(unlist(lapply(data_cards[,1:5], five_down_id))),
                  five_along_id(data_cards))
  winning_cards = unique(5*(winning_rows %/% 5) + 1)
  winning_cards = winning_cards[!(winning_cards %in% winning_order_cards)]
  winning_order_cards = c(winning_order_cards, winning_cards)
  if(length(winning_order_cards) == 100) {
    break
  }
}

last_winning_card = select_winning_card(data_cards, winning_order_cards[100])
last_winning_card[last_winning_card == 999] = 0
answer2 = sum(colSums(last_winning_card)) * i

answer2



