## December 6th 2021

## PART 1

library(tidyverse)
library(tictoc)

data = scan("input_2021_12_06.txt", sep = ",")

answer = data
for (i in 1:80) {
  answer = answer - 1
  birthing_fish = length(answer[answer == -1])
  answer[answer == -1] = 6
  answer = c(answer, rep(8, birthing_fish))
}

answer = length(answer)

answer


## PART 2

answer2 = data
for (i in 1:256) {
  answer2 = answer2 - 1
  birthing_fish = length(answer2[answer2 == -1])
  answer2 = replace(answer2, answer2 == -1, 6)
  answer2 = c(answer2, rep(8, birthing_fish))
}

answer2 = length(answer2)

answer2


## TESTING

answer = data
tic("total")
for (i in 1:80) {
  tic("Subtract 1 from vector")
   answer = answer - 1
   toc()
  tic("Calculate new fish total")
   birthing_fish = length(answer[answer == -1])
   toc()
  tic("Reset fish to 6")
   #answer[answer == -1] = 6
   replace(answer, answer == -1, 6)
   toc()
  tic("add new fish to vector")
   answer = c(answer, rep(8, birthing_fish))
   toc()
}
toc()




