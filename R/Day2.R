library(readr)
library(purrr)
library(dplyr)
library(tibble)

CHOICE_SCORES <- c(
  X = 1, Y = 2, Z = 3
)

MATCH_RESULT <- c(
  A = \(choice) switch(choice, X = 3, Y = 6, Z = 0),
  B = \(choice) switch(choice, X = 0, Y = 3, Z = 6),
  C = \(choice) switch(choice, X = 6, Y = 0, Z = 3)
)

RESULT_SCORE <- c(
  X = 0, Y = 3, Z = 6
)

RESULT_CHOICE <- c(
  A = \(choice) switch(choice, X = 3, Y = 1, Z = 2),
  B = \(choice) switch(choice, X = 1, Y = 2, Z = 3),
  C = \(choice) switch(choice, X = 2, Y = 3, Z = 1)
)

readData <- function(file){
  
  read_delim(
    file, 
    delim = " ", 
    col_names = c("opponent", "response"),
    show_col_types = FALSE
  )
  
}

calcScores <- function(data){
  
  data |> 
    mutate(
      selectScore = CHOICE_SCORES[response],
      matchScore = map2_dbl(MATCH_RESULT[opponent], response, ~.x(.y)),
      roundScore = selectScore + matchScore
    )
  
}

# sample 
readData("../data/day2_sample.txt") |> 
  calcScores() |> 
  summarize(total = sum(roundScore))

# part 1
readData("../data/day2.txt") |> 
  calcScores() |> 
  summarize(total = sum(roundScore))

# part 2
calcMoveScore <- function(data){
  
  data |> 
    mutate(
      selectScore = map2_dbl(RESULT_CHOICE[opponent], response, ~.x(.y)),
      matchScore = RESULT_SCORE[response],
      roundScore = selectScore + matchScore
    )
  
}

readData("../data/day2_sample.txt") |> 
  calcMoveScore() |> 
  summarize(total = sum(roundScore))

readData("../data/day2.txt") |> 
  calcMoveScore() |> 
  summarize(total = sum(roundScore))
