library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

readBuffer<- function(file){
  
  readLines(file) |> 
    strsplit(split = "") |> 
    unlist()
  
}

splitCodons <- function(buffer, n = 4){
  
  offset <- n - 1
  
  indices <- setdiff(seq_along(buffer), seq(offset))
  
  map(indices, ~buffer[seq(.x-offset, .x)])
  
}

isStart <- function(codon){
  
  length(codon) == length(unique(codon))
  
}

sb <-c(
  "mjqjpqmgbljsphdztnvjfqwrcgsmlb",
  "bvwbjplbgvbhsrlpgdmjqwftvncz",
  "nppdvjthqldpwncqszvftbrmjlhg",
  "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
  "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
)

sptSamps <- map(sb, ~strsplit(.x, split = "") |> unlist())

solve_part1 <- function(file){
  
  file |> 
    readBuffer() |> 
    splitCodons() |> 
    map_lgl(isStart) |> 
    which.max() |> 
    magrittr::add(3)
  
}

solve_part2 <-  function(file){
  
  file |> 
    readBuffer() |> 
    splitCodons(n = 14) |> 
    map_lgl(isStart) |> 
    which.max() |> 
    magrittr::add(13)
  
}

# sample part 1
map_dbl(
  sptSamps,
  ~splitCodons(.x) |> 
    map_lgl(isStart) |> 
    which.max() |> 
    magrittr::add(3)
)

# sample part 2
map_dbl(
  sptSamps,
  ~splitCodons(.x, 14) |> 
    map_lgl(isStart) |> 
    which.max() |> 
    magrittr::add(13)
)

part1 <- solve_part1("../data/day6.txt") |> 
  print()

part2 <- solve_part2("../data/day6.txt") |> 
  print()

