library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)


findDuplicateType <- function(str1, str2){
  
  vec1 <- str_split(str1, "") |> unlist()
  vec2 <- str_split(str2, "") |> unlist()
  
  intersect(vec1,vec2)
  
}


priorityScore <- function(char) {
  
  priorityList <- c(letters, LETTERS)
  
  map_int(char, ~which(priorityList == .x, arr.ind = TRUE ))
  
}

solve_part1 <- function(file){
  
  file |> 
    readLines() |> 
    tibble() |> 
    rename_with(\(x) "raw") |> 
    mutate(
      spltPt = str_length(raw)/2,
      compart_1 = str_sub(raw, end = spltPt),
      compart_2 = str_sub(raw, start = spltPt+1),
      common = map2_chr(compart_1, compart_2, findDuplicateType),
      score = map_int(common, priorityScore)
    ) |> 
    print() |> 
    summarize(total = sum(score)) |> 
    print()
}

solve_part2 <- function(file){
  
  file |> 
    readLines() |> 
    tibble() |> 
    rename_with(\(x) "raw") |> 
    mutate(
      group = rep(seq(length(raw)/3), each = 3)
    ) |> 
    group_by(group) |> 
    summarize(
      common = findDuplicateType(
        findDuplicateType(first(raw), nth(raw, n = 2)), 
        last(raw)
      )
    ) |> 
    mutate(
      score = map_int(common, priorityScore)
    ) |> 
    print() |> 
    summarize(total = sum(score)) |>
    print()
  
}

# sample
sample <- "../data/day3_sample.txt" |> 
  solve_part1()

sample2 <- "../data/day3_sample.txt" |> 
  solve_part2()

# part 1
part1 <- "../data/day3.txt" |> 
  solve_part1()

part2 <- "../data/day3.txt" |> 
  solve_part2()
  
