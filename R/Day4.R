library(readr)
library(dplyr)
library(tidyr)
library(purrr)

readData <- function(file) {
  
  file |> 
    read_csv(col_names = c("elf1", "elf2"), col_types = "cc") |> 
    separate(elf1, c("elf1_min", "elf1_max"), sep = "-") |> 
    separate(elf2, c("elf2_min", "elf2_max"), sep = "-") |> 
    mutate(
      elf1_set = map2(elf1_min, elf1_max, seq),
      elf2_set = map2(elf2_min, elf2_max, seq)
    )
  
}

isSubset <- function(set1, set2){
  
  if(length(set1) >= length(set2)){
    longSet <- set1
    shortSet <- set2
  } else {
    longSet <- set2
    shortSet <- set1
  }
  
  intersect(longSet, shortSet) |> 
    identical(shortSet)
  
}

intersectLen <- function(set1, set2) {
  
  intersect(set1, set2) |> 
    length()
  
}

solve_part1 <- function(file) {
  
  file |> 
    readData() |> 
    mutate(
      totalOverlap = map2_lgl(elf1_set, elf2_set, isSubset)
    ) |> 
    summarize(total = sum(totalOverlap)) |> 
    print()
  
}

solve_part2 <- function(file) {
  
  file |> 
    readData() |> 
    mutate(
      intLen = map2_int(elf1_set, elf2_set, intersectLen),
      isOverlap = intLen > 0
    ) |> 
    summarize(total = sum(isOverlap)) |>
    print()
  
}

# sample
sample <- "../data/day4_sample.txt" |> 
  solve_part1()

sample2 <- "../data/day4_sample.txt" |>
  solve_part2()

part1 <- "../data/day4.txt" |> 
  solve_part1()

part2 <- "../data/day4.txt" |>
  solve_part2()
