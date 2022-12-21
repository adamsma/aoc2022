library(dplyr)
library(readr)
library(tibble)

maxCalories <- function(dataFile, n = 1){
  
  tibble(raw = readLines(dataFile)) |> 
    mutate(
      newElf = raw == "",
      elfId  = cumsum(newElf)
    ) |> 
    filter(!newElf) |>
    group_by(elfId) |> 
    summarise(calories = sum(as.numeric(raw))) |> 
    slice_max(calories, n = n)
  
}

# sample data
maxCalories("../data/day1_sample.txt")

# part 1 
maxCalories("../data/day1_part1.txt", 3)

# part 2 