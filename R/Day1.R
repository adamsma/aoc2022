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
maxCalories("../data/day1_sample.txt", n = 3) |> 
  summarize(topCalories = sum(calories))

# part 1 
maxCalories("../data/day1_part1.txt")

# part 2 
maxCalories("../data/day1_part1.txt", n = 3) |> 
  summarize(topCalories = sum(calories))
