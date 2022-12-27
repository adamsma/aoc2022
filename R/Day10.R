library(dplyr)
library(purrr)

TARGET_CYCLES <- c(20, 60, 100, 140, 180, 220)

OPS <- list(
  noop = \(init, add) init,
  addx = \(init, add) c(init, init + add)
)

readOps <- function(file) {
  
  file |> 
    readLines() |> 
    strsplit(split = " ")
  
}

calcCycles <- function(opList){
  
  reduce(opList, ~.x + ifelse(.y[1] == "noop", 1, 2), .init = 0)
  
}

processOps <- function(opList){
  
  regValue <- 1
  
  for(op in opList){
    
    regValue <- append(
      regValue, 
      OPS[[op[1]]](tail(regValue, 1), as.numeric(op[2]))
    )
    
  }
  
  tibble(cycle = seq_along(regValue), register = regValue)
  
}

drawPixels <- function(regHist){
  
  crt <- rep(".", 40*6)
  
  for(i in regHist$cycle){
    
    spritePos <- regHist$register[i] + -1:1
    
    col <- (i - 1) %% 40
    
    if(col %in% spritePos) crt[i] <- "#"
    
  }
  
  crt |> 
    matrix(nrow = 6, byrow = TRUE)
  
}
drawCRT <- function(crt){
  
  map_chr(1:6, ~paste0(crt[.x, ], collapse = "")) |> 
    writeLines()
  
  invisible(crt)
  
}

# sample
preSample_ops <- list(
  c("noop"),
  c("addx", "3"),
  c('addx', "-5")
) 

preSample_part1 <- preSample_ops |> 
  processOps() 

drawPixels(preSample_part1) |>
  drawCRT()

sample_ops <- readOps("../data/day10_sample.txt")|> 
  processOps()

sample_part1 <- sample_ops |> 
  slice(TARGET_CYCLES) |> 
  mutate(sigStrength = cycle * register) |>
  summarize(total = sum(sigStrength)) |> 
  print()

sample_ops |> 
  drawPixels() |> 
  drawCRT()

# puzzle
puzzle_data <- readOps("../data/day10.txt") |> 
  processOps()

part1 <- puzzle_data|> 
  slice(TARGET_CYCLES) |> 
  mutate(sigStrength = cycle * register) |>
  summarize(total = sum(sigStrength)) |> 
  print()

part2 <- puzzle_data |> 
  drawPixels() |> 
  drawCRT()
