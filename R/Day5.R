library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

readRaw <- function(file){
  
  readLines(file)
  
}

extractInit <- function(rawData){
  
  spltInd <- grep("^ 1", rawData)
  stackCt <- rawData[spltInd] |> 
    strsplit(" ") |> 
    unlist() |> 
    as.integer() |>
    max(na.rm = TRUE) 
  
  idPos <- seq(from = 1, to = stackCt*4 - 3, by = 4) + 1
  
  rawData[seq(spltInd - 1)] |> 
  map(
    \(line) map_chr(
      idPos, 
      ~str_sub(line, start = .x, end = .x) 
    ) |> 
        set_names(paste0("stack_", seq(stackCt)))
  ) |> 
    bind_rows() |> 
    map(str_subset, pattern = "[[:alpha:]]")
  
}

extractMoves <- function(rawData){
  
  spltInd <- grep("^ 1", rawData)
  
  tail(rawData, -(spltInd+1)) |> 
    str_extract_all("[[:digit:]]+") |> 
    map(
      ~as.integer(.x) |> 
        set_names(c("boxCt", "from", "to")) 
    ) 
  
}

makeMoves <- function(stacks, moves) {
  
  for(move in moves){
    
    toMove <- stacks |> 
      pluck(move["from"]) |> 
      head(n = move["boxCt"]) |> 
      rev()
    
    stacks[[move["from"]]] <- tail(
      stacks[[move["from"]]], n = -move["boxCt"]
    )
    
    stacks[[move["to"]]] <- c(
      toMove,
      stacks[[move["to"]]]
    )
    
  }
  
  stacks
  
}

makeMoves2 <- function(stacks, moves) {
  
  for(move in moves){
    
    toMove <- stacks |> 
      pluck(move["from"]) |> 
      head(n = move["boxCt"]) 
    
    stacks[[move["from"]]] <- tail(
      stacks[[move["from"]]], n = -move["boxCt"]
    )
    
    stacks[[move["to"]]] <- c(
      toMove,
      stacks[[move["to"]]]
    )
    
  }
  
  stacks
  
}

topCrates <- function(stacks) {
  
  stacks |> 
    map_chr(~head(.x, 1)) |> 
    str_c(collapse = "")
  
}

# sample
sample_raw <- "../data/day5_sample.txt" |> 
  readRaw() 

sample_pos <- sample_raw |> 
  extractInit() 
  
sample_moves <- sample_raw |> 
  extractMoves() 

sample <- makeMoves(sample_pos, sample_moves) |> 
  topCrates() |> 
  print()

sample2 <- makeMoves2(sample_pos, sample_moves) |> 
  topCrates() |> 
  print()

# puzzle
part1_raw <- "../data/day5.txt" |> 
  readRaw()

part1 <- makeMoves(
  extractInit(part1_raw), extractMoves(part1_raw)
) |> 
  topCrates() |> 
  print()
  
part2 <- makeMoves2(
  extractInit(part1_raw), extractMoves(part1_raw)
) |> 
  topCrates() |> 
  print()