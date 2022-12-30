library(igraph)
library(magrittr)
library(purrr)
library(dplyr)
library(stringr)

getCoord <- function(data, pattern){
  
  findLetter <- str_locate(data, pattern)[,1] 
  
  startRow <- findLetter |> 
    na.omit() |> 
    as.vector()
  
  startCol <- findLetter |> 
    is.na() |> 
    not() |> 
    which()
  
  c(startRow, startCol)
  
}

getIndex <- function(data, pattern){
  
  data |> 
    paste0(collapse = "") |> 
    str_split("") |> 
    pluck(1) |> 
    str_which(pattern)
  
}

makeDataNodes <- function(dataRows){
  
  baseline <- utf8ToInt("a")
  
  dataRows |> 
    str_c(collapse = "") |> 
    # replace start and end with elevations
    str_replace("S", "a") |> 
    str_replace("E", "z") |> 
    utf8ToInt() 
  
}

makeElevGraph <- function(data, start, end){
  
  dataNodes <- makeDataNodes(data)
  nRows <- length(data)
  nCols <- str_length(data[1])
  
  elevGraph <- make_empty_graph(n = length(dataNodes), directed = TRUE)
  
  for(i in seq_along(dataNodes)){
      
      # check above
      if(i > nCols){
        
        aboveI <- i - nCols
        canMove <- (dataNodes[i] - dataNodes[aboveI]) >= -1
        if(canMove) elevGraph <- add_edges(elevGraph, c(i, aboveI))
        
      }
      
      # check right
      if((i %% nCols) != 0){
        
        rightI <- i + 1
        canMove <- (dataNodes[i] - dataNodes[rightI]) >= -1
        if(canMove) elevGraph <- add_edges(elevGraph, c(i, rightI))
        
      }
      
      # check left
      if((i %% nCols) != 1){
        
        leftI <- i - 1
        canMove <- (dataNodes[i] - dataNodes[leftI]) >= -1
        if(canMove) elevGraph <- add_edges(elevGraph, c(i, leftI))
        
      }
      
      # check below
      if(i <= ((nRows-1)*nCols)){
        belowI <- i + nCols
        canMove <- (dataNodes[i] - dataNodes[belowI]) >= -1
        if(canMove) elevGraph <- add_edges(elevGraph, c(i, belowI))
      }
    
    if((i %% 250) == 0) message(glue::glue("Mapping Index {i} Complete"))
      
  }
  
  elevGraph
  
}

# sample
sampleData <- readLines("../data/day12_sample.txt") 

sampleGraph <- makeElevGraph(sampleData)

plot(sampleGraph, layout = layout_on_grid)

sample_part1 <- shortest_paths(
  sampleGraph, 
  from = getIndex(sampleData, "S"), 
  to = getIndex(sampleData, "E")
) |> 
  pluck("vpath", 1) |> 
  length() |> 
  subtract(1) |> 
  print()

sample_part2 <-  shortest_paths(
  sampleGraph, 
  to = c(getIndex(sampleData, "S"), getIndex(sampleData, "a")),
  from = getIndex(sampleData, "E"),
  mode = "in"
) |> 
  pluck("vpath") |> 
  map_dbl(length) |> 
  min() |> 
  subtract(1) |> 
  print() 

# puzzle
puzzleData <- readLines("../data/day12.txt")

puzzleGraph <- makeElevGraph(puzzleData)

part1 <- shortest_paths(
  puzzleGraph,
  from = getIndex(puzzleData, "S"),
  to = getIndex(puzzleData, "E")
) |>
  pluck("vpath", 1) |>
  length() |>
  subtract(1) |>
  print()

part2 <- shortest_paths(
  puzzleGraph, 
  to = c(getIndex(puzzleData, "S"), getIndex(puzzleData, "a")),
  from = getIndex(puzzleData, "E"),
  mode = "in"
) |> 
  pluck("vpath") |> 
  map_dbl(length) |> 
  data.frame() |> 
  set_names("pathLen") |> 
  filter(pathLen > 0) |> 
  pluck("pathLen") |> 
  min() |> 
  subtract(1) |> 
  print() 
