library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

readData <- function(file){
  
  byRow <- file |> 
    readLines() |> 
    strsplit(split = "") |> 
    map(as.integer)
   
  
  byCol <- byRow |> 
    map(~set_names(.x, paste0("c", seq_along(.x)))) |> 
    bind_rows()
  
  list(rows =  set_names(byRow, paste0("r", seq_along(byRow))), cols = byCol)
  
}

calcVizVec<- function(data){
  
  rowCt <- length(data$rows)
  colCt <- length(data$cols)
  treeCt <- rowCt*colCt
  
  visTrees <- logical(treeCt)
  i <- 1
  
  for(row in seq(rowCt)){
    for(col in seq(colCt)){
      
      currHeight <- data$rows[[row]][[col]]
      
      if(row == 1 || row == rowCt || col == 1 || col == colCt){
        # outside tree
        visTrees[i] <- TRUE
      } else{
        
        visTrees[i] <- any(
          # up
          all(head(data$cols[[col]], row - 1) < currHeight),
          # down
          all(tail(data$cols[[col]], -row) < currHeight),
          # left
          all(head(data$rows[[row]], col - 1) < currHeight),
          # right
          all(tail(data$rows[[row]], -col) < currHeight)
        )
        
      }
      
      i <- i + 1
    }
  }
  
  visTrees
  
}

calcViewDist <- function(ref, los){
  
  viewRLE <- rle(los < ref)
  
  # add one for tree that's taller that can be seen
  # but not longer than line of sight (LOS)
  min(viewRLE$values[1]*viewRLE$lengths[1] + 1, length(los))
  
}

calcScenicVec <- function(data){
  
  rowCt <- length(data$rows)
  colCt <- length(data$cols)
  treeCt <- rowCt*colCt
  
  sceneScores <- integer(treeCt)
  i <- 1
  
  for(row in seq(rowCt)){
    for(col in seq(colCt)){
      
      currHeight <- data$rows[[row]][[col]]
      
      if(row == 1){
        upView <- 0
      } else {
        upView <- calcViewDist(currHeight, rev(head(data$cols[[col]], row - 1)))
      }
      
      if(row == rowCt) {
        downView <- 0
      } else {
        downView <- calcViewDist(currHeight, tail(data$cols[[col]], -row))
      }
      
      if(col == 1) {
        leftView <- 0
      } else {
        leftView <- calcViewDist(currHeight, rev(head(data$rows[[row]], col - 1)))
      }
      
      if(col == colCt){
        rightView <- 0
      } else {
        rightView <- calcViewDist(currHeight, tail(data$rows[[row]], -col))
      }
      
      sceneScores[i] <- upView*downView*leftView*rightView
    
      i <- i + 1
    }
    
  }
  
  sceneScores
  
}

# sample

sample_data <- "../data/day8_sample.txt" |> 
  readData()

sample_viz <- sample_data |> 
  calcVizVec() 

sample_viz |> 
  matrix(nrow = length(sample_data$rows), byrow = TRUE) |> 
  print()

sample_viz |> 
  sum() |> 
  print()

sample_view <- sample_data |> 
  calcScenicVec()

sample_view |> 
  matrix(nrow = length(sample_data$rows), byrow = TRUE) |> 
  print()

sample_view |> 
  max() |> 
  print()

# puzzle
part1_data <- "../data/day8.txt" |> 
  readData()

part1_viz <- part1_data |> 
  calcVizVec() 

part1_viz |> 
  sum() |> 
  print()

part2_view <- part1_data |> 
  calcScenicVec()

part2_view |> 
  max() |> 
  print()
