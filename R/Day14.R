library(ggplot2)
library(purrr)
library(dplyr)

readRockData <- function(file){
  
  file |> 
    readLines() |> 
    strsplit(" -> ") |> 
    imap(
      \(line, lineNum) strsplit(line, split = ",") |>
        map(
          \(pts) list(x = as.integer(pts[1]), y = as.integer(pts[2]))
        ) |> 
        bind_rows() |> 
        mutate(type = paste0("ledge_", lineNum))
    )
  
}

visualizeCave <- function(caveData){
  
  rockData <- filter(caveData, type != "sand")
  sandData <- filter(caveData, type == "sand")
  
  rockData |> 
    ggplot(aes(x,y)) +
    geom_path(aes(group = type)) +
    geom_point(data = data.frame(x = 500, y = 0)) +
    geom_tile(
      data = sandData, 
      height = 1, width = 1, 
      color = "black", fill = "white"
    ) +
    scale_y_reverse() +
    coord_fixed() +
    theme_minimal()
  
}

expandPoints <- function(x1, y1, x2, y2){
  
  data.frame(x = seq(x1, x2), y = seq(y1, y2)) 
  
}

expandLedge <- function(corners){
  
  list(
    x1 = head(corners$x, -1),
    y1 = head(corners$y, -1),
    x2 = tail(corners$x, -1),
    y2 = tail(corners$y, -1)
  ) |> 
    pmap_dfr(expandPoints) |> 
    distinct(x, y) |> 
    bind_cols(data.frame(type = corners$type[1]))
  
}

# returns location of where sand settles
addSand <- function(cave, minX, maxX, maxY){
  
  filledPts <- paste(cave$x, cave$y, sep = ",")
  
  settled <- FALSE
  i <- 0
  loc <- c(500, 0)
  
  while(!settled && is.finite(loc[1]) && i < 2*maxY){
    
    if(!(paste(loc[1], loc[2]+1, sep = ",") %in% filledPts)){
      loc <- loc + c(0,1)
    } else if(!(paste(loc[1]-1, loc[2]+1, sep = ",") %in% filledPts)) {
      loc <- loc + c(-1, 1)
    } else if(!(paste(loc[1]+1, loc[2]+1, sep = ",") %in% filledPts)) {
      loc <- loc + c(1, 1)
    } else {
      settled <- TRUE
    }
    
    if((loc[1] < minX) || (loc[1] > maxX) || (loc[2] > maxY)){
      loc <- c(Inf, Inf)
    }
    
    i <- i +1 
    
  }
  
  data.frame(x = loc[1], y = loc[2], type = "sand")
  
}

# returns cave where no more sand will fall
fillWSand <- function(emptyCave){
  
  filled <- FALSE
  currentCave <- emptyCave
  i <- 1
  
  minX <- min(emptyCave$x)
  maxX <- max(emptyCave$x)
  maxY <- max(emptyCave$y)
  
  while(!filled && i < 10000){
    
    nextLoc <- addSand(currentCave, minX, maxX, maxY)
    filled <- is.infinite(nextLoc$x)
    
    if(!filled) currentCave <- bind_rows(currentCave, nextLoc)
    
    if(i %% 100 == 0) message(sprintf("Next Grain: %i", i)) 
    i <- i + 1
    
  }
  
  currentCave
  
}

# returns cave where no more sand will fall
fillFloorWSand <- function(emptyCave){
  
  filled <- FALSE
  i <- 1
  
  maxY <- max(emptyCave$y) + 2
  minX <- min(emptyCave$x) - maxY
  maxX <- max(emptyCave$x) + maxY
  
  currentCave <- bind_rows(
    emptyCave,
    expandPoints(minX, maxY, maxX, maxY) |> 
      mutate(type = "ledge_0")
  )
  
  while(!filled && i < 1e5){
    
    nextLoc <- addSand(currentCave, minX, maxX, maxY)
    filled <- nextLoc$x == 500 && nextLoc$y == 0
    
    currentCave <- bind_rows(currentCave, nextLoc)
    
    i <- i + 1
    if(i %% 100 == 0) message(sprintf("Next Grain: %i", i))
    
  }
  
  currentCave
  
}

sampleData <- "../data/day14_sample.txt" |> 
  readRockData()

sampleCave <- sampleData |> 
  map(expandLedge) |> 
  bind_rows() 

sampleFilled_part1 <- sampleCave |> 
  fillWSand() 

sample_part1 <- sampleFilled_part1  |> 
  filter(type == "sand") |> 
  NROW() |> 
  print()


sampleFilled_part2 <- sampleCave |> 
  fillFloorWSand()

sample_part2 <- sampleFilled_part2  |> 
  filter(type == "sand") |> 
  NROW() |> 
  print()

# sampleFilled_part2 |>
#   visualizeCave()

# puzzle
puzzleData <- "../data/day14.txt" |>
  readRockData()

puzzleCave <- puzzleData |> 
  map(expandLedge) |> 
  bind_rows() 

part1_filled <- puzzleCave |> 
  fillWSand() 

part1 <- part1_filled |> 
  filter(type == "sand") |> 
  NROW() |> 
  print()

part2_filled <- puzzleCave |> 
  fillFloorWSand()

part2 <- part2_filled  |> 
  filter(type == "sand") |> 
  NROW() |> 
  print()

# visualizeCave(part2_filled)
