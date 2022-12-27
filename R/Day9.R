library(tidyr)
library(dplyr)
library(purrr)

MOVE <- c(
  R = complex(real = 1, imaginary = 0),
  L = complex(real = -1, imaginary = 0),
  U = complex(real = 0, imaginary = 1),
  D = complex(real = 0, imaginary = -1)
)

readData <- function(file){
  
  tibble(raw = readLines(file)) |> 
    separate(raw, into = c("direction", "steps"), sep = " ", convert = TRUE) 
  
}

moveHead <- function(hPos, direction){
  
  hPos + MOVE[direction]
  
}

adjustTail <- function(hPos, tPos){
  
  reDist <- Re(hPos) - Re(tPos)
  imDist <- Im(hPos) - Im(tPos)
  
  # assume always previously updated correctly such that 
  # at most one move needed in each direction
  if(abs(reDist) > 1 || abs(imDist) > 1){
    # diagonal so need to move both
    tPos <- tPos + sign(reDist)
    tPos <- tPos + complex(imaginary = sign(imDist))
  } else if(abs(reDist) > 0 || abs(imDist) > 0){
    
    # move horizontally
    if(abs(reDist) > 1) tPos <- tPos + sign(reDist)
    
    # move vertically
    if(abs(imDist) > 1) tPos <- tPos + complex(imaginary = sign(imDist))
      
  }
  
  tPos  
  
}

processMoves <- function(moveList){
  
  hPos <- complex(real = 1, imaginary = 1)
  tPos <- hPos
  
  locHist <- tibble(head = hPos, tail = tPos)
  
  nMoves <- NROW(moveList)
  
  for(i in seq(nMoves)){
    
    move <- slice(moveList, i)
    
    for(step in seq(move$steps)){
      
      hPos <- moveHead(hPos, move$direction)
      tPos <- adjustTail(hPos, tPos)
      
      locHist <- bind_rows(locHist, tibble(head = hPos, tail = tPos))
    }
    
  }
  
  locHist
  
}

processMultiKnot <- function(moveList, knots = 10){
  
  nMoves <- NROW(moveList)
  totalSteps <- sum(moveList$steps)
  
  locHist <- map(
    seq(knots), 
    # preallocate
    ~c(complex(real = 1, imaginary = 1), complex(totalSteps))
  )
  
  stepCount <- 1
  for(i in seq(nMoves)){
    
    if((i %% 10) == 0) message(sprintf("Move %i", i))
    
    move <- slice(moveList, i)
    
    for(step in seq(move$steps)){
      stepCount <- stepCount + 1
      
      locHist[[1]][stepCount] <- moveHead(locHist[[1]][stepCount-1], move$direction)
      
      for(knot in seq(from = 2, to = knots)){
        locHist[[knot]][stepCount] <- adjustTail(
          locHist[[knot-1]][stepCount], locHist[[knot]][stepCount-1])
      }
    }
    
  }
  
  locHist
  
}

# sample
sample_data <- "../data/day9_sample.txt" |> 
  readData()  

sample_part1 <- sample_data|> 
  processMoves() |> 
  distinct(tail) |> 
  NROW() |> 
  print()

sample_part2 <- sample_data |> 
  processMultiKnot() |> 
  print()

# par(mfcol = c(1,2))
# plot(sample_moveMap$head, type = "b")
# plot(sample_moveMap$tail, type = "b", xlim = c(1L, 6L), ylim = c(1L, 5L))

# puzzle
part1_data <- "../data/day9.txt" |> 
  readData() 

part1 <- part1_data |> 
  processMoves()  |> 
  distinct(tail) |> 
  NROW() |> 
  print()

part2 <- part1_data |> 
  processMultiKnot() |> 
  print()
