library(ggplot2)
library(glue)
library(purrr)
library(dplyr)
library(stringr)

readSensorData <- function(file){
  
  file |> 
    readLines() |> 
    strsplit(split = ":") |> 
    map_dfr(~tibble(sens = .x[1], bec =.x[2])) |> 
    extractCoord()
  
}

extractCoord <- function(data){
  
  tibble(
    sensX = str_extract(data$sens, "(?<=x=)-?\\d+"),
    sensY = str_extract(data$sens, "(?<=y=)-?\\d+"),
    becX = str_extract(data$bec, "(?<=x=)-?\\d+"),
    becY = str_extract(data$bec, "(?<=y=)-?\\d+")
  ) |> 
    mutate(across(.fns = as.numeric)) |> 
    mutate(dist = abs(sensX - becX) + abs(sensY - becY))
  
}

excludePts <- function(sensX, sensY, dist, ...){
  
  # message(glue("Sensor at x={sensX}, y={sensY}"))
  
  map_dfr(
    seq(from = -dist, to = dist), 
    \(i) tibble(x = sensX + seq(-(dist - abs(i)), dist - abs(i)), y = sensY + i)
  ) 
  
}

excludePts2 <- function(sensX, sensY, dist, targetRow){
  
  # message(glue("Sensor at x={sensX}, y={sensY}"))
  
  hasRow <- targetRow %in% seq(sensY - dist, sensY + dist)
  
  if(hasRow){
    
    offset <- abs(sensY - targetRow)
    pts <- tibble(
      x = sensX + seq(-(dist - offset), dist - offset),
      y = targetRow
    )
    
  } else {
    pts <- tibble(x = numeric(0), y = numeric(0))
  }
  
}

excludePtsBound <- function(minX, maxX, sensY, minY, maxY, targetRow, limit, ...){
  
    offset <- abs(sensY - targetRow)
    pts <- tibble(
      y = targetRow,
      xMin = max(minX + offset, 0),
      xMax = min(maxX - offset, limit)
    )
  
  pts
  
}

# assumes data is sorted ascending by xMin
compactRanges <- function(data){
  
  y <- data$y
  xMin <- data$xMin
  xMax <- data$xMax
  
  newMins <- xMin[1]
  newMaxs <- xMax[1]
  
  for(i in seq_along(xMin)){
    
    if(xMin[i] <= tail(newMaxs, 1) + 1){
      newMaxs[length(newMaxs)] <- max(tail(newMaxs, 1), xMax[i])
    } else {
      newMins <- c(newMins, xMin[i])
      newMaxs <- c(newMaxs, xMax[i])
    }
    
  }
  
  tibble(y = y[1], xMin = newMins, xMax = newMaxs) 
  
}

#### sample ####
sampleData <- "../data/day15_sample.txt" |> 
  readSensorData() |> 
  mutate(
    minY = (sensY - dist),
    maxY = (sensY + dist),
    minX = (sensX - dist),
    maxX = (sensX + dist),
  )

#### sample part 1 ####
sample_exclLocs <- sampleData |> 
  pmap(excludePts) |> 
  bind_rows() |> 
  distinct()

sample_part1 <- sample_exclLocs |> 
  filter(y == 10) |> 
  NROW() |> 
  magrittr::subtract(
    filter(sampleData, becY == 10) |> 
      distinct(becX, becY) |> 
      NROW()
  )

print(glue("Sample Part 1: {sample_part1}"))

sample_exclLocsv2 <- sampleData |> 
  select(sensX, sensY, dist) |> 
  pmap(excludePts2, targetRow = 10) |> 
  bind_rows() |> 
  distinct()

sample_part1v2 <- sample_exclLocsv2 |> 
  filter(y == 10) |> 
  NROW() |> 
  magrittr::subtract(
    filter(sampleData, becY == 10) |> 
      distinct(becX, becY) |> 
      NROW()
  ) 
  
print(glue("Sample Part 1(v2): {sample_part1v2}"))

#### sample part 2 ####
sampleLim <- 20
sp2D <- sampleData |> arrange(minY, maxY)

for(iRow in seq(0, sampleLim)){
  
  if(iRow %% 5 == 0) message(glue("Target Row = {iRow}"))
  
  sample_part2 <- sp2D |> 
    filter(iRow >= minY & iRow <= maxY) |>
    pmap_dfr(
      excludePtsBound,
      targetRow = iRow, limit =  sampleLim
    ) |> 
    arrange(xMin) |>
    compactRanges()
  
  if(NROW(sample_part2) > 1) break
    
}


glue(
  "Sample Part 2: {sample_part2$y[1] + 4000000*(sample_part2$xMin[2]-1)}"
) |> 
  print()

# ggplot(sample_exclLocs, aes(x, y)) +
#   geom_point() +
#   theme_minimal()

#### puzzle ####
puzzleData <- "../data/day15.txt" |> 
  readSensorData() |> 
  mutate(
    minY = (sensY - dist),
    maxY = (sensY + dist),
    minX = (sensX - dist),
    maxX = (sensX + dist),
  )

#### part 1 ####
exclLocsLtd <- puzzleData |>
  select(sensX, sensY, dist) |> 
  pmap(excludePts2, targetRow = 2000000) |>
  bind_rows() |>
  distinct()

part1 <- exclLocsLtd |>
  NROW() |>
  magrittr::subtract(
    filter(puzzleData, becY == 2000000) |>
      distinct(becX, becY) |>
      NROW()
  ) 

print(glue("Part 1: {part1}"))

#### part 2 ####
searchLim <- 4000000
p2Data <- puzzleData |> arrange(minY, maxY)

for(iRow in seq(2638237, searchLim)){
  
  if(iRow %% 100 == 0) message(glue("Target Row = {iRow}"))
  
  part2 <- p2Data |> 
    filter(iRow >= minY & iRow <= maxY) |> 
    pmap_dfr(
      excludePtsBound,
        targetRow = iRow, limit = searchLim
      ) |> 
    arrange(xMin) |> 
    compactRanges()
  
  if(NROW(part2) > 1) break
  
}

glue(
  "Sample Part 2: {part2$y[1] + 4000000*(part2$xMin[2]-1)}"
) |> 
  print()
