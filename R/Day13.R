library(jsonlite)
library(purrr)
library(magrittr)
library(stringr)

dividers <- list(
  fromJSON("[[2]]", simplifyVector = FALSE), 
  fromJSON("[[6]]", simplifyVector = FALSE)
)

readPackets <- function(file){
  
  raw <- readLines(file) |> 
    str_subset(".+") |> 
    map(fromJSON, simplifyVector = FALSE)
   
  seq(from = 2, to = length(raw), by = 2) |> 
    map(
      ~list(p1 = raw[[.x-1]], p2 = raw[[.x]]) |> 
        set_class("packets")
    )
  
}

# depth is used for debugging nested calls
compareParts <- function(p1, p2, depth = 1){
  
  verdict <- NA
  i <- 1
  
  shortLen <- min(length(p1), length(p2))
  maxLen <- max(length(p1), length(p2))
  
  if(length(p1) == 0 || length(p2) == 0) verdict <- length(p2) > 0
  
  while(is.na(verdict) && i <= maxLen){
    
    
    if(i > shortLen) {
      verdict <- i > length(p1)
      next
    }
    
    left <- p1[[i]]
    right <- p2[[i]]
    
    if(length(left) == 0 || length(right) == 0){
      
      if(length(left) > 0) verdict <- FALSE
      if(length(right) > 0) verdict <- TRUE
      
    } else if(length(left) > 1 || length(right) > 1){
      
      verdict <- compareParts(left, right, depth + 1)
      
    } else if(is.integer(left) && is.integer(right)){
      
      if(left < right){
        verdict <- TRUE
      } else if(left > right){
        verdict <- FALSE
      } 
      
    } else if(is.list(left) && is.list(right)){
      
      verdict <- compareParts(
        unlist(left, recursive = FALSE), 
        unlist(right, recursive = FALSE), 
        depth + 1
      )
      
    } else if(is.integer(left) && is.list(right)) {
      
      verdict <- compareParts(list(left), right, depth + 1)
      
    } else if(is.list(left) && is.integer(right)){
      
      verdict <- compareParts(left, list(right),  depth + 1)  
      
    } else if(xor(is.na(left), is.na(right))){
      
      verdict <- is.na(left)
      
    }
    
    i <- i + 1
    
  }
  
  if(exists("VERBOSE") && VERBOSE && depth == 1){
    message(glue::glue("{toJSON(p1)}: {verdict}"))
  }
  
  verdict
  
}

"==.packets" <- function(x, y){
  identical(unlist(x[[1]]), unlist(y[[1]]))
}

">.packets" <- function(x, y){
  compareParts(y[[1]], x[[1]])
}

"[.packets" <- function(x, i){

  class(x) <- "list"
  structure(x[i], class="packets")
  
}

sampleData <- readPackets("../data/day13_sample.txt")
class(sampleData) <- "packets"

sample_part1 <- sampleData |> 
  map_lgl(~compareParts(.x$p1, .x$p2)) |> 
  which() |> 
  sum() |> 
  print()

sampleSorted <- sampleData |> 
  unlist(recursive = FALSE) |> 
  append(dividers) |> 
  set_class("packets") |> 
  sort() |> 
  map_chr(~toJSON(.x, auto_unbox = TRUE))

# sampleSorted |> walk(~message())

sample_part2 <- sampleSorted %in% map_chr(dividers, toJSON, auto_unbox = TRUE) |>
  which() |> 
  prod() |> 
  print()

# puzzle
puzzleData <- readPackets("../data/day13.txt")

part1 <- puzzleData |>
  map_lgl(~compareParts(.x$p1, .x$p2)) |>
  which() |>
  sum() |>
  print()

puzzleSorted <- puzzleData |> 
  unlist(recursive = FALSE) |> 
  append(dividers) |> 
  set_class("packets") |> 
  sort() |> 
  map_chr(~toJSON(.x, auto_unbox = TRUE))

part2 <- puzzleSorted %in% map_chr(dividers, toJSON, auto_unbox = TRUE) |>
  which() |> 
  prod() |> 
  print()

