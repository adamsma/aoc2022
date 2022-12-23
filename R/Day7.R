library(dplyr)
library(purrr)
library(tidyr)
library(igraph)
library(stringr)

readCommands <- function(file){
  
  file |> 
    readLines()
  
}

getDirList <- function(cmdLst){
  
  str_subset(cmdLst, "cd [^.]") |> 
    str_extract("(?<=[:space:])[[:alpha:]/]+$")    
  
}

emptyDir <- function() {
  list(
    size = numeric(0), 
    dirs = list(), 
    files = character(0)
  )
}

extractContents <- function(cmds){
  
  currDir <- ""
  dirsNames <- getDirList(cmds)
  contents <- map(
    dirsNames, 
    ~list(size = numeric(0), dirs = character(0), files = character(0))
  ) |> 
    set_names(dirsNames) 
 
  for(line in cmds){
    
    # changing into a new, named dir
    if(str_sub(line, 1, 1) == "$"){
      # user command
      
      if(str_detect(line, "\\.\\.$")){
        # move up one level, ignore
        next
      } else if(str_detect(line, "ls$")){
        # next line will be data, ignore this one
        next
      } else if(str_detect(line, "(?<=[:space:])[[:alpha:]/]+$")) {
        # capture move into named dir
        currDir <- str_extract(line, "(?<=[:space:])[[:alpha:]/]+$") 
      } else {
        # unknown situation
        stop(glue::glue("Unknown command: {line}"))
      }
      
      
    } else {
      # data line
      
      if(str_detect(line, "^dir")){
        
        # add to dir list for current directory
        newDir <- str_extract(line, "(?<=[:space:])[[:alpha:]/]+$")
        contents[[currDir]]$dirs <- c(contents[[currDir]]$dirs, newDir)
        
      } else if(str_detect(line, "^[[:digit:]]+")){
        
        # add to file list
        contents[[currDir]]$files <- c(contents[[currDir]]$files, line)
        
      } else {
        # unknown situation
        stop(glue::glue("Unknown data: {line}"))
      }
      
    }
    
  }
  
  contents
  
}

buildFileSys <- function(cmds){
  
  currPath <- "/"
  contents <- list(`/` = emptyDir())
  
  # skip first command as already processed with contents initialization
  for(line in tail(cmds, -1)){
    
    # changing into a new, named dir
    if(str_sub(line, 1, 1) == "$"){
      # user command
      
      if(str_detect(line, "\\.\\.$")){
        # move up one level, pop dir name and "dir"
        currPath <- head(currPath, -2)
        
      } else if(str_detect(line, "ls$")){
        # next line will be data, ignore this one
        next
        
      } else if(str_detect(line, "(?<=[:space:])[[:alpha:]/]+$")) {
        # capture move into named dir
        currDir <- str_extract(line, "(?<=[:space:])[[:alpha:]/]+$")
        # add string constant to be able to index into data structure
        currPath <- c(currPath, "dirs", currDir)
        
      } else {
        # unknown situation
        stop(glue::glue("Unknown command: {line}"))
      }
      
      
    } else {
      # data line
      
      if(str_detect(line, "^dir")){
        
        # add to dir list for current directory
        oldDirs <- pluck(contents, !!!currPath, "dirs")
        newEmptyDir <- list(emptyDir()) |> 
          set_names(str_extract(line, "(?<=[:space:])[[:alpha:]/]+$"))
        
        pluck(contents, !!!currPath, "dirs") <- c(oldDirs, newEmptyDir)
        
      } else if(str_detect(line, "^[[:digit:]]+")){
        
        # add to file list
        pluck(contents, !!!currPath, "files") <- c(
          pluck(contents, !!!currPath, "files"), 
          line
        )
        
      } else {
        # unknown situation
        stop(glue::glue("Unknown data: {line}"))
      }
      
    }
    
  }
  
  contents
  
}

extractFileSize <- function(line){
  
  str_extract(line, "^[[:digit:]]+(?=[:space:])") |> 
    as.numeric()
  
}

calcDirSizes <- function(contents){
  
  remainDirs <- names(contents)
  
  # fail safe
  i <- 0
  limit <- 10e3
  
  leafDirs <- remainDirs[map_lgl(contents, ~length(.x$dirs) == 0)]
  for(dir in leafDirs){
    contents[[dir]]$size <- contents[[dir]]$files |> 
      extractFileSize() |> 
      sum()
  }
  
  remainDirs <- setdiff(remainDirs, leafDirs)
  
  while(length(remainDirs) > 0 && i < limit){
    
    for(dir in remainDirs){
      
      canCalc <- !any(contents[[dir]]$dirs %in% remainDirs)
      if(canCalc){
        
        filesSize <- contents[[dir]]$files |> 
          extractFileSize() |> 
          sum()
        
        dirsSize <- contents[[dir]]$dirs |> 
          map_dbl(~contents[[.x]]$size) |> 
          sum()
        
        contents[[dir]]$size <- filesSize + dirsSize
        
        remainDirs <- setdiff(remainDirs, dir)  
      }
      
    }
    i <- i + 1
  }
  
  if(i == limit){
    stop("Loop limit reached while calculating dir size")
  }
  
  contents
  
}

recurseDirSizes <- function(dir){
  
  fileTotal <- dir$files |> 
    extractFileSize() |> 
    sum()
  
  if(!is_empty(dir$dirs)){
    # recurse
    
    for(subDir in names(dir$dirs)){
      pluck(dir, "dirs", subDir) <- pluck(dir, "dirs", subDir) |> 
        recurseDirSizes()
    }
    
    dirsTotal <- dir$dirs |> 
      map_dbl("size") |> 
      sum()
    
    dir$size <- fileTotal + dirsTotal
    
  } else {
    # calc
    dir$size <- fileTotal
  }
  
  dir
  
}

dropLargeDirs <- function(contents){
  
  contents |> 
    keep(~.x$size <= 100000)
  
}

collectSmallSizes <- function(dir, limit = 100000){
  
  smallDirs <- numeric(0)
  
  if(!is_empty(dir$dirs)){
    
    for(subDir in names(dir$dirs)){
      smallDirs <- c(
        smallDirs, 
        pluck(dir, "dirs", subDir) |> 
          collectSmallSizes(limit = limit) 
      )
    }
    
  }
  
  if(dir$size <= limit) smallDirs <- c(smallDirs, dir$size)
  
  smallDirs
  
}

collectOptions <- function(dir, threshold){
  
  optsDirs <- numeric(0)
  
  if(!is_empty(dir$dirs)){
    
    for(subDir in names(dir$dirs)){
      optsDirs <- c(
        optsDirs, 
        pluck(dir, "dirs", subDir) |> 
          collectOptions(threshold = threshold) 
      )
    }
    
  }
  
  if(dir$size >= threshold) optsDirs <- c(optsDirs, dir$size)
  
  optsDirs
  
}

# # works but doesn't account for directories with the same name along different paths
# sampleContents <- readCommands("../data/day7_sample.txt") |>
#   extractContents() |>
#   calcDirSizes()

# smpl_part1 <- sampleContents |>
#   dropLargeDirs() |>
#   map_dbl("size") |>
#   sum() |>
#   print()

sampleFS <- readCommands("../data/day7_sample.txt") |> 
  buildFileSys() |> 
  map(recurseDirSizes) 

smpl_part1 <- sampleFS |> 
  map(collectSmallSizes) |> 
  unlist() |> 
  sum() |> 
  print()

smplMinSize <- sampleFS[[1]]$size - (70000000 - 30000000)

sample_part2 <- sampleFS |> 
  map(collectOptions, threshold = smplMinSize) |> 
  unlist() |> 
  min() |> 
  print()

# puzzle part 1
contents <- readCommands("../data/day7.txt") |> 
  buildFileSys() |> 
  map(recurseDirSizes)

part1 <- contents |> 
  map(collectSmallSizes) |> 
  unlist() |> 
  sum() |> 
  print()

# puzzle part 2

minSizeNeeded <- contents[[1]]$size - (70000000 - 30000000)
part2 <- contents |> 
  map(collectOptions, threshold = minSizeNeeded ) |> 
  unlist() |>
  min() |>
  print()
