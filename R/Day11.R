library(stringr)
library(purrr)
library(rlang)

readMonkeyProfiles <- function(file){
  
  file |> 
    readLines() |> 
    str_c(collapse = "\n") |> 
    strsplit(split = "\n\n") |> 
    pluck(1)
  
}

extractStartItems <- function(line){
  
  line |> 
    str_extract("(?<=:).+$") |> 
    str_split(",") |> 
    unlist() |> 
    str_trim() |> 
    as.integer()
  
}

extractOp <- function(line){
  
  formula <- line |> 
    str_extract("(?<=(new = )).+$") |> 
    parse_expr()
  
  function(old){
    eval_bare(formula)
  }
  
}

extractTest <- function(test, ifTrue, ifFalse){
  
  divisor <- str_extract(test, "[:digit:]+$") |> as.integer()
  toTrue <- str_extract(ifTrue, "[:digit:]+$") |> as.integer()
  toFalse <- str_extract(ifFalse, "[:digit:]+$") |> as.integer()
  
  function(worry){
    ifelse((worry %% divisor) == 0, toTrue, toFalse)
  }
  
}

newMonkey <- function(profile){
  
  data <- str_split(profile, "\n", n = 6) |> pluck(1)
  
  structure(
    list2(
      inventory = extractStartItems(data[2]),
      op = extractOp(data[3]),
      throwTo = extractTest(data[4], data[5], data[6]),
      processedCt = 0
    ), 
    class = "monkey"
  )
  
}

# returns list of transferred inventory
processInventory <- function(monkey, nOthers, worryReduct = 3){
  
  sendlist <- map(seq(nOthers), ~integer(0))
  
  worryLevels <- monkey$inventory |> 
    map_dbl(~floor(monkey$op(.x)/worryReduct))
  
  # monkeys are 0-based index
  toGet <- monkey$throwTo(worryLevels) + 1
  
  for(i in seq_along(toGet)){
    
    item <- worryLevels[i]
    toMonkey <- toGet[i]
    
    sendlist[[toMonkey]] <- c(sendlist[[toMonkey]], item)
    
  } 
  
  sendlist
  
}

catchItems <- function(monkey, items){
  
  monkey$inventory <- c(monkey$inventory, items)
  
  monkey
  
}

simRound <- function(monkeys, worryReduct = 3){
  
  n <- length(monkeys)
  
  for(i in seq(n)){
    
    transferList <- processInventory(monkeys[[i]], n, worryReduct)
    itemCt <- transferList |> 
      unlist() |> 
      length()
    
    # clear inventory of monkey whose turn it is and updated items processed
    monkeys[[i]]$inventory <- integer(0)
    monkeys[[i]]$processedCt <- monkeys[[i]]$processedCt + itemCt
    
    # update inventories of monkeys
    monkeys <- map2(monkeys, transferList, catchItems)
    
  }
  
  monkeys
  
}

simNRounds <- function(monkeys, n, worryReduct = 3){
  
  rounds <- list(monkeys)
  
  for(i in seq(n)){
    
    rounds[[i+1]] <- simRound(rounds[[i]], worryReduct)
    
  }
  
  rounds
  
}

calcMonkeyBusiness <- function(monkeys){
  
  monkeys |> 
    map_dbl("processedCt") |>
    sort(decreasing = TRUE) |> 
    head(2) |> 
    prod()
  
}

# sample
sampleMonkeys <- "../data/day11_sample.txt" |> 
  readMonkeyProfiles() |> 
  map(newMonkey)

sampleRounds <- simNRounds(sampleMonkeys, 20) |> 
  set_names(
    paste0("Round ", seq(from = 0, to = 20))
  )

sampleInventories <- sampleRounds |>  
  map(
    ~map(.x, "inventory") |> 
      set_names(paste0("Monkey ", seq_along(sampleMonkeys) - 1)) 
  ) 

sampleRounds |>
  tail(1) |> 
  pluck(1) |> 
  calcMonkeyBusiness() |> 
  print()

# puzzle part1 
nRounds <- 20 

monkeysInit <- "../data/day11.txt" |> 
  readMonkeyProfiles() |> 
  map(newMonkey)

roundResults <- simNRounds(monkeysInit, nRounds) |> 
  set_names(
    paste0("Round ", seq(from = 0, to = nRounds))
  )

roundInventories <- roundResults |>  
  map(
    ~map(.x, "inventory") |> 
      set_names(paste0("Monkey ", seq_along(monkeysInit) - 1)) 
  ) 

part1 <- roundResults |> 
  tail(1) |> 
  pluck(1) |> 
  calcMonkeyBusiness() |> 
  print()
