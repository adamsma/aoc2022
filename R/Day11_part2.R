library(stringr)
library(magrittr)
library(purrr)
library(rlang)

NROUNDS <- 10000

readProfiles <- function(file) {
  
  file |> 
    readLines() |> 
    str_c(collapse = "\n") |> 
    strsplit(split = "\n\n") |> 
    pluck(1)
  
}

extractStartItems <- function(profile){
  
  profile |>
    str_split("\n") |> 
    pluck(1, 2) |> 
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

extractMod <- function(line){
  
  line |> 
    str_extract("[:digit:]+$") |> 
    as.integer()
  
}

extractTargets <- function(ifTrue, ifFalse){
  
  toTrue <- str_extract(ifTrue, "[:digit:]+$") |> as.integer()
  toFalse <- str_extract(ifFalse, "[:digit:]+$") |> as.integer()
  
  function(worry){
    ifelse(worry == 0, toTrue, toFalse)
  }
  
}

newMonkey <- function(profile){
  
  data <- str_split(profile, "\n") |> pluck(1)
  
  structure(
    list(
      inventory = extractStartItems(profile),
      op = extractOp(data[3]),
      modulus = extractMod(data[4]),
      throwTo = extractTargets(data[5], data[6]),
      processedCt = 0
    ), 
    class = "monkey"
  )
  
}

# returns updated Monkeys
processInventory <- function(monkeys, nTurn){
  
  activeMonkey <- monkeys[[nTurn]]
  
  # update worry levels based on active monkey ops
  # use modular arithmetic to manage exponential growing worry
  for(item in activeMonkey$inventory){
    for(i in seq_along(monkeys)){
      
      newLvl <- activeMonkey$op(monkeys[[i]]$worryLevels[item])
      
      monkeys[[i]]$worryLevels[item] <- newLvl %% monkeys[[i]]$modulus
      
    }
  }
  
  # monkeys are 0-based index
  activeWorries <- monkeys[[nTurn]]$worryLevels[activeMonkey$inventory]
  toGet <- activeMonkey$throwTo(activeWorries) + 1
  
  for(i in seq_along(toGet)){
    
    item <- activeMonkey$inventory[i]
    
    monkeys[[toGet[i]]]$inventory <- c(monkeys[[toGet[i]]]$inventory, item)
    
  } 
  
  # clear inventory of monkey whose turn it is and updated items processed
  monkeys[[nTurn]]$inventory <- integer(0)
  monkeys[[nTurn]]$processedCt <- monkeys[[nTurn]]$processedCt + length(toGet)
  
  monkeys
  
}

simRounds <- function(monkeys, n){
  
  rounds <- list(monkeys)
  
  for(i in seq(n)){
    
    for(turn in seq_along(monkeys)) monkeys <- processInventory(monkeys, turn)
    
    rounds[[i+1]] <- monkeys
    
    if(i %% 100 == 0) message(glue::glue("Round {i} Complete"))
    
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
sampleData <- "../data/day11_sample.txt" |> 
  readProfiles()

sampleItems <- sampleData |> 
  map(extractStartItems)

sampleMonkeys <- sampleData |> 
  map(newMonkey)

# convert inventories to use indexes instead of scores
# add worryLevels
currIndex <- 0
for(i in seq_along(sampleMonkeys)){
  
  indices <- seq_along(sampleMonkeys[[i]]$inventory) + currIndex
  currIndex <- currIndex + length(indices)
  
  sampleMonkeys[[i]]$inventory <- indices
  sampleMonkeys[[i]]$worryLevels <- unlist(sampleItems)
  
}

sampleRounds <- simRounds(sampleMonkeys, NROUNDS) |> 
  set_names(
    paste0("Round ", seq(from = 0, to = NROUNDS))
  )

sampleItemCts <- sampleRounds |>  
  map(
    ~map(.x, "processedCt") |> 
      set_names(paste0("Monkey ", seq_along(sampleMonkeys) - 1)) 
  ) 

str(sampleItemCts[c(1, 20, seq(1000, 10000, by = 1000) )+1])

sampleRounds |>
  tail(1) |> 
  pluck(1) |> 
  calcMonkeyBusiness() |> 
  print()

# part 2
data <- "../data/day11.txt" |> 
  readProfiles()

part2Items <- data |> 
  map(extractStartItems)

part2Monkeys <- data |> 
  map(newMonkey)

# convert inventories to use indexes instead of scores
# add worryLevels
currIndex <- 0
for(i in seq_along(part2Monkeys)){
  
  indices <- seq_along(part2Monkeys[[i]]$inventory) + currIndex
  currIndex <- currIndex + length(indices)
  
  part2Monkeys[[i]]$inventory <- indices
  part2Monkeys[[i]]$worryLevels <- unlist(part2Items)
  
}

part2Rounds <- simRounds(part2Monkeys, NROUNDS) |> 
  set_names(
    paste0("Round ", seq(from = 0, to = NROUNDS))
  )

part2ItemCts <- part2Rounds |>  
  map(
    ~map(.x, "processedCt") |> 
      set_names(paste0("Monkey ", seq_along(part2Monkeys) - 1)) 
  ) 


part2Rounds |>
  tail(1) |> 
  pluck(1) |> 
  calcMonkeyBusiness() |> 
  print()
