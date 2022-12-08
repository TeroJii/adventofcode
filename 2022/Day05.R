#### puzzle part 1 ------------------------------

## After the rearrangement procedure completes, what crate ends up on top of each stack?

library(tidyverse)

## Load data ---
crates <- readr::read_delim(file = "2022/Input05.txt", delim = ",", col_names = FALSE, skip_empty_rows = FALSE, n_max = 9)

moves <- readr::read_delim(file = "2022/Input05.txt", delim = ",", col_names = FALSE, skip = 9) %>%
  rename(move = X1)


## We need to organize the crate chart into something that is easier to manipulate
# Let's first extract the stack position string

#Line 9 of the crate chart
stacks_positions <- crates[9,1] %>% as.character()

# there are nine stacks
nums <- 1:9 %>% as.character()

# get the positions of the stacks
pos <- NULL
for(i in seq_along(nums)){

  pos_temp <- str_locate(string = stacks_positions, pattern = nums[i])[1]
  pos <- c(pos, pos_temp)

}


#get the crates to the stacks
stacks <- data.frame(name = rep(NA, 9), num = 1:9, pile = rep(NA,9))
pile <- ""

for(i in seq_along(1:9)){

  stacks$name[i] <- paste0("stack", i)

  # get the crates from bottom to top
  for (j in 8:1) {

    #get grate
    if(str_length(crates[j,1])>=pos[i]){
      crate <-  str_sub(crates[j,1], start = pos[i], end = pos[i])
    } else {crate <- ""}

    #add crate to pile
    pile <- str_glue(pile, crate)
  }

  #record pile
  stacks$pile[i] <- pile

  # Set NULL for next round
  pile <- ""
}

## The order of crates is now stored in stacks data.frame as a string from bottom to top
stacks <- stacks %>%
  # lets remove white spaces
  mutate(pile = str_trim(pile))

## Stacks are now in order
## next we need to move the crates around

head(moves)

# a vector of how many crates to move
how_many_crates <- moves$move %>%
  # find the word "move" + 1-3 digits
  str_extract(pattern = "move \\d{1,3}") %>%
  #just get the digits
  str_extract(pattern = "\\d{1,3}") %>% as.numeric()

# a vector of starting piles
from_where <- moves$move %>%
  # find the word "move" + 1-3 digits
  str_extract(pattern = "from \\d{1}") %>%
  #just get the digits
  str_extract(pattern = "\\d{1}") %>% as.numeric()

# a vector of destinations piles
to_where <- moves$move %>%
  # find the word "move" + 1-3 digits
  str_extract(pattern = "to \\d{1}") %>%
  #just get the digits
  str_extract(pattern = "\\d{1}") %>% as.numeric()

# create a function for moving crates
move_crates <- function(df = stacks, how_many, from, to, reverse = TRUE){

  # record starting positions
  start_pile <- df$pile[from]
  end_pile <- df$pile[to]

  #start pile length
  start_length <- str_length(start_pile)

  #take move these amount of crates from the end of start pile
  move_these <- str_sub(string = start_pile, start = (start_length - (how_many-1)), end = start_length)

  new_start_pile <- str_sub(string = start_pile, start = 1, end = (start_length - how_many))

  ## update start pile crate situation
  df$pile[from] <- new_start_pile

  if(reverse){
    ## mirror the crates to be moved
    move_these <- stringi::stri_reverse(move_these)
  }

  #update the end pile
  new_end_pile <- str_glue(end_pile, move_these)

  df$pile[to] <- new_end_pile

  return(df)
}


# move crates around
stacks_after_moving <- stacks

for(i in seq_along(how_many_crates)){
  stacks_after_moving <- move_crates(df = stacks_after_moving,
                                     how_many = how_many_crates[i],
                                     from = from_where[i],
                                     to = to_where[i])
}

# Now we need to record the final letter from each string

stacks_after_moving %>%
  mutate(last_letter = str_sub(string = pile, start = str_length(pile), end = str_length(pile))) %>%
  pull(last_letter) %>% paste0(collapse = "")

### Puzzle part 2 -----------------------------

# The crates wont get mirrored in part two of the puzzle
#After the rearrangement procedure completes, what crate ends up on top of each stack?


# move crates around
stacks_after_moving <- stacks

for(i in seq_along(how_many_crates)){
  stacks_after_moving <- move_crates(df = stacks_after_moving,
                                     how_many = how_many_crates[i],
                                     from = from_where[i],
                                     to = to_where[i],
                                     reverse = FALSE)
}

# Now we need to record the final letter from each string

stacks_after_moving %>%
  mutate(last_letter = str_sub(string = pile, start = str_length(pile), end = str_length(pile))) %>%
  pull(last_letter) %>% paste0(collapse = "")
