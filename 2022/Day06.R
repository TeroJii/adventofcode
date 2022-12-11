#### puzzle part 1 ------------------------------

## To fix the communication system, you need to add a subroutine to the device that detects a start-of-packet marker in the datastream. In the protocol being used by the Elves, the start of a packet is indicated by a sequence of four characters that are all different.
## How many characters need to be processed before the first start-of-packet marker is detected?

library(tidyverse)

## Load data ---
datastream <- readr::read_lines(file = "2022/Input06.txt")


find_start_of_packet <- function(message){
  message_to_letters <- str_split(message, "") %>%
    unlist()

  for(i in seq_along(message_to_letters)) {
    #Check when the four letters are different
    first_pair <- message_to_letters[i] != message_to_letters[i+1]
    second_pair <- message_to_letters[i] != message_to_letters[i+2]
    third_pair <- message_to_letters[i] != message_to_letters[i+3]
    fourth_pair <- message_to_letters[i+1] != message_to_letters[i+2]
    fifth_pair <- message_to_letters[i+1] != message_to_letters[i+3]
    sixth_pair <- message_to_letters[i+2] != message_to_letters[i+3]

    # If all letters are different
    if(first_pair&second_pair&third_pair&fourth_pair&fifth_pair&sixth_pair){
      position_of_start_of_packet <- i+3
      break
    }
  }

  return(position_of_start_of_packet)
}

find_start_of_packet(datastream)

#### --- Part Two ------------
## A start-of-message marker is just like a start-of-packet marker, except it consists of 14 distinct characters rather than 4.
## How many characters need to be processed before the first start-of-message marker is detected?

find_start_of_message <- function(message){
  message_to_letters <- str_split(message, "") %>%
    unlist()

  for(i in seq_along(message_to_letters)) {
    #Check when the fourteen letters are different
    batch_start <- i
    batch_end <- i+13

    batch <- message_to_letters[batch_start:batch_end]

    # compare all letter combinations of two identical 14-letter vectors
    how_many_identicals <- expand.grid("let1" = batch, "let2" = batch) %>%
      # check if letters are the same
      mutate(same = (let1 == let2)) %>%
      # if the sum is larger than 14 the vector contains duplicates
      pull(same) %>% sum()

    # If all letters are different
    if(how_many_identicals == 14){
      position_of_message <- batch_end
      break
    }
  }

  return(position_of_message)
}


find_start_of_message(datastream)
