#### puzzle part 1 ------------------------------

## Find the signal strength during the 20th, 60th, 100th, 140th, 180th, and 220th cycles.
## What is the sum of these six signal strengths?

library(tidyverse)

## Load data ---
program_df <- readr::read_delim(file = "2022/Input10.txt", delim = " ", col_names = FALSE) %>%
  rename(instruction = X1, amount = X2) %>%
  mutate(instruction = gsub(pattern = "\r", replacement = "", x = instruction)) %>%
  mutate(amount = if_else(is.na(amount), true = 0, false = amount))

#calculate register x values
program_df <- program_df %>%
  mutate(x = 1 + cumsum(amount)) %>%
  mutate(cycles_to_add = if_else(instruction == "addx", true = 2, false = 1)) %>%
  mutate(cycle_after_completed = cumsum(cycles_to_add))

relevant_cycles <- c(20, 60, 100, 140, 180, 220)

## Find if any commands end at relevant cycles
rel_program_df <- program_df %>%
  mutate(relevant_cycle_during = ((cycle_after_completed+1)  %in% relevant_cycles | (cycle_after_completed+2) %in% relevant_cycles)) %>%
  filter(relevant_cycle_during == T)

rel_program_df$cycle_during <- relevant_cycles

# calculate signal strengths
rel_program_df %>%
  mutate(signal_strength = cycle_during * x) %>%
  pull(signal_strength) %>%
  sum()

#### --- Part Two ---

emptry_image <- matrix(rep(".",240), nrow = 240/40)
