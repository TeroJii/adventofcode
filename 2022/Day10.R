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

# a function for drawing a line of pixels
draw_pixels <- function(cycles = 1:240, df = program_df){
  #draw pixels to this vector
  pixels <- rep("", length(cycles))

  for(i in seq_along(cycles)){
    #for the first cycle x value = 1, so the first pixel is #
    if(i == 1){
      pixels[i] <- "#"
    }

    # get x value during each cycle
    if(i>1){
      x_val <- df[df$cycle_after_completed<=i,]$x %>% tail(1)
    }

    #for the first line of pixels excluding the first pixel
    if(i >1 & i<=40){
      # note: we are counting pixel rows from 1 to 40 here, not 0 to 39
      draw_hash <- between(i, left = x_val, right = x_val + 2)
      if(draw_hash){
        pixels[i] <- "#"
      } else {
        pixels[i] <- "."
      }
    }

    #for the second line of pixels
    if(i > 40 & i<=80){
      # note: we are counting pixel rows from 1 to 40 here, not 0 to 39
      draw_hash <- between(i-40, left = x_val, right = x_val + 2)
      if(draw_hash){
        pixels[i] <- "#"
      } else {
        pixels[i] <- "."
      }
    }

    #for the third line of pixels
    if(i > 80 & i<=120){
      # note: we are counting pixel rows from 1 to 40 here, not 0 to 39
      draw_hash <- between(i-80, left = x_val, right = x_val + 2)
      if(draw_hash){
        pixels[i] <- "#"
      } else {
        pixels[i] <- "."
      }
    }

    #for the fourth line of pixels
    if(i > 120 & i<=160){
      # note: we are counting pixel rows from 1 to 40 here, not 0 to 39
      draw_hash <- between(i-120, left = x_val, right = x_val + 2)
      if(draw_hash){
        pixels[i] <- "#"
      } else {
        pixels[i] <- "."
      }
    }

    #for the fifth line of pixels
    if(i > 160 & i<=200){
      # note: we are counting pixel rows from 1 to 40 here, not 0 to 39
      draw_hash <- between(i-160, left = x_val, right = x_val + 2)
      if(draw_hash){
        pixels[i] <- "#"
      } else {
        pixels[i] <- "."
      }
    }

    #for the sixth line of pixels
    if(i > 200 & i<=240){
      # note: we are counting pixel rows from 1 to 40 here, not 0 to 39
      draw_hash <- between(i-200, left = x_val, right = x_val + 2)
      if(draw_hash){
        pixels[i] <- "#"
      } else {
        pixels[i] <- "."
      }
    }
  } #end for loop

  return(pixels)
} # end draw_pixels

# draw pixels from the program
pixels <- draw_pixels()

#draw the image
matrix(pixels, nrow = 240/40)
