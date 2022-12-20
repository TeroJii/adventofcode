#### puzzle part 1 ------------------------------

## Simulate your complete hypothetical series of motions. How many positions does the tail of the rope visit at least once?

library(tidyverse)

## Load data ---
motions_df <- readr::read_delim(file = "2022/Input09.txt", delim = " ", col_names = FALSE) %>%
  rename(direction = X1, amount = X2)

test_motions <- readr::read_delim(file = "2022/TestInput09.txt", delim = " ", col_names = FALSE) %>%
  rename(direction = X1, amount = X2)

track_head <- function(direction, amount){
  # record head positions
  # start from origo
  df <- data.frame("x" = 0, "y" = 0)

  # go trough directions
  for(i in seq_along(direction)){
    step_length <- amount[i]

    # record previous position
    last_x <- df$x[length(df$x)]
    last_y <- df$y[length(df$y)]

    if(direction[i] == "U"){
      add_frame <- data.frame("x" = rep(last_x+0, step_length),
                              "y" = last_y+1:step_length)

      # add new moves to data frame
      df <- rbind(df, add_frame)
    }

    if(direction[i] == "D"){
      add_frame <- data.frame("x" = rep(last_x+0, step_length),
                              "y" = last_y-1:step_length)

      # add new moves to data frame
      df <- rbind(df, add_frame)
    }

    if(direction[i] == "L"){
      add_frame <- data.frame("x" = last_x-1:step_length,
                              "y" = rep(last_y+0, step_length))

      # add new moves to data frame
      df <- rbind(df, add_frame)
    }

    if(direction[i] == "R"){
      add_frame <- data.frame("x" = last_x+1:step_length,
                              "y" = rep(last_y+0, step_length))

      # add new moves to data frame
      df <- rbind(df, add_frame)
    }
  } # end directions loop

  return(df)
} # end track_head

# Simulate head movements
moves_head <- track_head(direction = motions_df$direction, amount = motions_df$amount)

## Trailing tail
## Starting positions:
# For any point in time the Head is on top of next to the tail:
#
# HHH
# HTH
# HHH
#
# After the head moves, it can be in the following positions:
#
#  HHH
# HHHHH
# HHTHH
# HHHHH
#  HHH
#
# If the new position of the head is in any of the original 9 squares the tail does not move
# However if the distance between the new position of the head and the old position of the tail
# is at least 2, then the tail moves.
# Updates needed in relative position:
# new head: U2L (up two and left) -> new tail pos: UL
# U2 -> U
# U2R -> UR
# UR2 -> UR
# R2 <- R ... and so on

track_tail <- function(head_movement_df = moves_head){
  # data.frame with head and tail
  head_and_tail <- data.frame("x_h" = head_movement_df$x,
                              "y_h" = head_movement_df$y,
                              # tail positions to be determined
                              "x_t" = NA,
                              "y_t" = NA)

  # starting pos of tail
  head_and_tail$x_t[1] <- head_and_tail$y_t[1] <- 0

  # Let's run through the tail positions
  for(i in 2:length(head_and_tail$x_h)){
    # record tail position before the move
    old_tail_pos <- c(head_and_tail$x_t[i-1], head_and_tail$y_t[i-1])
    # record head position after the move
    new_head_pos <- c(head_and_tail$x_h[i], head_and_tail$y_h[i])

    #define relative position of head to old tail pos
    rel_pos <- new_head_pos - old_tail_pos

    # calculate distance between new head and old tail
    distance <- rel_pos^2 %>% sum() %>% sqrt()

    # if distance is less than two then the tail won't move
    if(distance < 2){
      new_tail_pos <- old_tail_pos
    } else {
      # define new position if needed
      ## up 2
      if(rel_pos[2] == 2){
        #left
        if(rel_pos[1] == -1){
          new_tail_pos <- c(old_tail_pos[1] - 1, old_tail_pos[2] + 1)
        }
        #top
        if(rel_pos[1] == 0){
          new_tail_pos <- c(old_tail_pos[1], old_tail_pos[2] + 1)
        }
        #right
        if(rel_pos[1] == 1){
          new_tail_pos <- c(old_tail_pos[1] + 1, old_tail_pos[2] + 1)
        }
      }

      ## up 1
      if(rel_pos[2] == 1){
        #left 2
        if(rel_pos[1] == -2){
          new_tail_pos <- c(old_tail_pos[1] - 1, old_tail_pos[2] + 1)
        }
        #right 2
        if(rel_pos[1] == 2){
          new_tail_pos <- c(old_tail_pos[1] + 1, old_tail_pos[2] + 1)
        }
      }

      ## center line
      if(rel_pos[2] == 0){
        #left 2
        if(rel_pos[1] == -2){
          new_tail_pos <- c(old_tail_pos[1] - 1, old_tail_pos[2])
        }
        #right 2
        if(rel_pos[1] == 2){
          new_tail_pos <- c(old_tail_pos[1] + 1, old_tail_pos[2])
        }
      }

      ## down 1
      if(rel_pos[2] == -1){
        #left 2
        if(rel_pos[1] == -2){
          new_tail_pos <- c(old_tail_pos[1] - 1, old_tail_pos[2] - 1)
        }
        #right 2
        if(rel_pos[1] == 2){
          new_tail_pos <- c(old_tail_pos[1] + 1, old_tail_pos[2] - 1)
        }
      }

      ## down 2
      if(rel_pos[2] == -2){
        #left
        if(rel_pos[1] == -1){
          new_tail_pos <- c(old_tail_pos[1] - 1, old_tail_pos[2] - 1)
        }
        # down
        if(rel_pos[1] == 0){
          new_tail_pos <- c(old_tail_pos[1], old_tail_pos[2] - 1)
        }
        #right
        if(rel_pos[1] == 1){
          new_tail_pos <- c(old_tail_pos[1] + 1, old_tail_pos[2] - 1)
        }
      }
    }


    #record new tail positions for the round
    head_and_tail$x_t[i] <- new_tail_pos[1]
    head_and_tail$y_t[i] <- new_tail_pos[2]
  }

return(head_and_tail)
} # end track_tail functions


## How does the tail move
head_and_tail <- track_tail(head_movement_df = moves_head)

# #test with the example data
# test_head <- track_head(direction = test_motions$direction, amount = test_motions$amount)
# test_ht <- track_tail(test_head)

# record unique positions of tail

head_and_tail %>%
  select(x_t, y_t) %>%
  unique.data.frame() %>%
  dim()


## Part 2 ------------------------

# Simulate your complete series of motions on a larger rope with ten knots.
# How many positions does the tail of the rope visit at least once?

# We need to recursively track the following knots
## However, the movement of the head differs slightly from the movement of subsequent rope parts,
## which can also move diagonally
##   start:   possible next pos:
##              11111
##    111       11111
##    121       11211
##    111       11111
##              11111

# The new positions in the corners need to be defined and movement of "2" in those cases defined as well

## If the rope ahead moves diagonally, then the trailing part will also move diagonally to the same direction



# A function for tracking the parts of the rope where the preceding part can move diagonally
# give the x-y-movements of the first rope part as input
# The function calculates trailing part movements
track_rope <- function(df){
  # data.frame with two consecutive rope parts
  front_and_back <- data.frame("x_h" = df$x,
                              "y_h" = df$y,
                              # trailing positions to be determined
                              "x_t" = NA,
                              "y_t" = NA)

  # starting pos of trailing rope part
  front_and_back$x_t[1] <- front_and_back$y_t[1] <- 0

  # Let's run through the tail positions
  for(i in 2:length(front_and_back$x_h)){
    # record tail position before the move
    old_tail_pos <- c(front_and_back$x_t[i-1], front_and_back$y_t[i-1])
    # record head position after the move
    new_head_pos <- c(front_and_back$x_h[i], front_and_back$y_h[i])

    #define relative position of head to old tail pos
    rel_pos <- new_head_pos - old_tail_pos

    # calculate distance between new head and old tail
    distance <- rel_pos^2 %>% sum() %>% sqrt()

    # if distance is less than two then the tail won't move
    if(distance < 2){
      new_tail_pos <- old_tail_pos
    } else {
      # define new position if needed
      ## up 2
      if(rel_pos[2] == 2){
        #left
        if(rel_pos[1] == -1|rel_pos[1] == -2){
          new_tail_pos <- c(old_tail_pos[1] - 1, old_tail_pos[2] + 1)
        }
        #top
        if(rel_pos[1] == 0){
          new_tail_pos <- c(old_tail_pos[1], old_tail_pos[2] + 1)
        }
        #right
        if(rel_pos[1] == 1|rel_pos[1] == 2){
          new_tail_pos <- c(old_tail_pos[1] + 1, old_tail_pos[2] + 1)
        }
      }

      ## up 1
      if(rel_pos[2] == 1){
        #left 2
        if(rel_pos[1] == -2){
          new_tail_pos <- c(old_tail_pos[1] - 1, old_tail_pos[2] + 1)
        }
        #right 2
        if(rel_pos[1] == 2){
          new_tail_pos <- c(old_tail_pos[1] + 1, old_tail_pos[2] + 1)
        }
      }

      ## center line
      if(rel_pos[2] == 0){
        #left 2
        if(rel_pos[1] == -2){
          new_tail_pos <- c(old_tail_pos[1] - 1, old_tail_pos[2])
        }
        #right 2
        if(rel_pos[1] == 2){
          new_tail_pos <- c(old_tail_pos[1] + 1, old_tail_pos[2])
        }
      }

      ## down 1
      if(rel_pos[2] == -1){
        #left 2
        if(rel_pos[1] == -2){
          new_tail_pos <- c(old_tail_pos[1] - 1, old_tail_pos[2] - 1)
        }
        #right 2
        if(rel_pos[1] == 2){
          new_tail_pos <- c(old_tail_pos[1] + 1, old_tail_pos[2] - 1)
        }
      }

      ## down 2
      if(rel_pos[2] == -2){
        #left -1 or -2
        if(rel_pos[1] == -1|rel_pos[1] == -2){
          new_tail_pos <- c(old_tail_pos[1] - 1, old_tail_pos[2] - 1)
        }
        # down
        if(rel_pos[1] == 0){
          new_tail_pos <- c(old_tail_pos[1], old_tail_pos[2] - 1)
        }
        #right 1 or 2
        if(rel_pos[1] == 1|rel_pos[1] == 2){
          new_tail_pos <- c(old_tail_pos[1] + 1, old_tail_pos[2] - 1)
        }
      }
    }


    #record new tail positions for the round
    front_and_back$x_t[i] <- new_tail_pos[1]
    front_and_back$y_t[i] <- new_tail_pos[2]
  }

  return(front_and_back)
} # end track_rope



# we feed the tail as the new head for as many knots as needed
recursive_tail <- function(path, knot, max_knots = 10){
  if(knot == max_knots){
    return(track_rope(df = path))
  } else {
    df <- track_rope(path)
    df <- df %>% select(3:4)
    # feed new tail as the next head
   return(recursive_tail(path = df, knot = knot+1))
  }
} # end recursive tail

# Calculated the knots incorrectly at first
# works when you start from knot no. 3
ten_knots <- recursive_tail(path = select(head_and_tail, 3:4), knot = 3, max_knots = 10)

ten_knots %>%
  select(x_t, y_t) %>%
  unique.data.frame() %>%
  dim()
