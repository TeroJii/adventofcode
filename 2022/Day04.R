#### puzzle part 1 ------------------------------

## In how many assignment pairs does one range fully contain the other?

library(tidyverse)

## Load data ---
df <- readr::read_delim(file = "2022/Input04.txt", delim = ",", col_names = FALSE, skip_empty_rows = FALSE) %>%
  rename(range1 = X1, range2 = X2)

df <- df %>%
  #find the minima and maxima for both ranges
  separate(col = range1, into = c("min1", "max1"), sep = "-") %>%
  separate(col = range2, into = c("min2", "max2"), sep = "-") %>%
  mutate(across(.cols = everything(), .fns = as.numeric)) %>%
  # find if one range is within other
  mutate(first_in_second = (min1 >= min2 & max1 <= max2),
         second_in_first = (min2 >= min1 & max2 <= max1)) %>%
  # are some ranges fully contained in one
  mutate(range_contained = first_in_second|second_in_first)

## In how many assignment pairs does one range fully contain the other?
df %>% pull(range_contained) %>% sum()


## --- Part Two ---

##In how many assignment pairs do the ranges overlap?

df %>%
  mutate(overlap = (max1 <= max2 & max1>= min2) | (min1 >= min2 & min1<= max2) | range_contained) %>%
  pull(overlap) %>%
  sum()
