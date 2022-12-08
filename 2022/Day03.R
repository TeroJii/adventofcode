#### puzzle part 1 ------------------------------

## Find the item type that appears in both compartments of each rucksack. What is the sum of the priorities of those item types?

library(tidyverse)

## Load data ---
df <- readr::read_delim(file = "2022/Input03.txt", delim = "\n", col_names = FALSE, skip_empty_rows = FALSE) %>%
  rename(rucksack = X1)

# split the contents of the rucksack to two compartments

df <- df %>%
  # length of content string
  mutate(length = str_length(rucksack)) %>%
  mutate(comp1 = str_sub(string = rucksack, start = 1L, end = length/2),
         comp2 = str_sub(string = rucksack, start = (length/2 +1), end = length))


# a function for finding one common letter
# note returns the last found common letter if strings share several common letters
find_common_letter <- function(str1, str2){
  # string length
  l <- str_length(str1)

  common_letter <- NULL
  for(i in 1:l){
    letter_to_test <- substring(str1, first = i, last = i)

    if(grepl(pattern = letter_to_test, x = str2)){
      common_letter <- letter_to_test
    }
  }

  return(common_letter)
}

# find common letters in rucksacks
df$common_letter <- map2(.x = df$comp1, .y = df$comp2, ~find_common_letter(.x, .y)) %>% unlist()

#assing priority
priority_df <- data.frame(common_letter = c(letters, LETTERS), priority = 1:52)

# calculate sum of priorities
df %>% left_join(priority_df, by = "common_letter") %>% pull(priority) %>% sum()


### --- Part Two -------------------

# Find the item type that corresponds to the badges of each three-Elf group.
# What is the sum of the priorities of those item types?

#elf groups
df2 <- df %>%
  # define elf groups = each set of three lines forms a group
  mutate(elf_group = ceiling(row_number()/3))

# a function for finding the common team tag
find_group_tag <- function(rucksack1, rucksack2, rucksack3){
  # record length of the first rucksack string
  l <- str_length(rucksack1)

  team_tag <- ""
  for(i in 1:l){
    letter_to_test <- substring(rucksack1, first = i, last = i)

    #check if letter is found from rucksack2 and rucksack3
    if(grepl(pattern = letter_to_test, x = rucksack2) & grepl(pattern = letter_to_test, x = rucksack3)){
      team_tag <- letter_to_test
    }

  }

  return(team_tag)
}


## Add team member numbers
df2$team_member <-  rep(1:3,100)

# add team members (and rucksack content) for each team side-by-side
teams_df <- df2 %>%
  select(-length, -comp1, -comp2, -common_letter) %>%
  # spread each team member to separate column
  pivot_wider(names_from = "team_member", values_from = "rucksack", names_prefix = "member_")

# find team badge
teams_df$team_badge <- purrr::pmap(list(teams_df$member_1, teams_df$member_2, teams_df$member_3), find_group_tag) %>% unlist()

# What is the sum of the priorities of those item types?
teams_df %>%
  rename(common_letter = team_badge) %>%
  left_join(priority_df, by = "common_letter") %>%
  pull(priority) %>%
  sum()
