#### puzzle part 1 ------------------------------

## What would your total score be if everything goes exactly according to your strategy guide?

library(tidyverse)


## Load data ---
df <- readr::read_delim(file = "2022/Input02.txt", delim = " ", col_names = FALSE, skip_empty_rows = FALSE) %>%
  rename(opponent = X1, you = X2)

# Decoding for hands
opponents_hand <- c("A" = "Rock", "B" = "Paper", "C" = "Scissors")
your_hand <- c("X" = "Rock", "Y" = "Paper", "Z" = "Scissors")

## Add info to a data.frame
decoder_df <- data.frame("opponent" = names(opponents_hand),
                         "opponents_hand" = opponents_hand,
                         "you" = names(your_hand),
                         "your_hand" = your_hand,
                         "points" = c(1, 2, 3))

## Combine decoder info with strategy guide
part1 <- df %>%
  select(opponent) %>%
  left_join(select(decoder_df, opponent, opponents_hand), by = "opponent")

part2 <- df %>%
  select(you) %>%
  left_join(select(decoder_df, you, your_hand, points), by = "you")

df2 <- bind_cols(part1, part2)

## define outcome of each round and calculate point
df2 <- df2 %>%
  mutate(outcome = case_when(
    opponents_hand == "Rock" & your_hand == "Scissors" ~ 0,
    opponents_hand == "Rock" & your_hand == "Rock" ~ 3,
    opponents_hand == "Rock" & your_hand == "Paper" ~ 6,
    opponents_hand == "Scissors" & your_hand == "Scissors" ~ 3,
    opponents_hand == "Scissors" & your_hand == "Rock" ~ 6,
    opponents_hand == "Scissors" & your_hand == "Paper" ~ 0,
    opponents_hand == "Paper" & your_hand == "Scissors" ~ 6,
    opponents_hand == "Paper" & your_hand == "Rock" ~ 0,
    opponents_hand == "Paper" & your_hand == "Paper" ~ 3,
  )) %>%
  #total score per round
  mutate(total_score = points + outcome)

## Total score for all rounds
sum(df2$total_score)


## --- Part Two --------------

#decoding for outcomes
outcome <- c("X" = "lose", "Y" = "draw", "Z" = "win")

## Add info to a data.frame
decoder_df <- data.frame("outcome_code" = names(outcome),
                         "outcome" = outcome,
                         "outcome_score" = c(0, 3, 6))

## Combine decoder info with strategy guide
part2 <- df %>%
  select(you) %>%
  rename(outcome_code = you) %>%
  left_join(decoder_df, by = "outcome_code")

# combine
df3 <- bind_cols(part1, part2)

# define your hand and calculate final score
df3 <- df3 %>%
  mutate(your_hand = case_when(
    outcome == "win" & opponents_hand == "Rock" ~ "Paper",
    outcome == "win" & opponents_hand == "Paper" ~ "Scissors",
    outcome == "win" & opponents_hand == "Scissors" ~ "Rock",
    outcome == "draw" & opponents_hand == "Rock" ~ "Rock",
    outcome == "draw" & opponents_hand == "Paper" ~ "Paper",
    outcome == "draw" & opponents_hand == "Scissors" ~ "Scissors",
    outcome == "lose" & opponents_hand == "Rock" ~ "Scissors",
    outcome == "lose" & opponents_hand == "Paper" ~ "Rock",
    outcome == "lose" & opponents_hand == "Scissors" ~ "Paper"
  )) %>%
  # calculate your points based on your hand
  mutate(points = case_when(
    your_hand == "Rock" ~ 1,
    your_hand == "Paper" ~ 2,
    your_hand == "Scissors" ~ 3,
  )) %>%
  #total score per round
  mutate(total_score = points + outcome_score)


## Total score for all rounds
sum(df3$total_score)
