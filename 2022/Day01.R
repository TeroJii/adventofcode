#### puzzle part 1 ------------------------------

## Find the elf carring the most calories

library(tidyverse)

df <- readr::read_delim(file = "2022/Input01.txt", delim = "\n", col_names = FALSE, skip_empty_rows = FALSE) %>%
  rename(elves = X1)


rows_with_breaks <- which(is.na(df))

elves <- data.frame("until" = rows_with_breaks, elf = 1:length(rows_with_breaks))


df <- df %>%
  mutate(elf_no = NA)

## Add elf no to rows with NA, i.e. between elfs
for(i in seq_along(rows_with_breaks)){
  df$elf_no[rows_with_breaks[i]] <- elves$elf[i]
}

## fill in missing elf no's
df <- df %>%
  #fill NA's with elf no
  fill(elf_no, .direction = "up") %>%
  drop_na()

total_cals <- df %>%
  group_by(elf_no) %>%
  summarise(total_calories = sum(elves))


# Find the elf with most calories
total_cals[which.max(total_cals$total_calories),]

#### puzzle part 2 ------------------------------

# find the top three elves with most calories
total_cals %>%
  arrange(desc(total_calories)) %>%
  top_n(n = 3) %>%
  pull(total_calories) %>%
  sum()

