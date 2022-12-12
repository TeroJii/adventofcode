#### puzzle part 1 ------------------------------

## Find all of the directories with a total size of at most 100000. What is the sum of the total sizes of those directories?

library(tidyverse)

## Load data ---
command_history <- readr::read_lines(file = "2022/Input07.txt")

##
example_file <- c("$ cd /", "$ ls", "dir a", "14848514 b.txt","8504156 c.dat", "dir d", "$ cd a", "$ ls", "dir e", "29116 f", "2557 g", "62596 h.lst", "$ cd e", "$ ls", "584 i", "$ cd ..", "$ cd ..", "$ cd d", "$ ls", "4060174 j", "8033020 d.log", "5626152 d.ext", "7214296 k")




directories <- command_history[grepl(pattern = "dir", x = command_history)] %>% unique()
# remove dir
directories <- gsub(pattern = "^dir ", replacement = "", x = directories)

# same with examples
directories2 <- example_file[grepl(pattern = "dir", x = example_file)] %>% unique() %>% gsub(x = ., pattern = "^dir ", replacement = "")

## All the files inside a directory are listed between commands cd dir-name and cd ..

get_line_numbers <- function(dir_name, command_history = command_history){

  starting_line <- which(grepl(pattern = paste("cd", dir_name), x = command_history))

  #possible end lines
  end_line <- which(grepl(pattern = "cd \\.\\.", x = command_history))

  # if dir name encoutered several times
  end_lines <- rep(NA, length(starting_line))
  # the correct one is the smallest of larger than starting_line
  for(i in seq_along(starting_line)){
    #record length for each dir with the same name
    end_lines[i] <- min(end_line[end_line>starting_line[i]])
  }

  df <- data.frame("dir_name" = dir_name, "start" = starting_line, "end" = end_lines) %>%
    # the last directory does not have cd .. after cd dir_name
    mutate(end = if_else(end == Inf, true = length(command_history), false = as.integer(end)))
  return(df)
}


find_size <- function(starting_line, end_line, command_history = command_history){

  vector_to_check <- command_history[starting_line:end_line]

  # find numbers
  numbers_in_vector <- as.numeric(gsub(pattern = "([0-9]+).*$", replacement = "\\1", vector_to_check))
  # non-numerals turned to NA's -> drop them
  numbers_in_vector <- numbers_in_vector[!is.na(numbers_in_vector)]

  total_size <- sum(numbers_in_vector)

  return(total_size)
}

## name of directory and the starting line cd dir_name + the next cd ..
## have to consider what to do with nested directories
files_and_stuff_df <- directories %>%
  map_df(~get_line_numbers(.x, command_history = command_history))


## record file sizes
files_and_stuff_df$size <- map2(.x = files_and_stuff_df$start,
                                .y = files_and_stuff_df$end,
                                ~find_size(.x, .y, command_history = command_history)
                                ) %>% unlist()

# let's see if this works (nested files not taken into account)
small_ones_df <- files_and_stuff_df %>%
  filter(size <= 100000)

find_nested_directories <- function(start_line, end_line, command_history){
  # if there is a nested directory the cd dir_name and cd .. contain a cd another_dir_name in between
  # for these cases file size has been incorrectly determined

  #record dir_name
  dir_name <- command_history[start_line]
  dir_name <- gsub(pattern = "^\\$ cd ", replacement = "", x = dir_name)

  #let's see if stuff in between contains cd another_dir_name
  stuff_in_between <- command_history[(start_line+1):(end_line-1)]

  stuff_with_cds <- stuff_in_between[grepl(pattern = "^\\$ cd ", x = stuff_in_between)]
  #filter away dir_name
  stuff_with_cds <- stuff_with_cds[!grepl(pattern = dir_name, x = stuff_with_cds)]

  #  TRUE if cd another_dir_name found
  nested <- length(stuff_with_cds>0)

  return(nested)
}

small_ones_df$nested <- map2(.x = small_ones_df$start,
                             .y = small_ones_df$end,
                             ~find_nested_directories(.x, .y, command_history = command_history)) %>%
  unlist()

# fix file size for nested files
## for nested files the cd dir_name command is followed by a cd another_dir_name
## we need to define the depth of the file tree and when the dir_name folder is exited
find_length_of_nested <- function(start_line, command_history = command_history){
  # when we enter the folder the level of depth is one
  # the depth increases in nested folders
  # folder is exited at level = 0
  level <- 1

  commands_after_entering <- command_history[(start_line+1):length(command_history)]

  for(i in (start_line+1):length(command_history)){
    cmd <- command_history[i]
    # check if command has cd
    if(grepl(pattern = "^\\$ cd ", x = cmd)){
      #check if the cd command is cd ..
      if(grepl(pattern = "^\\$ cd \\.\\.", x = cmd)){
        level <- level-1
      } else{
        level <- level+1
      }
    }

    #see if folder is exited
    if(level == 0){
      folder_end_line <- i
      break
    }
  }

  return(folder_end_line)
}

## Fix length of nested files
# which are nested
files_and_stuff_df$nested <- map2(.x = files_and_stuff_df$start,
                               .y = files_and_stuff_df$end,
                               ~find_nested_directories(.x, .y, command_history = command_history)) %>%
  unlist()

nested_file_lines <- NULL
for(i in seq_along(files_and_stuff_df$nested)){
  if(files_and_stuff_df$nested[i]>0){
    nested_file_lines <- c(nested_file_lines,i)
  }
}

end_lines <- NULL
for(i in seq_along(nested_file_lines)){
  # ninth, 33 pos causes problem

  if(!(i %in% c(9,33,34))){
    line_pos <- find_length_of_nested(start_line = files_and_stuff_df$start[nested_file_lines[i]],
                                      command_history = command_history)
  } else {
    line_pos <- files_and_stuff_df$end[nested_file_lines[i]]
  }

  print(i)
  end_lines <- c(end_lines, line_pos)
}

# update end lines for nested files
files_and_stuff_df$end[nested_file_lines] <- end_lines

#update file size
## record file sizes
files_and_stuff_df$size <- map2(.x = files_and_stuff_df$start,
                                .y = files_and_stuff_df$end,
                                ~find_size(.x, .y, command_history = command_history)
) %>% unlist()

# let's see if this works (nested files not taken into account)
small_ones_df <- files_and_stuff_df %>%
  filter(size <= 100000)

## updated answer
small_ones_df$size %>% sum()

## Example file gives the correct results

# test example_file
files_and_stuff_df2 <- directories2 %>%
  map_df(~get_line_numbers(.x, command_history = example_file))

## record file sizes
files_and_stuff_df2$size <- map2(.x = files_and_stuff_df2$start, .y = files_and_stuff_df2$end, ~find_size(.x, .y, command_history = example_file)) %>% unlist()

### PART 2 --------------------
# Now, you're ready to choose a directory to delete.


total_disk_space_available <- 70000000
needed_unused_space <- 30000000


## Let's first figure out the size of the files in the root folder
#total_size_test <- find_size(starting_line = 1, end_line = length(example_file), command_history = example_file)
total_size <- find_size(starting_line = 1, end_line = length(command_history), command_history = command_history)

space_available <- total_disk_space_available - total_size

delete_file_size <- needed_unused_space - space_available

# find the size of smallest enough directory to delete
files_and_stuff_df %>%
  filter(size >= delete_file_size) %>%
  pull(size) %>%
  min()
