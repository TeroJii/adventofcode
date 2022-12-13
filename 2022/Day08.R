#### puzzle part 1 ------------------------------

## Consider your map; how many trees are visible from outside the grid?

library(tidyverse)

## Load data ---
trees_df <- readr::read_delim(file = "2022/Input08.txt", delim = "\n", col_names = FALSE) %>%
  rename(tree_rows = X1) %>%
  separate(col = "tree_rows", into = paste0("col", 0:99), sep = "") %>%
  # separate makes an extra empty column, which we remove
  select(-col0) %>%
  mutate_all(as.numeric)

# if we consider a tree at position [i,j] visibility is determined by size of trees in the same row and column

is_visible <- function(df){
  max_rows <- dim(df)[1]
  max_cols <- dim(df)[2]


  visibility_df <- data.frame(matrix(nrow = max_rows, ncol = max_cols))

  #Everything on the sides is visible (TRUE if visible, FALSE if not)
  visibility_df[1,] <- visibility_df[,1] <- visibility_df[max_rows,] <- visibility_df[,max_cols] <- TRUE

  ## go through trees on rows 2 -> (max_rows - 1) and cols 2 -> (max_cols - 1)

  # rows
  for(i in 2:(max_rows-1)){
    #columns
    for(j in 2:(max_cols-1)){
     # record tree length
      tree_length <- df[i,j] %>% as.numeric()

      # seen from top?
      col_on_top <- df[1:(i-1),j]
      visibility_top <- all(tree_length>col_on_top)

      #seen from bottom?
      col_below <- df[(i+1):max_rows,j]
      visibility_bottom <- all(tree_length>col_below)

      #seen from left?
      row_left <- df[i,1:(j-1)]
      visibility_left <- all(tree_length>row_left)

      #seen from right
      row_right <- df[i,(j+1):max_cols]
      visibility_right <- all(tree_length>row_right)

      #visibility from any directions
      visible <- visibility_top|visibility_bottom|visibility_left|visibility_right

      #record visibility
      visibility_df[i,j] <- visible
    }
  }


  return(visibility_df)
}

## Visibility of trees?
tree_vis_df <- is_visible(trees_df)

#amount of visible trees
rowSums(tree_vis_df) %>% sum()

## PArt 2 -------------------------
## Consider each tree on your map. What is the highest scenic score possible for any tree?

# a function for scoring a row/column of trees before the tree (top/left)
score_before <- function(column_of_trees, tree_length){
  max_score <- length(column_of_trees)
  score <- max_score

  for(k in seq_along(column_of_trees)){
    if(column_of_trees[length(column_of_trees)- (k-1)] >= tree_length){
      score <- k
      break
    }
  }
  return(score)
}

# a function for scoring a row/column of trees after the tree (bottom/right)
score_after <- function(column_of_trees, tree_length){
  max_score <- length(column_of_trees)
  score <- max_score

  for(k in seq_along(column_of_trees)){
    if(column_of_trees[k] >= tree_length){
      score <- k
      break
    }
  }
  return(score)
}

score_scenes <- function(df){
  max_rows <- dim(df)[1]
  max_cols <- dim(df)[2]


  scenic_scores_df <- data.frame(matrix(nrow = max_rows, ncol = max_cols))

  # score on sides is 0
  scenic_scores_df[1,] <- scenic_scores_df[,1] <- scenic_scores_df[max_rows,] <- scenic_scores_df[,max_cols] <- 0

  ## go through trees on rows 2 -> max_rows - 1 and cols 2 -> max_cols - 1


  # rows
  for(i in 2:(max_rows-1)){
    #columns
    for(j in 2:(max_cols-1)){
      # record tree length
      tree_length <- df[i,j] %>% as.numeric()

      # score from top?
      col_on_top <- df[1:(i-1),j] %>% unlist() %>%  as.numeric()
      score_top <- score_before(col_on_top, tree_length = tree_length)

      # score from bottom?
      col_below <- df[(i+1):max_rows,j] %>% unlist() %>% as.numeric()
      score_bottom <- score_after(col_below, tree_length = tree_length)

      # score from left?
      row_left <- df[i,1:(j-1)] %>% unlist() %>% as.numeric()
      score_left <- score_before(column = row_left, tree_length = tree_length)

      #seen from right
      row_right <- df[i,(j+1):max_cols] %>% unlist() %>% as.numeric()
      score_right <- score_after(row_right, tree_length = tree_length)

      #visibility from any directions
      score <- score_top*score_bottom*score_left*score_right

      #record scenic score
      scenic_scores_df[i,j] <- score
    }
  }


  return(scenic_scores_df)
}

## Scenic score of trees?
tree_scores_df <- score_scenes(trees_df)

## Maximum score for any tree?
max(tree_scores_df)
