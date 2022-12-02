# Functions ---------------------------------------------------------------

convert_to_same_shape <- function(x) {
  if (x[[2]] == 'X') round <- c(x[[1]], 'A')
  if (x[[2]] == 'Y') round <- c(x[[1]], 'B')
  if (x[[2]] == 'Z') round <- c(x[[1]], 'C')
  return(round)
}

convert_to_new_strategy <- function(x) {
  if (x[[2]] == 'A') {
    if (x[[1]] == 'A') round <- c(x[[1]], 'C')
    else if (x[[1]] == 'B') round <- c(x[[1]], 'A')
    else round <- c(x[[1]], 'B')
  }
  else if (x[[2]] == 'B') {
    round <- c(x[[1]], x[[1]])
  }
  else {
    if (x[[1]] == 'A') round <- c(x[[1]], 'B')
    else if (x[[1]] == 'B') round <- c(x[[1]], 'C')
    else round <- c(x[[1]], 'A')
  }
  return(round)
}

score_shape <- function(x) {
  shape_points <- c('1' = 'A', '2' = 'B', '3' = 'C')
  scores <- sapply(X = x, FUN = \(.x) match(.x[[2]], shape_points))
  total <- sum(scores)
  return(total)
}

get_outcome_result <- function(x) {
  if (x[[1]] == 'A' && x[[2]] == 'B') result <- 6
  else if (x[[1]] == 'B' && x[[2]] == 'C') result <- 6
  else if (x[[1]] == 'C' && x[[2]] == 'A') result <- 6
  else if (x[[1]] == x[[2]]) result <- 3
  else result <- 0
  return(result)
}

score_outcome <- function(x) {
  scores <- sapply(X = x, FUN = get_outcome_result)
  total <- sum(scores)
  return(total)
}


# Code --------------------------------------------------------------------

input <- readLines('input-day02.txt')
round <- strsplit(x = input, split = ' ')
round_converted <- lapply(round, convert_to_same_shape)
round_new_converted <- lapply(round_converted, convert_to_new_strategy)

# Part 1 ------------------------------------------------------------------
total_score <- score_shape(round_converted) + score_outcome(round_converted)

# Part 2 ------------------------------------------------------------------
total_new_score <- score_shape(round_new_converted) + score_outcome(round_new_converted)
