
# Functions ---------------------------------------------------------------
list_expression <- \(x) gsub('\\]', ')', gsub('\\[', 'list(', x))

#function from https://github.com/wurli
right_order <- function(left, right) {
  left_length <- length(left)
  right_length <- length(right)
  length_range <- seq_len(max(left_length, right_length))
  
  for (idx in length_range) {
    if (idx > left_length) return(TRUE) 
    else if (idx > right_length) return(FALSE)
    result <- if (is.list(c(left[[idx]], right[[idx]]))) right_order(left[[idx]], right[[idx]]) else sign(left[[idx]] - right[[idx]])
    if (is.logical(result)) return(result) else if (result < 0) return(TRUE) else if (result > 0) return(FALSE)
  }
  0
}

divider_packet <- function(x, value) {
  x_value <- lapply(x, \(y) c(value, y))
  order <- lapply(x_value, \(y) sapply(tail(y, -1), \(z) right_order(y[[1]], z)))
  idx <- sapply(order, \(y) sum(y == FALSE))
  pos <- sum(idx)
  return(pos)
}

# Code --------------------------------------------------------------------
input <- readLines('input-day13.txt')
input_expression <- list_expression(input)
input_eval2list <- lapply(input_expression, \(x) eval(str2expression(x)))
pairs <- lapply(split(input_eval2list, cumsum(input == '')), \(x) Filter(\(y) !is.null(y), x))

# Part 1 ------------------------------------------------------------------
pairs_right_order <- which(sapply(pairs, \(x) right_order(x[[1]], x[[2]])))
total_pairs_right_order <- sum(pairs_right_order)

# Part 2 ------------------------------------------------------------------
keys <- list(list(list(2)), list(list(6)))
keys_pos <- sapply(keys, \(x) divider_packet(pairs, x)) + c(1, 2)
decorder_key <- prod(keys_pos)
