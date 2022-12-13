
# Functions ---------------------------------------------------------------
move_tail <- function(x) {
  for (head_idx in seq_along(x)[-1]) {
    tail_idx <- head_idx - 1
    diff_head_tail <-  x[[head_idx]] - x[[tail_idx]]
    if (any(abs(diff_head_tail) >= 2)) {
      x[[head_idx]] <- x[[tail_idx]] + sign(diff_head_tail)
    } else {
      x[[head_idx]] <- x[[tail_idx]]
    }
  }
  return(x)
}

length_unique <- \(x) length(unique(x))


# Code --------------------------------------------------------------------
input <- read.table('input-day09.txt', col.names = c('values', 'lengths'))
input_rle <- inverse.rle(input)
moves <- lapply(input_rle, \(x) switch(x, 'U' = c(-1, 0), 'D' = c(1, 0), 'L' = c(0, -1), 'R' = c(0, 1)))
head_pos <- Map(c, cumsum(lapply(moves, \(x) x[[1]])), cumsum(lapply(moves, \(x) x[[2]])))

# Part 1 ------------------------------------------------------------------
tail_pos <- move_tail(head_pos)
unique_tail_pos <- length_unique(paste(tail_pos, sep = ','))

# Part 2 ------------------------------------------------------------------
tail_ninth <- Reduce(f = \(x, y) move_tail(x), x = 1:8, init = tail_pos)
unique_tail_ninth <- length_unique(paste(tail_ninth, sep = ','))
