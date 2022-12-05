
# Functions ---------------------------------------------------------------

get_moves <- function(x) {
  moves_text <- strsplit(trimws(gsub('[a-z]', '', x)), '  ')
  moves <- lapply(moves_text, as.numeric)
  return(moves)
}

move_crates <- function(pick = c('naive', 'expert')) {
  pick <- match.arg(pick)
  func <- switch(
    pick,
    naive = rev,
    expert = identity
  )
  function(crate, move) {
    qty <- move[[1]]
    from <- move[[2]]
    to <- move[[3]]
    crate_move <- head(crate[[from]], qty)
    crate_move <- do.call(func, list(x = crate_move))
    crate[[to]] <- c(crate_move, crate[[to]])
    crate[[from]] <- tail(crate[[from]], -qty)
    crate
  }
}


# Code --------------------------------------------------------------------

file <- 'input-day05.txt'
input <- readLines(file)
input_stacks <- read.fwf(file, n = 8, widths = rep(4, 9))
stacks <- lapply(input_stacks, \(x) gsub('[\\[\\]\\s]', '', x, perl = TRUE))
valid_stacks <- lapply(stacks, \(x) x[nchar(x) > 0])
moves <- get_moves(input[-c(1:10)])


# Part 1 ------------------------------------------------------------------

move_crates_naive <- move_crates(pick = 'naive')
new_stacks_naive <- Reduce(f = move_crates_naive, x = moves, init = valid_stacks)
stacks_letters_naive <- paste(lapply(new_stacks_naive, head, n = 1), collapse = '')


# Part 2 ------------------------------------------------------------------

move_crates_expert <- move_crates(pick = 'expert')
new_stacks_expert <- Reduce(f = move_crates_expert, x = moves, init = valid_stacks)
stacks_letters_expert <- paste(lapply(new_stacks_expert, head, n = 1), collapse = '')
