
# Functions ---------------------------------------------------------------
extract_all_numbers <- \(x) lapply(x, \(y) regmatches(y, gregexpr('\\(?[0-9]+', y))[[1]])

extract_operations <- \(x) lapply(x, \(y) regmatches(y, gregexpr('\\(?[\\+\\*\\-]+', y))[[1]])

monkey_business <- \(x) prod(sort(sapply(x, \(y) y[[7]]), decreasing = TRUE)[1:2])

monkey_round <- function(monkey, relief = TRUE) {
  divisor <- prod(sapply(monkey, \(x) as.numeric(x[[4]])))
  idx <- 1
  while (idx <= length(monkey)) {
    if (length(monkey[[idx]][[2]]) == 0) {
      increment <- which.max(sapply(tail(monkey, -idx), \(x) length(x[[2]]) > 0))
      idx <- idx + ifelse(length(increment) == 0, 1, increment)
      next
    }
    current_monkey <- monkey[[idx]]
    items <- as.numeric(current_monkey[[2]])
    op <- current_monkey[[3]]
    divisible <- as.numeric(current_monkey[[4]])
    true <- as.numeric(current_monkey[[5]]) + 1
    false <- as.numeric(current_monkey[[6]]) + 1
    inspect_item <- head(items, 1)
    monkey[[idx]][[7]] <- as.numeric(monkey[[idx]][[7]]) + 1
    monkey[[idx]][[2]] <- tail(items, -1)
    worry_level <- if (length(op) > 1) do.call(op[[1]], list(inspect_item, as.numeric(op[[2]]))) else inspect_item^2
    worry_level <- ifelse(relief, floor(worry_level / 3), worry_level %% divisor)
    thrown_to_monkey <- ifelse(worry_level %% divisible == 0, true, false)
    monkey[[thrown_to_monkey]][[2]] <- c(monkey[[thrown_to_monkey]][[2]], worry_level)
    if (length(monkey[[idx]][[2]]) == 0) {
      increment <- which.max(sapply(tail(monkey, -idx), \(x) length(x[[2]]) > 0))
      idx <- idx + ifelse(length(increment) == 0, 1, increment)
    }
  }
  return(monkey)
}

# Code --------------------------------------------------------------------
input <- readLines('input-day11.txt')
groups <- c(rep(1:7, each = 7), rep(8, 6))
monkeys <- mapply(
  \(x, y) {
    c(Filter(\(x) length(x) > 0, Map(c, y, x)), 0)
  },
  lapply(split(input, groups), extract_all_numbers),
  lapply(split(input, groups), extract_operations),
  SIMPLIFY = FALSE
)

# Part 1 ------------------------------------------------------------------
round20 <- Reduce(x = 1:20, init = monkeys, f = \(x, y) monkey_round(x))
level_business20 <- monkey_business(round20)

# Part 2 ------------------------------------------------------------------
round10000 <- Reduce(x = 1:10000, init = monkeys, f = \(x, y) monkey_round(x, relief = FALSE))
level_business10000 <- monkey_business(round10000)
