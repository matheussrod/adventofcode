
# Code --------------------------------------------------------------------
input <- readLines('input-day10.txt')
input_split <- strsplit(input, ' ')
instructions <- sapply(input_split, \(x) x[[1]])
n_cycles <- ifelse(instructions == "noop", 1, 2)
values <- as.numeric(sapply(input_split, \(x) if (length(x) == 1) 0 else x[[2]]))
X <- Reduce(
  \(x, y) {
    last_value <- if (length(x$seq) == 1) x$seq else tail(x$seq, 1)
    x[[2]]$value <- x[[2]]$value + if (y$cycle == 2) y$value else 0
    x[[2]]$cycle <- x[[2]]$cycle + y$cycle
    x$seq <- c(x$seq, last_value, if (y$cycle == 2) x[[2]]$value)
    x
  },
  x = Map(\(x, y) list('cycle' = x, 'value' = y), n_cycles, values),
  init = list(seq = c(1), list('cycle' = 1, 'value' = 1))
)

# Part 1 ------------------------------------------------------------------
cycles <- seq(20, 220, 40)
X_sum <- sum(X$seq[cycles] * cycles)

# Part 2 ------------------------------------------------------------------
raw_CRT <- abs(X$seq[-240] - 0:239 %% 40)
CRT <- ifelse(raw_CRT <= 1, TRUE, FALSE)
cols <- 240 / 40
points <- which(matrix(CRT, ncol = cols) == 1, arr.ind = TRUE)
plot(points, ylim = c(0, 10))
#RZEKEFHA
