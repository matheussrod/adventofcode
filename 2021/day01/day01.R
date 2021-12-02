
input <- as.numeric(readLines('input-day01.txt'))

part1 <- sum(diff(input) > 0)

part2 <- sum(diff(input, lag = 3) > 0)