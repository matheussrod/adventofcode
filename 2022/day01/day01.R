
input <- read.table('input-day01.txt', blank.lines.skip = FALSE)
input$group <- cumsum(is.na(input$V1))
split_group <- split(input, input$group)
calories <- sapply(X = split_group, FUN = \(x) sum(x$V1, na.rm = TRUE))


# Part 1 ------------------------------------------------------------------

max(calories)


# Part 2 ------------------------------------------------------------------

sum(sort(calories, decreasing = TRUE)[1:3])
