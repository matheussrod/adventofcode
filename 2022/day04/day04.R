
# Functions ---------------------------------------------------------------

check_overlap_sections <- function(x, fully = TRUE) {
  first <- as.integer(x[[1]])
  second <- as.integer(x[[2]])
  first_seq <- seq(first[[1]], first[[2]])
  second_seq <- seq(second[[1]], second[[2]])
  first_in_second <- first_seq %in% second_seq
  second_in_first <- second_seq %in% first_seq
  first_second_list <- list(first_in_second, second_in_first)
  return(first_second_list)
}

is_fully <- \(x) all(x[[1]]) | all(x[[2]])
is_not_fully <- \(x) any(x[[1]]) | any(x[[2]])


# Code --------------------------------------------------------------------

input <- readLines('input-day04.txt')

input_split <- lapply(strsplit(input, ','), strsplit, split = '-')
sections_overlapping <- lapply(input_split, check_overlap_sections)


# Part 1 ------------------------------------------------------------------

fully <- sapply(sections_overlapping, is_fully)
fully_total <- sum(fully)


# Part 2 ------------------------------------------------------------------

not_fully <- sapply(sections_overlapping, is_not_fully)
not_fully_total <- sum(not_fully)
