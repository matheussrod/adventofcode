
input <- readLines('input-day03.txt')
input_split <- strsplit(input, NULL)
all_letters <- c(letters, LETTERS)
input_match <- lapply(input_split, match, all_letters)
  
common_type <- function(x, n) {
  first_compartment <- head(x, n)
  second_compartment <- tail(x, n)
  both_type <- intersect(first_compartment, second_compartment)
  return(both_type)
}

common_items <- mapply(common_type, input_match, lengths(input_match)/2)

# Part 1 ------------------------------------------------------------------

priority <- sum(common_items)


# Part 2 ------------------------------------------------------------------

groups <- ceiling(seq_along(input_split)/3)
common_items_groups <- tapply(input_match, groups, \(.x) Reduce(intersect, .x))
priority_groups <- sum(common_items_groups)
