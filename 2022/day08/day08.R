
# Functions ---------------------------------------------------------------
view <- function(row, col, trees, dim_input) {
  remove_top_down <- ifelse(row == 1, -1, dim_input)
  remove_down_top <- ifelse(row == dim_input, -1, dim_input)
  remove_left_right <- ifelse(col == 1, -1, dim_input)
  remove_right_left <- ifelse(col == dim_input, -1, dim_input)
  
  select_top_down <- ifelse(row - 1 < 1, 1, row - 1)
  select_down_top <- ifelse(row + 1 > dim_input, dim_input, row + 1)
  select_left_right <- ifelse(col - 1 < 1, 1, col - 1)
  select_right_left <- ifelse(col + 1 > dim_input, dim_input, col + 1)
  
  top_down <- head(trees[select_top_down:1, col], remove_top_down)
  down_top <- head(trees[select_down_top:dim_input, col], remove_down_top)
  left_rigth <- tail(trees[row, select_left_right:1], remove_left_right)
  right_left <- tail(trees[row, select_right_left:dim_input], remove_right_left)
  
  list(
    "top_down" = top_down,
    "down_top" = down_top,
    "left_rigth" = left_rigth,
    "right_left" = right_left
  )
}

greater <- \(x, values) {
  check <- x > values
  value <- if (all(check)) sum(check) else which.min(ifelse(x > values, 1, 0))
  return(value)
}

acess_value_tree <- function(x) {
  list(
    "value" = x[[1]],
    "top_down" = x[[2]],
    "down_top" = x[[3]],
    "left_right" = x[[4]],
    "right_left" = x[[5]]
  )
}

visible_tree <- function(values, trees) {
  value_tree <- Map(c, values, trees)
  sapply(
    value_tree,
    \(x) {
      obj <- acess_value_tree(x)
      any(
        ifelse(length(obj$top_down), all(obj$value > obj$top_down), TRUE),
        ifelse(length(obj$down_top), all(obj$value > obj$down_top), TRUE),
        ifelse(length(obj$left_right), all(obj$value > obj$left_right), TRUE),
        ifelse(length(obj$right_left), all(obj$value > obj$right_left), TRUE)
      )
    }
  )
}

scenic_score <- function(values, trees) {
  value_tree <- Map(c, input_split, tree_view)
  sapply(
    value_tree,
    \(x) {
      obj <- acess_value_tree(x)
      Reduce(`*`, c(
        ifelse(length(obj$top_down), greater(obj$value, obj$top_down), 0),
        ifelse(length(obj$down_top), greater(obj$value, obj$down_top), 0),
        ifelse(length(obj$left_right), greater(obj$value, obj$left_right), 0),
        ifelse(length(obj$right_left), greater(obj$value, obj$right_left), 0))
      )
    }
  )
}

# Code --------------------------------------------------------------------
input <- paste(readLines('input-day08.txt'), collapse = '')
input_split <- as.numeric(strsplit(input, '')[[1]])
dim_input <- sqrt(length(input_split))
input_matrix <- matrix(input_split, nrow = dim_input, ncol = dim_input, byrow = TRUE)

tree_view <- lapply(
  Map(c, rep(1:dim_input, each = dim_input), rep(1:dim_input, dim_input)),
  \(x) view(x[[1]], x[[2]], input_matrix, dim_input)
)

# Part 1 ------------------------------------------------------------------
trees <- visible_tree(input_split, tree_view)
total_visible <- sum(trees)


# Part 2 ------------------------------------------------------------------
scenic <- scenic_score(input_split, tree_view)
max_scenic <- max(scenic)
