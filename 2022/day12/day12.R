
# Functions ---------------------------------------------------------------
find_possible_way <- function(x, row, col) {
  max_ncol <- ncol(x)
  max_nrow <- nrow(x)
  value <- ifelse(x[row, col] == 0, 1, x[row, col])
  value <- ifelse(value == 27, 26, value)
  points <- list(
    'up' = c(row, col) + c(-1, 0),
    'down' = c(row, col) + c(1, 0),
    'left' = c(row, col) + c(0, -1),
    'right' = c(row, col) + c(0, 1)
  )
  valid_points <- Filter(\(x) x[[1]] > 0 & x[[1]] <= max_nrow & x[[2]] > 0 & x[[2]] <= max_ncol, points)
  moves <- lapply(valid_points, \(p) c(x[p[[1]], p[[2]]]) - value)
  valid_moves <- valid_points[which(moves <= 1)]
  unique_values <- lapply(valid_moves, \(x) ifelse(x[[1]] == 1, 0, (x[[1]] - 1)) * max_ncol + x[[2]])
  from_to <- Map(c, (row - 1) * max_ncol + col, unique_values)
  return(from_to)
}

# Code --------------------------------------------------------------------
library(igraph)
input <- readLines('input-day12.txt')
height_map <- t(sapply(strsplit(input, ''), \(x) match(x, c('S', letters, 'E')) - 1))
idx_grid <- expand.grid(1:nrow(height_map), 1:ncol(height_map))

relations <- lapply(Map(c, idx_grid$Var1, idx_grid$Var2), \(x) {
  find_possible_way(height_map, x[[1]], x[[2]])
})
relations_unlist <- unlist(relations, recursive = FALSE)
relations_df <- data.frame(
  'from' = sapply(relations_unlist, \(x) x[[1]]),
  'to' = sapply(relations_unlist, \(x) x[[2]])
)
relations_graph <- graph_from_data_frame(relations_df)

start_point <- which(height_map == 0, arr.ind = TRUE)
end_point <- which(height_map == 27, arr.ind = TRUE)
get_node <- \(x) ifelse(x[[2]] == 1, 0, (x[[2]] - 1)) * nrow(height_map) + x[[1]]
start_node <- get_node(start_point)
end_node <- get_node(end_point)

# Part 1 ------------------------------------------------------------------
path_start_end <- shortest_paths(relations_graph, start_node, end_node)$vpath[[1]]
total_steps <- length(path_start_end) - 3

# Part 2 ------------------------------------------------------------------
all_a_elevation <- which(height_map == 1 | height_map == 0, arr.ind = TRUE)
all_nodes_a <- apply(all_a_elevation, 1, get_node)
all_paths <- sapply(all_nodes_a, \(x) shortest_paths(relations_graph, x, end_node)$vpath[[1]])
total_fewest_steps <- min(Filter(\(x) x > 0, sapply(all_paths, length))) - 3
