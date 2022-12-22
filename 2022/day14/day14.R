
# Functions ---------------------------------------------------------------
make_groups <- function(x, groups) {
  m <- length(x) - groups + 1
  data <- x[1:m + rep.int(1:groups, rep.int(m, groups)) - 1]
  groups <- rep(seq_len(m), groups)
  unname(split(data, groups))
}

sand_move <- function(position, points) {
  stopifnot(!is.unsorted(points))
  valid_points <- points[Arg(points - position) > 0]
  possible <- min(which(Re(position - valid_points) == 0))
  new_point <- valid_points[[possible]] - 1i
  left_possible <- valid_points[[possible]] - 1
  right_possible <- valid_points[[possible]] + 1
  result <- if (!(left_possible %in% points)) sand_move(left_possible, points)
    else if (!(right_possible %in% points)) sand_move(right_possible, points)
    else sort(c(points, new_point))
  return(result)
}

# Code --------------------------------------------------------------------
input <- readLines('input-day14.txt')
shape_path <- lapply(strsplit(input, ' -> '), \(x) strsplit(x, ',')) |> 
  lapply(\(x) lapply(x, as.integer))

min_x <- min(rapply(shape_path, \(x) x[[1]])) - 1
points <- rapply(shape_path, \(x) c(x[[1]] - min_x, x[[2]] + 1), how = 'list')
points_groups <- sapply(points, \(x) make_groups(x, 2))

points_complex <- lapply(points_groups, \(x) lapply(x, \(y) {
    seq(y[[1]][[1]], y[[2]][[1]]) + seq(y[[1]][[2]], y[[2]][[2]])*1i
  })) |> unlist() |> unique() |> sort()

begin <- 500 - min_x + 1i
limit <- max(Re(points_complex)) * max(Im(points_complex))

# Part 1 ------------------------------------------------------------------
new_points_complex <- sand_move(begin, points_complex)
abyss_begin <- 1
while (TRUE) {
  new_points_complex <- sand_move(begin, new_points_complex)
  abyss_begin <- abyss_begin + 1
}

# Part 2 ------------------------------------------------------------------
# Optimization seriously needed
floor_idx <- max(Im(points_complex)) + 2
points_floor <- sort(c(points_complex, c(-200:200) + complex(r = 0, i = floor_idx)))
new_points_floor <- sand_move(begin, points_floor)
sands_unit <- 1
while (!(begin %in% new_points_floor)) {
  new_points_floor <- sand_move(begin, new_points_floor)
  sands_unit <- sands_unit + 1
}
