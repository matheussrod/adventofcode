
input <- readLines('input-day05.txt') |> 
  dplyr::as_tibble() |> 
  tidyr::separate(value, into = c('x1', 'y1', 'x2', 'y2'), convert = TRUE)


count_points <- function(.data, part1) {
  
  if (part1) {
    .data <- dplyr::filter(.data, x1 == x2 | y1 == y2)    
  }
  
  count <- .data |> 
    dplyr::group_nest(row = dplyr::row_number()) |> 
    dplyr::mutate(diags = purrr::map(data, ~ tibble::tibble(x = .x$x1:.x$x2, y = .x$y1:.x$y2))) |> 
    tidyr::unnest(diags) |> 
    dplyr::count(x, y) |> 
    dplyr::filter(n >= 2) |> 
    nrow()
  
  
  return(count)
  
}


part1 <- count_points(input, TRUE)

part2 <- count_points(input, FALSE)
  