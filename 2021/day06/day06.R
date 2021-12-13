
input <- scan('input-day06.txt', sep = ',', quiet = TRUE) |> 
  dplyr::as_tibble() |> 
  dplyr::count(value) |> 
  dplyr::right_join(tibble::tibble(days = 0:8), by = c('value' = 'days')) |> 
  tidyr::replace_na(list(n = 0)) |> 
  dplyr::arrange(value)

count_fish <- function(.data, value, count, days) {
  
  for (i in 1:days) {
    .data[[value]] <- (.data[[value]] - 1) %% 9
    .data[[count]][.data[[value]] == 6] <- .data[[count]][.data[[value]] == 6] + .data[[count]][.data[[value]] == 8]
  }
  
  fish <- format(sum(.data[[count]]), scientific = FALSE)
  
  return(fish)
  
}

part1 <- count_fish(input, 'value', 'n', 80)
part2 <- count_fish(input, 'value', 'n', 256)