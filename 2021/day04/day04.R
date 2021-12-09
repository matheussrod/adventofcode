
path <- 'input-day04.txt'

input_numbers <- as.numeric(strsplit(readLines(path, n = 1), ",")[[1]])
input_boards <- read.table(path, skip = 1)
boards <- rep(1:(nrow(input_boards) / 5), each = 5)
input_boards$board <- boards

rows <- dplyr::mutate(dplyr::rowwise(input_boards), row = list(c(V1, V2, V3, V4, V5)))$row

cols <- purrr::map(.x = dplyr::group_split(input_boards, board),
                   .f = ~ purrr::map(.x[, -6], ~ unlist(.x))) |> 
  purrr::flatten() |>
  unname()
  
boards_row_col <- tibble::tibble(value = c(rows, cols)) |> 
  tidyr::hoist(value, V1 = 1, V2 = 2, V3 = 3, V4 = 4, V5 = 5) |> 
  dplyr::mutate(board = rep(boards, 2)) |> 
  dplyr::rowwise() |> 
  dplyr::mutate(match_max =   max(which(!is.na(match(input_numbers, c(V1, V2, V3, V4, V5)))))) |> 
  dplyr::as_tibble()
  
winning_streak <- dplyr::with_groups(boards_row_col, board, ~ dplyr::summarise(.x, match = min(match_max)))
winning_streak_first <- winning_streak[winning_streak$match == min(winning_streak$match), ]
winning_streak_last <- winning_streak[winning_streak$match == max(winning_streak$match), ]


puzzle_answer <- function(.data, numbers, winner_position) {
  
  .data_sum <- .data |> 
    dplyr::filter(board == winner_position$board) |> 
    dplyr::mutate(dplyr::across(V1:V5, ~ ifelse(.x %in% numbers[1:winner_position$match], 0, .x))) |> 
    dplyr::summarise(answer = sum(V1 + V2 + V3 + V4 + V5)/2 * numbers[winner_position$match])
  
  return(.data_sum)
}

part1 <- puzzle_answer(boards_row_col, input_numbers, winning_streak_first)
part2 <- puzzle_answer(boards_row_col, input_numbers, winning_streak_last)
