
input <- read.table('input-day02.txt')

input_puzzle <- input |> 
  dplyr::as_tibble() |> 
  dplyr::transmute(course = ifelse(V1 %in% c('down', 'up'), 'depth', V1),
                   units = ifelse(V1 == 'up', -V2, V2)) |> 
  dplyr::group_by(course)

part1_units <- dplyr::summarise(input_puzzle, units = sum(units))

part1 <- Reduce('*', part1_units$units)


part2_units <- input_puzzle |> 
  dplyr::mutate(aim = ifelse(course == 'depth', cumsum(units), NA_integer_)) |> 
  dplyr::ungroup() |> 
  tidyr::fill(aim, .direction = 'down') |> 
  dplyr::mutate(aim = ifelse(course == 'forward', aim * units, aim)) |> 
  dplyr::filter(course == 'forward') |> 
  dplyr::summarise(units = sum(units),
                   aim = sum(aim, na.rm = T))

part2 <- part2_units$units * part2_units$aim