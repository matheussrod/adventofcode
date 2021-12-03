
input <- readLines('input-day03.txt')


# Functions -----------------------------------------------------------------------------------------

get_binary <- function(.data) {
  
  .data_frame <- data.frame(
    V1 = .data[[1]],
    V2 = .data[[2]],
    V3 = .data[[3]],
    V4 = .data[[4]],
    V5 = .data[[5]],
    V6 = .data[[6]],
    V7 = .data[[7]],
    V8 = .data[[8]],
    V9 = .data[[9]],
    V10 = .data[[10]],
    V11 = .data[[11]],
    V12 = .data[[12]]
  )
  
  return(.data_frame)
}

find_mode <- function(x, part1, part2 = FALSE) {
  
  u <- unique(x)
  
  if (length(u) == 1) return(u)
  
  tab <- tabulate(match(x, u))
  
  if (part1 == 'max') {
    
    if (part2 & tab[[1]] == tab[[2]]) {
      mode <- '1'
    }
    else{
      mode <- u[which.max(tab)]  
    }
    
  } 
  else if (part1 == 'min') {
    
    if (part2 & tab[[1]] == tab[[2]]) {
      mode <- '0'
    }
    else{
      mode <- u[which.min(tab)]  
    }
  }
  
  
  return(mode)
}

binary_number_p1 <- function(.data, number = 'max') {
  
  if (number == 'max') {
    modes <- dplyr::summarise(.data, dplyr::across(dplyr::everything(), ~find_mode(.x, part1 = number)))
  }
  else if (number == 'min') {
    modes <- dplyr::summarise(.data, dplyr::across(dplyr::everything(), ~find_mode(.x, part1 = number)))
  }
  
  rate <- strtoi(paste0(modes, collapse = ''), 2)
  
  return(rate)
}

binary_number_p2 <- function(.data, part1, part2) {
  
  col_position <- 1
  
  col_names <- names(.data)
  
  while (nrow(.data) > 1) {
    
    mode <- find_mode(x = .data[[col_position]], part1 = part1, part2 = part2)
    
    .data <- .data[.data[[col_position]] == mode, ]
    
    col_position <- col_position + 1
  }
  
  rate <- strtoi(paste0(.data, collapse = ''), 2)
  
  
  return(rate)
  
}


# Part 01 -------------------------------------------------------------------------------------------

input_binary <- purrr::map_df(.x = strsplit(input, ""), .f = ~ get_binary(.x))

gamma_rate <- binary_number_p1(input_binary, 'max')

epsilon_rate <- binary_number_p1(input_binary, 'min')

part1 <- gamma_rate * epsilon_rate


# Part 02 -------------------------------------------------------------------------------------------

oxygen_rate <- binary_number_p2(input_binary, 'max', TRUE)

CO2_rate <- binary_number_p2(input_binary, 'min', TRUE)

part2 <- oxygen_rate * CO2_rate