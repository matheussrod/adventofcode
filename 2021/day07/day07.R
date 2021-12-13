
input <- scan('input-day07.txt', sep = ',', quiet = TRUE)


fuel <- function(x, part2) {
  
  f1 <- function(x, position) {
    
    horizontal_step <- abs(x - position)
    
    if (part2) {
      costs <- sum(horizontal_step * (1 + horizontal_step) / 2)
    }
    else {
      costs <- sum(horizontal_step)  
    }
    
    return(costs)
  }
  
  f_minimize <- optimize(f1, range(x), position = x)
  
  minimize <- f1(x, round(f_minimize$minimum))
  
  return(minimize)
  
}


part1 <- fuel(input, part2 = FALSE)
part2 <- fuel(input, part2 = TRUE)