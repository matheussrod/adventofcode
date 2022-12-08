
# Functions ---------------------------------------------------------------
folder_size <- function(x) {
  files <- gsub('\\D', '', x)
  sizes <- as.numeric(files)
  total_size <- sum(sizes, na.rm = TRUE)
  return(total_size)
}

# Code --------------------------------------------------------------------
input <- readLines('input-day07.txt')
cd <- grepl('\\$ cd [a-z]', input)
dotdot <- grepl('\\$ cd \\.{2}', input)
not_dotdot <- !dotdot
valid_input <- input[not_dotdot]
depth <- (cumsum(cd) - cumsum(dotdot))[not_dotdot]
cd_idx <- which(cd[not_dotdot])
cd_idx_next <- cd_idx + c(diff(cd_idx), length(valid_input) - cd_idx[length(cd_idx)])

files <- lapply(cd_idx, \(x){
  depth_value <- depth[[x]]
  folders <- cd_idx[cd_idx > x]
  same_depth <- depth[folders] == depth_value
  greater_depth <- depth[folders] < depth_value
  next_folder <- cd_idx_next[cd_idx_next > x]
  idx <- match(TRUE, same_depth | greater_depth)
  idx_adj <- ifelse(is.na(idx), length(next_folder), idx)
  seq(x, next_folder[[idx_adj]])
})

# Part 1 ------------------------------------------------------------------
sizes <- sort(sapply(files, \(x) folder_size(valid_input[x])))
total_size <- sum(sizes[sizes < 100000])

# Part 2 ------------------------------------------------------------------
size_root <- folder_size(valid_input)
needed <- 30000000 - (70000000 - size_root)
size_to_free <- sizes[which.max(sizes > needed)]
