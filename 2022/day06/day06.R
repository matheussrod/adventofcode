
# Functions ---------------------------------------------------------------
different_letters <- function(x) {
  length(unique(x)) == length(x)
}

find_first_detected <- function(x, marker_type = c('packet', 'message')) {
  marker_type <- match.arg(marker_type)
  marker <- apply(x, 1, different_letters)
  first_marker <- which(marker)[[1]]
  return(first_marker)
}

# Code --------------------------------------------------------------------
input <- readLines('input-day05.txt')
input_split <- strsplit(input, '')[[1]]
packets_detected <-  embed(input_split, 4)
messages_detected <-  embed(input_split, 14)

# Part 1 ------------------------------------------------------------------
first_packet_marker <- find_first_detected(packets_detected, 'packet') + 3

# Part 2 ------------------------------------------------------------------
first_message_marker <- find_first_detected(messages_detected, 'message') + 13
