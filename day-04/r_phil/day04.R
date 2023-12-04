file <- "input.txt"
dat <- readLines(file)

# Fn to extract numbers from scratchcard
get_num_vec <- function(string) {
  if (grepl("Card", string)) string <- strsplit(string, ":")[[1]][2]
  return(regmatches(string, gregexpr("\\d+", string)))
}

# Format data
dat <- dat |> 
  strsplit("\\|") |>
  lapply(function(x) sapply(x, get_num_vec)) |>
  lapply(function(x) sapply(x, as.numeric))

# Part 1
n_win <- lapply(dat, function(x) sum(x[[1]] %in% x[[2]])) |> Reduce(c, x=_)
n_win |>
  sapply(function(x) (1*2^(x-1))*(x>0)) |>
  sum()

# Part 2
recurse_count_cards <- function(vec, index) {
  wins <- vec[index]
  if(wins > 0) next_cards <- (index+1):(index+wins)
  else return(1)
  return(1 + sum(sapply(next_cards, function(x) recurse_count_cards(vec, x))))
}

sapply(1:length(n_win), function(x) recurse_count_cards(n_win, x)) |>
  sum()

