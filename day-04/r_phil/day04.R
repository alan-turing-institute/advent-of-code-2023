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
  sapply(function(x) (x>0)*2^(x-1)) |>
  sum()

# Part 2
get_win_cards <- function(vec, index) {
  wins <- vec[index]
  if(wins > 0) return((index+1):(index+wins))
  return(wins)
}
inds_to_counts <- function(l, counts, index) {
  counts[index] <- 1 + sum(counts[l[[index]]])
  if (index == 1) return(counts)
  return(inds_to_counts(l, counts, index-1))
}

win_indices <- lapply(1:length(n_win), function(x) get_win_cards(n_win, x))
counts <- inds_to_counts(win_indices, rep(0, length(win_indices)), length(win_indices))
sum(counts)

