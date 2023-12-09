file <- "input.txt"
input <- readLines(file) |> 
  lapply(function(x) as.numeric(strsplit(x, " ")[[1]]))

sequence_fn <- function(sequence, condition, op) {
  new_sequence <- sequence[-1] - sequence[-length(sequence)]
  value <- unique(sequence)
  if (length(value) == 1) return(value)
  return(op(sequence[eval(condition)], sequence_fn(new_sequence, condition, op)))
}

# Part 1
sapply(input, function(x) sequence_fn(x, rlang::expr(length(sequence)), `+`)) |> sum()

# Part 2
sapply(input, function(x) sequence_fn(x, 1, `-`)) |> sum()



