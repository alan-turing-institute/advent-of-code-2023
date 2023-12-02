# Read in file ----
file <- "data.txt"
vec <- scan(file, character(), sep="\n", quote=NULL)

#  Fns ----
is_digit <- function(character) !is.na(suppressWarnings(as.numeric(character)))

find_digit <- function(string, n, operator) {
  character <- substring(string, n, n)
  if (is_digit(character)) return(character)
  else return(find_digit(string, operator(n, 1), operator))
}

words <- c("one", "two", "three", "four", "five",
           "six", "seven", "eight", "nine")
numbers <- 1:9

apply_replacements <- function(string, replacements, num=1) {
  indices <- replacements[[num]]
  for (i in indices) substr(string, i, i) <- as.character(num)
  if (num == 9) return(string)
  return(apply_replacements(string, replacements, num+1))
}

make_replacements <- function(string) {
  replacements <- lapply(numbers, function(x) gregexpr(words[x], string)[[1]])
  return(apply_replacements(string, replacements))
}

sum_values <- function(vec, replace) {
  if (replace) vec <- sapply(vec, function(x) make_replacements(x))
  first <- sapply(vec, function(x) find_digit(x, 1, `+`))
  last <- sapply(vec, function(x) find_digit(x, nchar(x), `-`))
  return(sum(as.numeric(paste0(first, last))))
}

# Part 1 ----
sum_values(vec, F)

# Part 2 ----
sum_values(vec, T)

