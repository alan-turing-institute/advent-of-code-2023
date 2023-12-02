file <- "input.txt"
vec <- readLines(file)

# Organise data
get_num <- function(vec, colour) {
  index <- grep(paste0("\\d+ ", colour), vec)
  if (length(index) == 0) return(0)
  return(as.numeric(gsub(paste0(" ", colour), "", vec[index])))
}

games <- strsplit(vec, ";") |>
  lapply(function(x) regmatches(x, gregexpr("\\d+ [a-z]+", x))) |>
  lapply(function(x) sapply(x, function(y) {
    data.frame(
      red=get_num(y, "red"),
      green=get_num(y, "green"),
      blue=get_num(y, "blue")
    )
  }))

# Part 1
maxima <- c(red=12, green=13, blue=14)
sum((1:length(games))[sapply(games, function(x) all(x <= maxima))])

# Part 2
sum(sapply(games, function(x) prod(apply(x, 1, function(y) max(unlist(y))))))
