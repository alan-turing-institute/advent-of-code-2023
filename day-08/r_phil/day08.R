file <- "input.txt"
input <- readLines(file)

# Process data
steps <- strsplit(input[1], "")[[1]]
df <- regmatches(input[-(1:2)], gregexpr("[A-Z]{3}", input[-(1:2)])) |>
  Reduce(rbind, x=_) |>
  as.data.frame(row.names=1:(length(input)-2))
names(df) <- c("location", "L", "R")

# Part 1 ----

find_n_steps <- function(dat, dat_index=1, sequence, seq_index=0, count=0) {
  while(substr(dat$location[dat_index], 3, 3) != "Z") {
    seq_index <- seq_index + 1
    if (seq_index > length(sequence)) seq_index <- 1
    count <- count+1
    dat_index <- which(dat$location == dat[[sequence[seq_index]]][dat_index])
  }
  return(count)
}

find_n_steps(df, which(df$location == "AAA"), steps, 0, 0)


# Part 2 ----

gcd <- function(vec) {
  a <- vec[1]; b <- vec[2]
  while(b) {
    new <- a %% b
    a <- b
    b <- new
  }
  return(a)
}

lcm <- function(vec) prod(vec) / gcd(vec)

lcms <- function(vec, curr=1) {
  if (length(vec) == 0) return(curr)
  curr <- lcm(c(curr, vec[1]))
  return(lcms(vec[-1], curr))
}

vec <- sapply(which(substr(df$location,3,3) == "A"), function(x) find_n_steps(df, x, steps, 0, 0))

sprintf("%.0f", lcms(vec))



