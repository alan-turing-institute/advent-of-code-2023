file <- "input.txt"
input <- readLines(file)

# Process data
input_strs <- regmatches(input, gregexpr("\\d+", input))
dat <- as.data.frame(lapply(input_strs, as.numeric))
names(dat) <- c("time", "best")

# Part 1
time_fn <- function(t) (t-(1:t)) * 1:t
possible_times <- lapply(dat$time, time_fn)
sapply(1:nrow(dat), function(x) sum(possible_times[[x]] > dat$best[x])) |> prod()

# Part 2
dat2 <- as.data.frame(lapply(input_strs, function(x) as.numeric(paste0(x, collapse=""))))
names(dat2) <- names(dat)
sum(time_fn(dat2$time) > dat2$best)
