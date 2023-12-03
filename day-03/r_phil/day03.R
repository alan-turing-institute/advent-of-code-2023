file <- "input.txt"
dat <- readLines(file) |> 
  sapply(function(x) strsplit(x, "")) |> 
  Reduce(rbind, x=_)

# Fns for extracting matrices
get_lgl <- function(input, pattern) t(apply(input, 1, function(x) grepl(pattern, x)))
lgl_to_indices <- function(lgl) which(lgl, arr.ind=T)
get_symbol_indices <- function(input, pattern) lgl_to_indices(get_lgl(input, pattern))

# Fn to extract inds to search over
get_search_inds <- function(inds) {
  grid <- expand.grid(-1:1,-1:1)
  return(apply(inds, 1, function(x) t(replicate(9, x)) + grid))
}

# Fn to find intersections of two sets of inds
get_intersection <- function(inds1, inds2) {
  joint <- rbind(inds1, inds2)
  return(joint[duplicated(joint),])
}

# Fn to extract index slices for numbers from logical matrices
get_slice_part <- function(lgl, row, col, op, condition) {
  if (col != condition) {
    if (lgl[row, op(col, 1)]) {
      return(get_slice_part(lgl, row, op(col, 1), op, condition))
    }
  }
  return(col)
}
get_num_slice <- function(lgl, row, col) {
  start <- get_slice_part(lgl, row, col, `-`, 1)
  end <- get_slice_part(lgl, row, col, `+`, ncol(lgl))
  return(c(row, start, end))
}

# Fn that given an intersection matrix, applies get_num_slice to each row and returns a matrix
mat_get_slices <- function(mat) {
  out <- t(apply(mat, 1, function(x) get_num_slice(num_grid, x[1], x[2])))
  return(out[!duplicated(out),])
}

# Fn that takes a slice matrix, and returns numbers
slice_to_nums <- function(slice, num_mat) as.numeric(paste0(num_mat[slice[1], slice[2]:slice[3]], collapse=""))
slice_mat_to_nums <- function(slice_mat, num_mat) {
  if (!"matrix" %in% class(slice_mat)) return(slice_to_nums(slice_mat, num_mat))
  return(apply(slice_mat, 1, function(x) slice_to_nums(x, num_mat)))
}

# Fn that brings everything together
pipeline <- function(dat, pattern, part2=F) {
  symbol_inds <- get_symbol_indices(dat, "[^0-9\\.]")
  search_inds <- get_search_inds(symbol_inds)
  intersections <- lapply(search_inds, function(x) get_intersection(x, num_inds))
  num_slices <- lapply(intersections, mat_get_slices)
  if(part2) num_slices <- num_slices[sapply(num_slices, function(x) max(nrow(x), 0) > 0)]
  nums <- lapply(num_slices, function(x) slice_mat_to_nums(x, dat))
  return(nums)
}

# Prepare data
num_grid <- get_lgl(dat, "\\d")
num_inds <- lgl_to_indices(num_grid)
colnames(num_inds) <- paste0("Var", 1:2)

# Part 1
pipeline(dat, "[^0-9\\.]") |> Reduce(c, x=_)  |> sum()

# Part 2
pipeline(dat, "\\*", T) |> sapply(function(x) prod(x)) |> sum()

