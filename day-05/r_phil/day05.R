file <- "test.txt"
input <- readChar(file, file.info(file)$size)

# Fn that given an input string,
values_to_mapping <- function(vec) {
  return(data.frame(
    input_start=vec[2],
    input_end=vec[2]  + vec[3] - 1,
    output_start=vec[1],
    output_end=vec[1] + vec[3] -1
  ))
}

str_to_mappings <- function(x) {
  rows <- strsplit(x, "\n")[[1]][-1]
  nums <- lapply(regmatches(rows, gregexpr("\\d+", rows)), as.numeric)
  map <- lapply(nums, values_to_mapping) |> Reduce(rbind, x=_)
  return(map)
}

# Process data
dat <- strsplit(input, "\n\n")[[1]]
seeds <- as.numeric(regmatches(dat[1], gregexpr("\\d+", dat[1]))[[1]])
maps <- dat[-1] |> lapply(str_to_mappings)

# Function that passes a seed through the series of maps
map_to_location <- function(x, maps, index=1) {
  output_index <- max(which(x >= maps[[index]]$input_start & x <= maps[[index]]$input_end), 0)
  if (output_index == 0) value <- x
  if (output_index > 0) value <- maps[[index]]$output_start[output_index] + (x - maps[[index]]$input_start[output_index])
  if (index == 7) return(value)
  return(map_to_location(value, maps, index+1))
}

# Part 1
min(sapply(seeds, function(x) map_to_location(x, maps, 1)))



# Part 2
get_seeds <- function(seeds) {
  adds <- seq(from=0, to=length(seeds)-2, by=2)
  part1 <- 1 + adds
  part2 <- 2 + adds
  return(data.frame(
    seed_start = seeds[part1],
    seed_fin = seeds[part1] + seeds[part2] -1
  ))
}

seeds2 <- get_seeds(seeds)


range_to_ranges <- function(r, maps, index) {
  if (index == 8) {
    return(r[1])
  }
  
  start <- maps[[index]]$input_start
  end <- maps[[index]]$input_end
  
  test1 <- r[1] > start & r[2] < end
  
  # print(paste0("Test 1:", any(test1)))
  if (any(test1)) {
    m <- maps[[index]][test1,]
    coef <- m$output_start - m$input_start
    new_r <- c(r[1] + coef, r[2] + coef)
    return(range_to_ranges(new_r, maps, index+1))
  }
  
  test2 <- !(r[1] > end | r[2] < start)
  if (any(test2)) {
    # Get all partially overlapping ranges
    m <- maps[[index]][test2,]
    
    # Find overlaps that correspond to 
    rdf <- lapply(1:nrow(m), function(x) {
      data.frame(
        start=max(m[x,]$input_start, r[1]),
        end=min(m[x,]$input_end, r[2]),
        coef=m[x,]$output_start - m[x,]$input_start
      )
    }) |> Reduce(rbind, x=_)
    
    # Add any 'hanging changes'
    rdf <- rdf[order(rdf$start),]
    
    if (nrow(rdf) > 1) {
      extras <- data.frame(start=numeric(0), end=numeric(0), coef=numeric(0))
      for (i in 1:(nrow(rdf)-1)) {
        if (rdf[i,"end"] + 1 < rdf[i+1, "start"]) {
          extras <- rbind(extras,
                          data.frame(
                            start=rdf[i,"end"] + 1,
                            end=rdf[i+1,"start"] - 1,
                            coef=0
                          ))
        }
      }
    }

    if (r[1] < rdf[1,"start"]) {
      rdf <- rbind(data.frame(start=r[1], end=rdf[1,"start"]-1, coef=0), rdf)
    }

    if (r[2] > rdf[nrow(rdf),"end"]) {
      rdf <- rbind(data.frame(start=rdf[nrow(rdf),"end"]+1, end=r[2], coef=0), rdf)
    }
    
    rdf$start <- rdf$start + rdf$coef
    rdf$end <- rdf$end + rdf$coef
    
    return(lapply(1:nrow(rdf), function(x) {
      range_to_ranges(as.numeric(rdf[x,c("start", "end")]), maps, index+1)
    }) |> Reduce(c, x=_))
  }
  
  return(range_to_ranges(r, maps, index+1))
}


sapply(1:length(seeds2), function(x) range_to_ranges(as.numeric(seeds2[x,]), maps, 1)) |>
  Reduce(c, x=_) |>
  min()

range_to_ranges(as.numeric(seeds2[4,]), maps, 1)
