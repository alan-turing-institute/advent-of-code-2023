file <- "input.txt"
input <- read.table(file, sep=" ")

# Get hands from input
hands <- strsplit(input$V1, "")

# Card strengths + fn for retrieving strength
strength <- c(2:9, "T", "J", "Q", "K", "A")
card_to_strength <- function(card) which(strength==card)

# Hand-processing fn
process_hand <- function(hand) {
  hand_len <- length(unique(hand))
  if (hand_len == 1) return(7) #five of a kind
  if (hand_len == 2) {
    if (4 %in% table(hand)) return(6) #four of a kind
    return(5) # full house
  }
  if (hand_len == 3) {
    if (3 %in% table(hand)) return(4) #three of a kind
    return(3) # two pair
  }
  if (hand_len == 4) return(2) #one pair
  return(1) #high card
}


# Joker Processing Fn
j_hand_type <- function(hand, replacement) {
  hand[which(hand == "J")] <- replacement
  return(process_hand(hand))
}
j_process_hand <- function(hand) {
  if ("J" %in% hand) {
    return(max(sapply(strength[2:length(strength)], function(x) j_hand_type(hand, x))))
  }
  return(process_hand(hand))
}

# Input ordering fn
order_input <- function(input, fn) {
  input$hand <- sapply(hands, fn)
  dat <- cbind(input, t(sapply(hands, function(x) sapply(x, card_to_strength))))
  names(dat)[4:8] <- paste0("c_", 1:5)
  dat <- dat[order(dat$hand, dat$c_1, dat$c_2, dat$c_3, dat$c_4, dat$c_5),]
  return(dat)
}


# Part 1
out1 <- order_input(input, process_hand)
sum(1:nrow(out1)*out1$V2)


# Part 2
strength <- c("J", strength[(1:length(strength))[which(strength!="J")]])
out2 <- order_input(input, j_process_hand)
sum(1:nrow(out2)*out2$V2)


