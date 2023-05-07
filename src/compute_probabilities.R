# Function to calculate the probabilities where n is the number of people
f <- function(n) {

  # Probability that no two people have the same birthday
  p1 <- prod((365 - seq_len(n) + 1) / 365)

  # Probability that at least two people have the same birthday
  p2 <- 1 - p1

  # Probability that exactly two people have the same birthday
  p3 <- choose(n, 2) * (365 / 365) * (1 / 365) * prod((365 - seq_len(n - 2)) / 365)

  list("Prob. no two people have the same birthday" = p1,
       "Prob. at least two people have the same birthday" = p2,
       "Prob. exactly two people have the same birthday" = p3)
}