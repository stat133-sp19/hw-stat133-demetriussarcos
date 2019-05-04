# Title: aux_mean
# Description: computes the expected value or mean of a binomial distribution
# Inputs
#   prob: probability of success
#   trials: number of trials
# Output
#   Computed mean for a binomial distribution

aux_mean <- function(prob, trials) {
  prob * trials
}


# Title: aux_mean
# Description: computes the variance of a binomial distribution
# Inputs
#   prob: probability of success
#   trials: number of trials
# Output
#   Computed variance of a binomial distribution

aux_variance <- function(prob, trials) {
  trials * prob * (1 - prob)
}


# Title: aux_mode
# Description: computes the mode of a binomial dstributon
# Inputs
#   prob: probability of success
#   trials: number of trials
# Output
#   Computed mode of a binomial distribution

aux_mode <- function(trials, prob) {
  input <- (trials*prob)+prob
  m <- floor(input)
  if ((input %% 1) != 0) {
    output <- m
    return(output)
  }
  return(c(m, m-1))
}


# Title: aux_skewness
# Description: computes the skewness of a binomial dstributon
# Inputs
#   prob: probability of success
#   trials: number of trials
# Output
#   Computed skewness of a binomial distribution

aux_skewness <- function(prob, trials) {
  (1 - (2 * prob)) / sqrt(trials * prob * (1 - prob))
}


# Title: aux_kurtosis
# Description: computes the kurtosis of a binomial dstributon
# Inputs
#   prob: probability of success
#   trials: number of trials
# Output
#   Computed kurtosis of a binomial distribution

aux_kurtosis <- function(prob, trials) {
  (1 - ((6 * prob) * (1 - prob))) / (trials * prob * (1 - prob))
}
