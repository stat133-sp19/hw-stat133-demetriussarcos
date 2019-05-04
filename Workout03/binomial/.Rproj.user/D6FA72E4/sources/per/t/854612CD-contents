#' @title bin_choose
#' @description calculates the number of combinations in which k successes can occur in n trials
#' @param n number of trials
#' @param k number of successes
#' @return computed number of trials
#' @examples
#' bin_choose(n = 5, k = 2)
#' bin_choose(5, 0)
#' bin_choose(5, 1:3)
#' @export

bin_choose <- function(n, k) {
  if (k > n) {
    stop("\nk cannot be greater than n")
  }
  factorial(n) / ((factorial(k)) * factorial(n - k))
}


#' @title bin_probability
#' @description computes the binomial probability of getting k successes in n trials
#' @param trials number of trials
#' @param success number of successes
#' @param prob probability of success
#' @return computed binomial probability
#' @examples
#' # probability of getting 2 successes in 5 trials
#' # (assuming prob of success = 0.5)
#' bin_probability(success = 2, trials = 5, prob = 0.5)
#' # probabilities of getting 2 or less successes in 5 trials
#' # (assuming prob of success = 0.5)
#' bin_probability(success = 0:2, trials = 5, prob = 0.5)
#' # 55 heads in 100 tosses of a loaded coin with 45% chance of heads
#' bin_probability(success = 55, trials = 100, prob = 0.45)
#' @export

bin_probability <- function(trials, success, prob) {
  check_trials(trials = trials)
  check_prob(prob = prob)
  check_success(success = success, trials = trials)
  bin_choose(n = trials, k = success) * (prob ^ success) * ((1 - prob) ^ (trials - success))
}


#' @title bin_distribution
#' @description creates a data frame of binomial distribution with successes in 1st column and probability in the other
#' @param trials number of trials
#' @param prob probability of success
#' @return data frame of the binomial distribution
#' @example
#' # binomial probability distribution
#' bin_distribution(trials = 5, prob = 0.5)
#' @export

bin_distribution <- function(trials, prob) {
  n <- trials
  p <- prob
  probs <- rep(0, n+1)
  for (k in 0:n) {
    probs[k+1] <- bin_probability(n, k, p)
  }
  df <- data.frame(
    success = 0:n,
    probability = probs
  )
  class(df) <- c("bindis", "data.frame")
  df
}


#' @title bin_cumulative
#' @description creates a data frame of a binomial distribution that lists the successes, the binomial probability, and the cumulative
#' @param trials number of trials
#' @param prob probability of success
#' @return the generated data frame
#' @example
#' # binomial cumulative distribution
#' bin_cumulative(trials = 5, prob = 0.5)
#' @export

bin_cumulative <- function(trials, prob) {
  n <- trials
  p <- prob
  probs <- rep(0, n+1)
  cum <- rep(0, n+1)
  for (k in 0:n) {
    probs[k+1] <- bin_probability(n, k, p)
  }

  df <- data.frame(
    success = 0:n,
    probability = probs,
    cumulative = cumsum(probs)
  )
  class(df) <- c("bincum", "data.frame")
  df
}


#' @title bin_variable
#' @description creates a list representing a binomial random variable with class "binvar"
#' @param trials number of trials
#' @param prob probability of success
#' @return the generated list
#' @example
#' bin_variable(trials = 10, p = 0.3)
#' @export

bin_variable <- function(trials, prob) {
  check_trials(trials = trials)
  check_prob(prob = prob)
  output <- list(trials = trials, prob = prob)
  class(output) <- "binvar"
  output
}


#' @title bin_mean
#' @description computes the expected value or mean of a binomial distribution
#' @param trials number of trials
#' @param prob probability of success
#' @return the computed mean for a binomial distribution
#' @example
#' binmean(10, 0.3)
#' @export

bin_mean <- function(trials, prob) {
  check_trials(trials = trials)
  check_prob(prob = prob)
  aux_mean(trials = trials, prob = prob)
}


#' @title bin_variance
#' @description computes the variance of a binomial distribution
#' @param trials number of trials
#' @param prob probability of success
#' @return Computed variance of a binomial distribution
#' @example
#' bin_variance(10, 0.3)
#' @export

bin_variance <- function(trials, prob) {
  check_trials(trials = trials)
  check_prob(prob = prob)
  aux_variance(trials = trials, prob = prob)
}

#' @title bin_mode
#' @description computes the mode of a binomial dstributon
#' @param prob probability of success
#' @param trials number of trials
#' @return
#' @example
#' bin_mode(10, 0.3)
#' @export

bin_modex <- function(trials, prob) {
  check_trials(trials = trials)
  check_prob(prob = prob)
  aux_mode(trials = trials, prob = prob)
}



#' @title bin_skewness
#' @description computes the skewness of a binomial dstributon
#' @param trials number of trials
#' @param prob probability of success
#' @return Computed skewness of a binomial distribution
#' @example
#' bin_skewness(10, 0.3)
#' @export

bin_skewness <- function(trials, prob) {
  check_trials(trials = trials)
  check_prob(prob = prob)
  aux_skewness(trials = trials, prob = prob)
}


#' @title bin_kurtosis
#' @description computes the kurtosis of a binomial dstributon
#' @param trials number of trials
#' @param prob probability of succcess
#' @return  Computed kurtosis of a binomial distribution
#' @example
#' bin_kurtosis(10, 0.3)
#' @export

bin_kurtosis <- function(trials, prob) {
  check_trials(trials = trials)
  check_prob(prob = prob)
  aux_kurtosis(trials = trials, prob = prob)
}
