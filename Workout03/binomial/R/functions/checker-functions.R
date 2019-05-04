# Title: check_prob
# Description: tests if input "prob" is a valid probability value (i.e. 0 ≤ p ≤ 1)
# Inputs
#   prob: a vector of probabilities
# Output
#   TRUE if "prob" is a valid probability value and an error message otherwise

check_prob <- function(prob) {
  if (is.numeric(prob) == FALSE) {
    stop("\ninvalid prob value; prob must be numeric")
  }
  if (prob < 0 | prob > 1) {
    stop("\ninvalid prob value; prob must be a number betwen 0 and 1")
  }
  if (length(prob) != 1) {
    stop("\ninvalid prob value; prob must be a number betwen 0 and 1")
  }
  TRUE
}


# Title: check_trials
# Description: tests if input "trial" is a valid value for number of trials (i.e. n is a non-negative integer)
# Inputs
#   trials: a vector of length 1 for number of trials
# Output
#   TRUE if "trial" is a valid value for number of trials and an error message otherwise

check_trials <- function(trials) {
  if ((trials %% 1) != 0) {
    stop("\ninvalid trials value; trials must be a non-negative integer")
  }
  if (trials < 0) {
    stop("\ninvalid trials value; trials must be a non-negative integer")
  }
  if (length(trials) != 1) {
    stop("\ninvalid trials value; trials must be a non-negative integer")
  }
  TRUE
}


# Title: check_success
# Description: tests if input "success" is a valid value for number of successes (i.e. 0 ≤ k ≤ n)
# Inputs
#   success: a vector of non-negative integer(s) for number of succcess
#   trials: a vector of non-negative integer(s) for number of trials
# Output
#   TRUE if success is a valid value for number of successes and an error message otherwise

check_success <- function(success, trials) {
  if (success > trials) {
    stop("\ninvalid success value; success cannot be greater than trials")
  }
  if ((success %% 1) != 0) {
    stop("\ninvalid success value; success must be a vector of non-negative integers")
  }
  if (success < 0) {
    stop("\ninvalid success value; success must be a vector of non-negative integers")
  }
  TRUE
}
