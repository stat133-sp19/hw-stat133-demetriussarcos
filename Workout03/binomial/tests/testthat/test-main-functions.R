context("Main Binomial Functions")

test_that("bin_choose computes valid combinations" {
  expect_is(bin_choose(10, 5), "numeric")
  expect_error(bin_choose(10, "one"), "non-numeric argument")
  expect_error(bin_choose(trials = 5, success = 10), "k cannot be greater than n")
})

test_that("bin_probability computes valid binomial probability" {
  expect_is(bin_probability(trials = 10, success = 5, prob = 0.5), "numeric")
  expect_length(bin_probability(trials = 10, success = 5, prob = 0.5), 1)
  expect_error(bin_probability(trials = 5, success = 10, prob = 0.5))
})

test_that("bin_distribution produces valid binomial distribution" {
  expect_is(bin_distribution(10, 0.5), c("bindis", "data.frame"))
  expect_named(bin_distribution(10,0.5), c("success", "probability"))
  expect_error(bin_distribution(10, "1/2"))
})

test_that("bin_cumulative produces valid binomial distribution" {
  expect_is(bin_cumulative(10, 0.5), c("bincum", "data.frame"))
  expect_named(bin_distribution(10, 0.5), c("success", "probability", "cumulative"))
  expect_error(bin_cumulative(10, "1/2"))
})
