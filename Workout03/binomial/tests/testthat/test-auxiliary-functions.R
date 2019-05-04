context("Summary Measures")

test_that("aux_mean computes valid binomial distribution mean" {
  expect_is(aux_mean(10, 0.5), "numeric")
  expect_gte(aux_mean(10, 0.5), 0)
  expect_error(aux_mean(10, "1/2"), "non-numeric argument")
})

test_that("aux_variance computes valid variance" {
  expect_is(aux_variance(10, 0.5), "numeric")
  expect_gte(aux_variance(10, 0.5), 0)
  expect_error(aux_variance(10, "1/2"), "non-numeric argument")
})

test_that("aux_mode computes valid mode" {
  expect_is(aux_mode(10, 0.5), "numeric")
  expect_length(aux_mode(15, 0.2), 2)
  expect_error(aux_mode(10, "1/2"), "non-numeric argument")
})

test_that("aux_skewness computes valid skewness value" {
  expect_is(aux_skewness(10, 0.5), "numeric")
  expect_length(aux_skewness(10, 0.5), 1)
  expect_error(aux_skewness(10, "1/2"), "non-numeric argument")
})

test_that("aux_kurtosis computes valid kurtosis value" {
  expect_is(aux_kurtosis(10, 0.5), "numeric")
  expect_length(aux_kurtosis(10,0.5), 1)
  expect_error(aux_kurtosis(10, "1/2"), "non-numeric argument")
})
