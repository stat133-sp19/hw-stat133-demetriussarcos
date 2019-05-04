context("Checker Functions")

test_that("prob is valid" {
  expect_error(check_prob("1/2"), "invalid prob value; prob must be numeric")
  expect_error(check_prob(-1), "invalid prob value; prob must be a number betwen 0 and 1")
  expect_error(check_prob(c(0.4, 0.6)), "invalid prob value; prob must be a number betwen 0 and 1")
  expect_length(prob, 1)
  expect_true(check_prob(0.5))
})

test_that("trials is valid" {
  expect_error(check_trials(c(1,2,3)), "invalid trials value; trials must be a non-negative integer")
  expect_error(check_trials(22.5), "invalid trials value; trials must be a non-negative integer")
  expect_error(check_trials(-5), "invalid trials value; trials must be a non-negative integer")
  expect_gte(trials, 1)
  expect_length(trials, 1)
  expect_true(check_trials(100))
})

test_that("success is valid" {
  expect_lte(success, trials)
  expect_gte(length(success), 1)
  expect_gte(success, 0)
})
