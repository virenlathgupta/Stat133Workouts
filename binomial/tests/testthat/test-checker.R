context("Check checker context")

test_that("check_prob with ok probabilities", {

  expect_true(check_prob(0.67))
  expect_true(check_prob(0))
})


test_that("check_prob fails with invalid prob", {

  expect_error(check_prob(-1))
  expect_error(check_prob(1.1))
})

test_that("check_prob fails with invalid lengths", {

  expect_error(check_prob(1:5))
  expect_error(check_prob(c(0.6, 0.3, 0.1)))
})


test_that("check_trials with ok trials", {
  expect_true(check_trials(99))
  expect_true(check_trials(1))
  expect_true(check_trials(0))
})


test_that("check_trials fails with invalid trials", {

  expect_error(check_trials(-4))
  expect_error(check_trials(-6))
  expect_error(check_trials(-10))
})


test_that("check_trials fails with vectors", {

  expect_error(check_trials(0:2))
  expect_error(check_trials(-6:4))
  expect_error(check_trials(1:6))
})


test_that("check_success with ok numbers and vectors", {
  expect_true(check_success(6, 8))
  expect_true(check_success(7, 7))
  expect_true(check_success(0:3, 6))
})

test_that("check_success with invalid numbers", {
  expect_error(check_success(9, 8))
  expect_error(check_success(10, 7))
  expect_error(check_success(0:7, 6))
})

test_that("check_success with invalid success", {
  expect_error(check_success(-3, 8))
  expect_error(check_success(-4, 7))
  expect_error(check_success(-1, 6))
})


