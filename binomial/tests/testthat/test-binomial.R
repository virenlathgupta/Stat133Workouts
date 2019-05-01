context("Check binomial context")

test_that("bin_choose with ok numbers", {

  expect_equal(bin_choose(4, 2), 6)
  expect_equal(bin_choose(3, 3), 1)
})


test_that("bin_choose fails with invalid numbers", {

  expect_error(bin_choose(3, 4))
  expect_error(bin_choose(3, 4))
})

test_that("bin_probability with ok numbers", {
  expect_equal(bin_probability(success = 2, trials = 5, prob = 0.5), 0.3125)
})


test_that("bin_probability fails with invalid trials", {
  expect_error(bin_probability(success = 2, trials = -1, prob = 0.5))
})


test_that("bin_probability fails with invalid probability and success", {
  expect_error(bin_probability(success = 2, trials = 5, prob = 1.1))
  expect_error(bin_probability(success = 6, trials = 5, prob = 1.1))
})


test_that("bin_ditribution with ok numbers", {
  expect_true(typeof(bin_distribution(2, .5)) == "list")
  expect_true(length(bin_distribution(2, .5)) == 2)
  expect_equal(bin_distribution(2, .5)[1, 2], .25)

})

test_that("bin_distribution fails with invalid numbers", {
  expect_error(bin_distribution(9, 8))
  expect_error(bin_distribution(-5, 0.3))
  expect_error(bin_distribution(9, 1.1))
})

test_that("bin_cumulative with ok numbers", {
  expect_true(typeof(bin_cumulative(2, .5)) == "list")
  expect_true(length(bin_cumulative(2, .5)) == 3)
  expect_equal(bin_cumulative(2, .5)[3, 3], 1)

})

test_that("bin_cumulative fails with invalid numbers", {
  expect_error(bin_distribution(9, 8))
  expect_error(bin_distribution(-5, 0.3))
  expect_error(bin_distribution(9, 1.1))
})



