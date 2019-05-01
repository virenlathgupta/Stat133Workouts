context("Context for summary measures.")

test_that("aux_mean with ok numbers", {

  expect_true(aux_mean(6, 0.5) == 3)
  expect_true(aux_mean(10, 0.9) == 9)
  expect_true(aux_mean(10, 0.8) == 8)
})


test_that("aux_variance with ok numbers", {

  expect_true(aux_variance(6, 0.5) == 1.5)
  expect_true(aux_variance(10, 0.5) == 2.5)
  expect_true(aux_variance(20, 0.5) == 5)
})

test_that("aux_mode with ok numbers", {

  expect_true(aux_mode(10, 0.3) == 3)
  expect_true(aux_mode(10, 0.5) == 5)
  expect_true(aux_mode(10, 0.7) == 7)
})


test_that("aux_skewness with ok numbers", {

  expect_equal(aux_skewness(10, 0.5), 0)
  expect_equal(aux_skewness(2, 0.7), -0.6172134)
  expect_equal(aux_skewness(8, 0.3), 0.3086067)

})


test_that("aux_kurtosis with ok numbers", {

  expect_equal(aux_kurtosis(10, 0.5), -.2)
  expect_equal( aux_kurtosis(7, 0.8), 0.03571429)
  expect_equal(aux_kurtosis(8, 0.5), -.25)
})
