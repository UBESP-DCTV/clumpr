context("test-utils.R")

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("tools for probability", {
  expect_equal(neg_p(0.2), 0.8)
  expect_equal(at_least_one(0.5, 0.5), 0.75)

  expect_error(neg_p(-1), 'too low')
  expect_error(neg_p(3), 'too high')
  expect_error(at_least_one(1.2), 'too high')
})
