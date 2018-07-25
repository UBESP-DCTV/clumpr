context("test-zzz.R")

test_that("options are set", {
  expect_equal(getOption('clumpr.foo'), 'foo')
})
