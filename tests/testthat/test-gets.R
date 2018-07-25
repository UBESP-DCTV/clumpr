context("test-get_region.R")

test_that("methods get_center return correct output", {
  center_1 <- center('Pavia', 'Lombardia')
  center_2 <- center('Milano', 'Lombardia')
  expect_equal(get_center(center_1), 'Pavia')
  expect_equivalent(get_center(set_centers(center_1, center_2)),
    c('Pavia', 'Milano')
  )
})

test_that("methods get_region return correct output", {
  center_test <- center('Padova', 'Veneto')
  region_test <- region(set_centers(center_test))
  expect_equal(get_region(center_test), 'veneto')
  expect_equal(get_region(region_test), 'veneto')
})

test_that("methods get_state return correct output", {
  center_test <- center('Padova', 'Veneto')
  expect_equal(get_state(center_test), 'italy')
})

test_that("methods get_p_default return correct output", {
  center_test <- center('Padova', 'Veneto', p_accept = 0.2)
  region_test <- region(set_centers(center_test), default_p = 0.5)
  expect_equal(get_p_default(region_test), 0.5)
  expect_equal(get_p_accept(region_test), 0.2)
})
