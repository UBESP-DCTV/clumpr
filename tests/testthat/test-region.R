context("test-region.R")

test_that("set_centers input", {
  padova <- center('Padova', 'Veneto')
  pavia  <- center('Pavia',  'Lombardia')
  milano <- center('Milano', 'Lombardia')

  expect_error(set_centers(padova, 1), 'must be of class')
  expect_error(set_centers(padova, pavia), 'same region')
  expect_is(set_centers(pavia, milano), 'set_centers')
})

test_that("gets expected class", {
  padova <- center('Padova', 'Veneto')
  pavia  <- center('Pavia',  'Lombardia')
  milano <- center('Milano', 'Lombardia')

  expect_is(region(set_centers(padova)), 'region')
  expect_is(region(set_centers(pavia, milano)), 'region')
})

test_that("throws error on wrong input", {
  padova <- center('Padova', 'Veneto')
  expect_error(region(padova), 'set_centers')
  expect_error(region(set_centers(padova), 'a'), 'is not of clas')
  expect_error(region(set_centers(padova), -1), 'too low')
  expect_error(region(set_centers(padova),  2), 'too high')
})


test_that('print works properly', {
  padova <- center('Padova', 'Veneto', 10, 0)
  veneto <- region(set_centers(padova))
  expect_output(print(veneto), 'Acceptance')
  expect_is(print(veneto), 'region')

  milano    <- center('Milano', 'Lombardia')
  lombardia <- region(set_centers(milano), default_p = 0.2)
  expect_output(print(lombardia), '(at least one)')
})


test_that('pass correct default', {
  pavia  <- center('Pavia',  'Lombardia', p_accept = 0.9)
  milano <- center('Milano', 'Lombardia')
  lombardia <- region(set_centers(pavia, milano), default_p = 0.3)

  expect_equal(get_p_accept(lombardia), at_least_one(c(0.9, 0.3)))
})

test_that('print works for set_center', {
  padova <- center('Padova', 'Veneto', 10, 0.8)
  veneto <- region(set_centers(padova))
  expect_output(print(get_centers(veneto)), 'Center')
  expect_is(print(get_centers(veneto)), 'set_centers')

})
