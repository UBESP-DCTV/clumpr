context("test-center.R")

test_that("gets expected class", {
  expect_is(center('Padova', 'Veneto', 10, 0.8), 'center')
  expect_is(center('Padova', 'veneto', 10, 0.8), 'center')
})

test_that('Uppercase nations are good input', {
  expect_is(center('Padova', 'Veneto', 10, 0.8, 'Italy'), 'center')
})

test_that("throws error on wrong input", {
  expect_error(center(1,  'Veneto', 10, 0.8), 'class \'character\'')
  expect_error(center(NA, 'Veneto', 10, 0.8), 'class \'character\'')
  expect_error(center(c('Padova', 'Mestre'), 'Veneto', 10, 0.8),
    'has length'
  )
  expect_error(center('Udine', 'lienz', 10, 0.8), 'must be a region of')
  expect_error(center('Padova', 'Veneto', 10, 0.8, 'England'),
    'must be a state'
  )

  expect_error(center('Padova',  1, 10, 0.8), 'class \'character\'')
  expect_error(center('Padova', NA, 10, 0.8), 'class \'character\'')
  expect_error(center('Padova', c('Veneto', 'Friuli'), 10, 0.8),
    'has length'
  )

  expect_error(center('Padova', 'Veneto', 0.5, 0.8), 'must be an integer')
  expect_error(center('Padova', 'Veneto', -4, 0.8), 'too low')
  expect_error(center('Padova', 'Veneto', c(1, 2), 0.8), 'has length')
  expect_error(center('Padova', 'Veneto', 'a', 0.8), 'class \'numeric\'')
  expect_error(center('Padova', 'Veneto', NA_real_, 0.8), 'missing value')
  expect_is(get_offered(center('Padova', 'Veneto', 10, 0.8)), 'integer')

  expect_error(center('Padova', 'Veneto', 10, 'a'), 'class \'numeric\'')
  expect_error(center('Padova', 'Veneto', 10, -0.5), 'too low')
  expect_error(center('Padova', 'Veneto', 10, 5), 'too high')
  expect_error(center('Padova', 'Veneto', 10, c(0.2, 0.3)), 'has length 2')
  expect_is(center('Padova', 'Veneto', 10, NA_real_), 'center')
})

test_that('print works properly', {
  center_test <- center('Padova', 'Veneto', 10, 0.8)
  center_p0 <- center('Padova', 'Veneto', 10, 0)
  center_null <- center('Padova', 'Veneto', 10)
  expect_output(print(center_test), 'Center')
  expect_output(print(center_p0), 'Center')
  expect_output(print(center_null), 'inherit')
  expect_is(print(center_test), 'center')
})
