context("test-macroarea.R")

pavia     <- center('Pavia',   'Lombardia', offered = 5, p_accept = 0.6)
bergamo   <- center('Bergamo', 'Lombardia', offered = 8)
milano    <- center('Milano',  'Lombardia', p_accept = 0.8)
lombardia <- region(set_centers(pavia, bergamo, milano), default_p = 0.7)

padova <- center('Padova', 'Veneto', 8, 0.7)
veneto <- region(set_centers(padova))

nitp <- macroregion('NITp', set_regions(lombardia, veneto))

torino   <- center('Torino', 'Piemonte', 7, 0.6)
piemonte <- region(set_centers(torino))

test_that("set_macroregions input", {
  expect_error(set_macroregions(lombardia, 1), 'must be of class')
  expect_error(set_macroregions(lombardia, lombardia), 'must be different')
  expect_error(set_macroregions(lombardia, nitp), 'must be different')
  expect_is(set_macroregions(lombardia, veneto), 'set_macroregions')
  expect_is(set_macroregions(piemonte, nitp), 'set_macroregions')
})

test_that("gets expected class", {
  expect_is(macroarea('test-2', set_macroregions(lombardia, veneto)),
    'macroarea'
  )
  expect_is(macroarea('test-1', set_macroregions(lombardia)), 'macroarea')
  expect_is(macroarea('test-2', set_macroregions(piemonte, nitp)),
    'macroarea'
  )
  expect_is(macroarea('test-1', set_macroregions(nitp)), 'macroarea')
})


test_that("throws error on wrong input", {
  expect_error(macroregion(set_macroregions(veneto)),
    "not inherit from the class character"
  )
  expect_error(macroregion(1, set_regions(veneto)),
    "not inherit from the class character"
  )
  expect_error(macroregion(veneto, set_regions(veneto)),
    "not inherit from the class character"
  )
})



test_that("correct known output", {
  nord <- macroarea('Nord', set_macroregions(piemonte, nitp))

  expect_is(get_regions(nord), 'set_regions')
  expect_equal(get_regions(nord), set_regions(piemonte, lombardia, veneto))
  expect_equivalent(get_initial_strip(nord), c('piemonte', 'nitp'))
  expect_equivalent(get_current_strip(nord), c('piemonte', 'nitp'))
  expect_equal(get_initial_time(nord), 0L)
  expect_equal(get_initial_time(nord), 0L)
  expect_equal(get_time(nord), 0L)
  expect_equal(get_final_time(nord), Inf)
})



test_that('print works for get_center', {
  nord <- macroarea('Nord', set_macroregions(piemonte, nitp))
  expect_output(print(get_centers(nord)), 'piemonte')
  expect_output(print(get_centers(nitp)), 'Pavia')
  expect_is(print(get_centers(nord)), 'list')
  expect_is(print(get_centers(nord))[[1]], 'set_centers')
  expect_is(print(get_centers(nord))[[2]], 'set_centers')
})

test_that('print works for set_macroregions', {
  expect_output(print(set_macroregions(piemonte, nitp)), 'Region')
  expect_output(print(set_macroregions(piemonte, nitp)), 'Macroregion')
  expect_is(print(set_macroregions(piemonte, nitp)), 'set_macroregions')
})

test_that('print works properly', {
  nord <- macroarea('Macroarea Nord', set_macroregions(piemonte, nitp))
  expect_output(print(nord), 'Macroarea')
  expect_output(print(nord), 'Macroregions')
  expect_output(print(nord), 'Acceptance')
  expect_output(print(nord), 'Offered')
  expect_output(print(nord), 'Time period')
  expect_output(print(nord), 'Lombardia')
  expect_is(print(nord), 'macroarea')

  test_1 <- center('uno', 'veneto', p_accept = 0) %>%
    set_centers() %>%
    region() %>% set_macroregions() %>%
    macroarea(name = 'macro_1')
  expect_output(print(test_1), 'rate : 0')

  test_area1 <- macroarea('Nord', set_macroregions(nitp))
  test_area2 <- macroarea('Nord', set_macroregions(piemonte))
  expect_is(print(test_area1), 'macroarea')
  expect_is(print(test_area2), 'macroarea')
})


test_that('pass correct default', {
  nord <- macroarea('Nord', set_macroregions(piemonte, nitp))
  expect_equivalent(get_initial_strip(nord),
    c('piemonte', 'nitp')
  )
  expect_equal(get_initial_time(nord), 0L)
  expect_equal(get_final_time(nord), Inf)
  expect_equal(get_all_macroregion(nord), c("piemonte", "nitp"))
})
