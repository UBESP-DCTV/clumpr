context("test-state.R")

pavia     <- center('Pavia',   'Lombardia', offered = 5, p_accept = 0.6)
bergamo   <- center('Bergamo', 'Lombardia', 8)
milano    <- center('Milano',  'Lombardia', 10, 0.8)
lombardia <- region(set_centers(pavia, bergamo, milano), default_p = 0.7)

padova <- center('Padova', 'Veneto', 8, 0.7)
veneto <- region(set_centers(padova))

nitp <- macroregion('NITp', regions = set_regions(lombardia, veneto),
  initial_strip = c('lombardia', 'lombardia', 'veneto')
)

torino   <- center('Torino', 'Piemonte', 7, 0.6)
piemonte <- region(set_centers(torino))

nord <- macroarea('Nord',
  macroregions = set_macroregions(piemonte, nitp)
)

roma  <- center('Roma', 'Lazio', 10, 0.9)
lazio <- region(set_centers(roma))

sud   <- macroarea('Sud', macroregions = set_macroregions(lazio))


test_that("set_macroareas input", {
  expect_error(set_macroareas(nord, 1), 'must be of class')
  expect_error(set_macroareas(nord, nord), 'must be different')
  expect_error(set_macroareas(nord, lombardia), 'must be of class')
  expect_is(set_macroareas(nord, sud), 'set_macroareas')
})

test_that("gets expected class", {
  expect_is(state('Italy', set_macroareas(nord, sud)), 'state')
  expect_is(state('Italy', set_macroareas(nord)), 'state')
  expect_is(state('Italy', set_macroareas(sud)), 'state')
})

test_that("throws error on wrong input", {
  expect_error(state(set_macroareas(nord)),
    "not inherit from the class character"
  )
  expect_error(state(1, set_macroareas(nord)),
    "not inherit from the class character"
  )
  expect_error(state(nord, set_macroareas(nord)),
    "not inherit from the class character"
  )
  expect_error(state('Italia', set_macroareas(nord)),
     "must be the same as the name"
  )
})


test_that("correct known output", {
  italy <- state('Italy', set_macroareas(nord, sud))

  expect_is(get_regions(italy), 'set_regions')
  expect_equal(get_regions(italy), set_regions(piemonte, lombardia, veneto, lazio))
  expect_equivalent(get_initial_strip(italy), c('nord', 'sud'))
  expect_equivalent(get_current_strip(italy), c('nord', 'sud'))
  expect_equal(get_time(italy), 0L)
})
