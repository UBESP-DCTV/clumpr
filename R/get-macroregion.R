#' @describeIn macroregion function to access to all the centers in a
#'             macroregion.
#' @inheritParams get_centers
#' @export
#' @examples
#' pavia   <- center('Pavia',   'Lombardia', offered = 5, p_accept = 0.6)
#' bergamo <- center('Bergamo', 'Lombardia', 8)
#' milano  <- center('Milano',  'Lombardia', 10, 0.8)
#' lombardia <- region(set_centers(pavia, bergamo, milano), default_p = 0.7)
#'
#' padova <- center('Padova', 'Veneto', 8, 0.7)
#' veneto <- region(set_centers(padova))
#'
#' nitp <- macroregion('NITp',
#'   regions = set_regions(lombardia, veneto),
#'   initial_strip = c('lombardia', 'lombardia', 'veneto')
#' )
#' get_centers(nitp)
get_centers.macroregion <- function(x, ...) {
  attr(x, 'regions') %>% purrr::map(attr, 'centers')
}

#' @describeIn macroregion wrapper function to access to the detail
#'             "regions".
#' @inheritParams get_regions
#' @export
#' @examples
#' get_regions(nitp)
get_regions.macroregion <- function(x, ...) {
  attr(x, 'regions')
}

#' @describeIn macroregion wrapper function to access to the all the names
#'             of regions appear in the detail "regions".
#' @inheritParams get_all_region
#' @export
#' @examples
#' get_all_region(nitp)
get_all_region.macroregion <- function(x, ...) {
  get_regions(x) %>% purrr::map_chr('region')
}



#' @describeIn macroregion wrapper function to access to the name of the
#'             state
#' @inheritParams get_state
#' @export
#' @examples
#' get_state(nitp) # "italy"
get_state.macroregion <- function(x, ...) {
  attr(x, 'state')
}

#' @describeIn macroregion compute the probability that at least one center
#'             in any region of the macroregion accept an offered organ.
#' @inheritParams get_p_accept
#' @export
#' @examples
#' pavia   <- center('Pavia',   'Lombardia', offered = 5, p_accept = 0.6)
#' bergamo <- center('Bergamo', 'Lombardia', 8)
#' milano  <- center('Milano',  'Lombardia', 10, 0.8)
#' lombardia <- region(set_centers(pavia, bergamo, milano), default_p = 0.7)
#'
#' padova <- center('Padova', 'Veneto', 8, 0.7)
#' veneto <- region(set_centers(padova))
#'
#' nitp <- macroregion('NITp',
#'   regions = set_regions(lombardia, veneto),
#'   initial_strip = c('lombardia', 'lombardia', 'veneto')
#' )
#' get_p_accept(nitp)
get_p_accept.macroregion <- function(x, ...) {
  attr(x, 'acceptance_rate')
}

#' @describeIn macroregion wrapper function to access to the detail
#'             "offered".
#' @inheritParams get_offered
#' @export
#' @examples
#' get_offered(nitp)
get_offered.macroregion <- function(x, ...) {
  attr(x, 'offered')
}

#' @describeIn macroregion wrapper function to access to the detail
#'             "initial_strip".
#' @inheritParams get_initial_strip
#' @export
#' @examples
#' get_initial_strip(nitp)
get_initial_strip.macroregion <- function(x, ...) {
  attr(x, 'initial_strip')
}

#' @describeIn macroregion wrapper function to access to the detail
#'             "current_strip".
#' @inheritParams get_current_strip
#' @export
#' @examples
#' get_current_strip(nitp)
get_current_strip.macroregion <- function(x, ...) {
  attr(x, 'current_strip')
}


#' @describeIn macroregion wrapper function to access to the detail
#'             "time_period".
#' @inheritParams get_time
#' @export
#' @examples
#' get_time(nitp)
get_time.macroregion <- function(x, ...) {
  attr(x, 'current_time')
}
