#' @describeIn macroarea wrapper function to access to the 'macroregion'
#'             detail.
#' @inheritParams get_macroregions
#' @export
#' @examples
#' pavia   <- center('Pavia',   'Lombardia', offered = 5, p_accept = 0.6)
#' pavia
#'
#' bergamo <- center('Bergamo', 'Lombardia', 8)
#' milano  <- center('Milano',  'Lombardia', 10, 0.8)
#'
#' lombardia <- region(set_centers(pavia, bergamo, milano), default_p = 0.7)
#' lombardia
#'
#' padova <- center('Padova', 'Veneto', 8, 0.7)
#' veneto <- region(set_centers(padova))
#'
#' nitp <- macroregion('NITp', regions = set_regions(lombardia, veneto),
#'   initial_strip = c('lombardia', 'lombardia', 'veneto')
#' )
#' nitp
#'
#' torino   <- center('Torino', 'Piemonte', 7, 0.6)
#' piemonte <- region(set_centers(torino))
#'
#' nord <- macroarea('Macroarea Nord',
#'   macroregions = set_macroregions(piemonte, nitp)
#' )
#' get_macroregions(nord)
get_macroregions.macroarea <- function(x, ...) {
  attr(x, 'macroregions')
}

#' @describeIn macroarea wrapper function to access to all the reagions in
#'             macroarea
#' @inheritParams get_regions
#' @export
#' @examples
#' get_regions(nord)
get_regions.macroarea <- function(x, ...) {
  get_macroregions(x) %>%
    purrr::map(get_regions) %>%
    unlist_1 %>%
    do.call(what = set_regions)
}

#' @describeIn macroarea wrapper function to access to the all the
#'             names of reagions appear in the `set_macroregion`,
#'             recursively.
#' @inheritParams get_all_region
#' @export
#' @examples
#' get_all_region(nord)
get_all_region.macroarea <- function(x, ...) {
    get_macroregions(x) %>%
    get_all_region
}

#' @describeIn macroarea wrapper function to access to the all the names
#'             of macroregions (regions included).
#' @inheritParams get_all_macroregion
#' @export
#' @examples
#' get_all_macroregion(nord)
get_all_macroregion.macroarea <- function(x, ...) {
  get_macroregions(x) %>%
    get_all_macroregion()
}

#' @describeIn macroarea wrapper function to access to the name of the
#'             state
#' @inheritParams get_state
#' @export
#' @examples
#' get_state(nord) # "italy"
get_state.macroarea <- function(x, ...) {
  attr(x, 'state')
}


#' @describeIn macroarea wrapper function to access to all the centers of
#'             a macroarea
#' @inheritParams get_centers
#' @export
#' @examples
#' get_centers(nord)
get_centers.macroarea <- function(x, ...) {
  get_regions(x) %>%
    purrr::map(get_centers)
}



#' @describeIn macroarea compute the probability that at least one center
#'             in any region of the macroarea accept an offered organ.
#' @inheritParams get_p_accept
#' @export
#' @examples
#' get_p_accept(nord)
get_p_accept.macroarea <- function(x, ...) {
  attr(x, 'acceptance_rate')
}

#' @describeIn macroarea wrapper function to access to the detail
#'             "offered".
#' @inheritParams get_offered
#' @export
#' @examples
#' get_offered(nord)
get_offered.macroarea <- function(x, ...) {
  attr(x, 'offered')
}

#' @describeIn macroarea wrapper function to access to the detail
#'             "initial_strip".
#' @inheritParams get_initial_strip
#' @export
#' @examples
#' get_initial_strip(nord)
get_initial_strip.macroarea <- function(x, ...) {
  attr(x, 'initial_strip')
}

#' @describeIn macroarea wrapper function to access to the detail
#'             "current_strip".
#' @inheritParams get_current_strip
#' @export
#' @examples
#' get_current_strip(nord)
get_current_strip.macroarea <- function(x, ...) {
  attr(x, 'current_strip')
}


#' @describeIn macroarea wrapper function to access to the detail
#'             "initial_time".
#' @inheritParams get_time
#' @export
#' @examples
#' get_initial_time(nord)
get_initial_time.macroarea <- function(x, ...) {
  attr(x, 'initial_time')
}

#' @describeIn macroarea wrapper function to access to the detail
#'             "current_time".
#' @inheritParams get_time
#' @export
#' @examples
#' get_time(nord)
get_time.macroarea <- function(x, ...) {
  attr(x, 'current_time')
}

#' @describeIn macroarea wrapper function to access to the detail
#'             "final_time".
#' @inheritParams get_time
#' @export
#' @examples
#' get_final_time(nord)
get_final_time.macroarea <- function(x, ...) {
  attr(x, 'final_time')
}
