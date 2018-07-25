#' @describeIn state wrapper function to access to the 'macroareas' detail.
#' @inheritParams get_macroareas
#' @export
#' @examples
#' pavia     <- center('Pavia',   'Lombardia', offered = 5, p_accept = 0.6)
#' bergamo   <- center('Bergamo', 'Lombardia', 8)
#' milano    <- center('Milano',  'Lombardia', 10, 0.8)
#' lombardia <- region(set_centers(pavia, bergamo, milano), default_p = 0.7)
#'
#' padova <- center('Padova', 'Veneto', 8, 0.7)
#' veneto <- region(set_centers(padova))
#'
#' nitp <- macroregion('NITp', regions = set_regions(lombardia, veneto),
#'                     initial_strip = c('lombardia', 'lombardia', 'veneto')
#' )
#'
#' torino   <- center('Torino', 'Piemonte', 7, 0.6)
#' piemonte <- region(set_centers(torino))
#'
#' nord <- macroarea('Macroarea Nord',
#'                   macroregions = set_macroregions(piemonte, nitp)
#' )
#'
#' roma  <- center('Roma', 'Lazio', 10, 0.9)
#' lazio <- region(set_centers(roma))
#' sud   <- macroarea('Macroarea Sud', macroregions = set_macroregions(lazio))
#'
#' italy <- state('Italy', set_macroareas(nord, sud))
#' get_macroareas(italy)
get_macroareas.state <- function(x, ...) {
  attr(x, 'macroareas')
}


#' @describeIn state wrapper function to access to all the macroreagions in
#'             a state
#' @inheritParams get_macroregions
#' @export
#' @examples
#' get_macroregions(italy)
get_macroregions.state <- function(x, ...) {
   get_macroareas(x) %>%
    purrr::map(get_macroregions) %>%
    unlist(recursive = FALSE) %>%
    do.call(what = set_macroregions)
}



#' @describeIn state wrapper function to access to all the reagions in
#'             state
#' @inheritParams get_regions
#' @export
#' @examples
#' get_regions(italy)
get_regions.state <- function(x, ...) {
  get_macroregions(x) %>%
    purrr::map(get_regions) %>%
    unlist_1 %>%
    do.call(what = set_regions)
}



#' @describeIn state wrapper function to access to the all the
#'             names of reagions appear in the `set_macroregion`,
#'             recursively.
#' @inheritParams get_all_region
#' @export
#' @examples
#' get_all_region(italy)
get_all_region.state <- function(x, ...) {
  get_macroregions(x) %>%
    get_all_region
}




#' @describeIn state wrapper function to access to the name of the
#'             state
#' @inheritParams get_state
#' @export
#' @examples
#' get_state(italy) # "italy"
get_state.state <- function(x, ...) {
  tolower(x[[1]])
}

#' @describeIn state wrapper function to access to all the centers of
#'             a state
#' @inheritParams get_centers
#' @export
#' @examples
#' get_centers(italy)
get_centers.state <- function(x, ...) {
  get_regions(x) %>%
    purrr::map(get_centers)
}





#' @describeIn state compute the probability that at least one center
#'             in any region of the state accept an offered organ.
#' @inheritParams get_p_accept
#' @export
#' @examples
#' get_p_accept(italy)
get_p_accept.state <- function(x, ...) {
  attr(x, 'acceptance_rate')
}

#' @describeIn state wrapper function to access to the detail
#'             "offered".
#' @inheritParams get_offered
#' @export
#' @examples
#' get_offered(italy)
get_offered.state <- function(x, ...) {
  attr(x, 'offered')
}

#' @describeIn state wrapper function to access to the detail
#'             "initial_strip".
#' @inheritParams get_initial_strip
#' @export
#' @examples
#' get_initial_strip(italy)
get_initial_strip.state <- function(x, ...) {
  attr(x, 'initial_strip')
}

#' @describeIn state wrapper function to access to the detail
#'             "current_strip".
#' @inheritParams get_current_strip
#' @export
#' @examples
#' get_current_strip(italy)
get_current_strip.state <- function(x, ...) {
  attr(x, 'current_strip')
}

#' @describeIn state wrapper function to access to the detail
#'             "current_time".
#' @inheritParams get_time
#' @export
#' @examples
#' get_time(italy)
get_time.state <- function(x, ...) {
  attr(x, 'current_time')
}
