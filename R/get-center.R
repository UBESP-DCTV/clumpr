#' @describeIn center wrapper function to access to the center's name.
#' @inheritParams get_center
#' @export
#' @examples
#' get_center(padova) # Padova
get_center.center <- function(x, ...) {
  x[[1L]]
}

#' @describeIn set_centers wrapper function to access to the centers' names.
#' @inheritParams get_center
#' @export
#' @examples
#' pavia  <- center('Pavia',  'Lombardia')
#' milano <- center('Milano', 'Lombardia')
#' get_center(set_centers(pavia, milano)) # Pavia, Milano
get_center.set_centers <- function(x, ...) {
  purrr::map_chr(x, get_center)
}

#' @describeIn center wrapper function to access to the detail "region".
#' @inheritParams get_region
#' @export
#' @examples
#' get_region(padova) # veneto
get_region.center <- function(x, ...) {
  attr(x, 'region')
}

#' @describeIn center wrapper function to access to the detail "p_accept".
#' @inheritParams get_p_accept
#' @export
#' @examples
#' get_p_accept(padova)
get_p_accept.center <- function(x, ...) {
  attr(x, 'p_accept')
}

#' @describeIn center wrapper function to access to the detail "offered".
#' @inheritParams get_offered
#' @export
#' @examples
#' get_offered(padova)
get_offered.center <- function(x, ...) {
  attr(x, 'offered')
}

#' @describeIn center wrapper function to access to the detail "state".
#' @inheritParams get_state
#' @export
#' @examples
#' get_state(padova) # italy
get_state.center <- function(x, ...) {
  attr(x, 'state')
}
