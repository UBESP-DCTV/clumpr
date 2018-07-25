#' @describeIn region compute the probability that at least one center in
#'             in the region accept an offered organ.
#' @inheritParams get_p_accept
#' @export
#' @examples
#' pavia   <- center('Pavia',   'Lombardia', 10, 0.8)
#' bergamo <- center('Bergamo', 'Lombardia', 10, 0.5)
#' milano  <- center('Milano',  'Lombardia', 10)
#'
#' lombardia <- region(set_centers(pavia, bergamo, milano), default_p = 0.2)
#' get_p_accept(lombardia)
get_p_accept.region <- function(x, ...) {
  attr(x, 'acceptance_rate')
}

#' @describeIn region wrapper function to access to the detail "offered".
#' @inheritParams get_offered
#' @export
#' @examples
#' get_offered(lombardia)
get_offered.region <- function(x, ...) {
  attr(x, 'offered')
}

#' @describeIn region wrapper function to access to the detail "default_p".
#' @inheritParams get_p_default
#' @export
#' @examples
#' get_p_default(lombardia)
get_p_default.region <- function(x, ...) {
  attr(x, 'default_p')
}

#' @describeIn region wrapper function to access to the detail "state".
#' @inheritParams get_state
#' @export
#' @examples
#' get_state(lombardia) # italy
get_state.region <- function(x, ...) {
  attr(x, 'state')
}

#' @describeIn region wrapper function to access to the detail "state".
#' @inheritParams get_state
#' @export
#' @examples
#' get_state(lombardia) # italy
get_region.region <- function(x, ...) {
  x[['region']]
}

#' @describeIn region wrapper function to access to the region itself.
#' @inheritParams get_regions
#' @export
#' @examples
#' get_regions(lombardia)
get_regions.region <- function(x, ...) {
  x
}

#' @describeIn region wrapper function to access to the detail "region".
#' @inheritParams get_all_region
#' @export
#' @examples
#' get_all_region(lombardia) # lombardia
get_all_region.region <- function(x, ...) {
  x[['region']]
}

#' @describeIn region wrapper function to access to the detail "centers".
#' @inheritParams get_centers
#' @export
#' @examples
#' get_centers(lombardia)
get_centers.region <- function(x, ...) {
  attr(x, 'centers')
}
