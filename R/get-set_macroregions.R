#' @describeIn set_macroregions wrapper function to access to the all the
#'             names of reagions appear in the `set_macroregion`,
#'             recursively.
#' @inheritParams get_all_region
#' @export
#' @examples
#' pavia  <- center('Pavia',  'Lombardia')
#' milano <- center('Milano', 'Lombardia')
#' lombardia <- region(set_centers(pavia, milano))
#'
#' padova <- center('Padova', 'Veneto', 8, 0.7)
#' veneto <- region(set_centers(padova))
#'
#' nitp <- macroregion('NITp', set_regions(lombardia, veneto))
#'
#' torino   <- center('Torino', 'Piemonte', 7, 0.6)
#' piemonte <- region(set_centers(torino))
#'
#' get_all_region(set_macroregions(piemonte, nitp))
get_all_region.set_macroregions <- function(x, ...) {
    purrr::map(x, get_all_region) %>%
    unlist %>%
    unname
}

#' @describeIn set_macroregions wrapper function to access to the all the
#'             names of macroregions (regions included).
#' @inheritParams get_all_region
#' @export
#' @examples
#' get_all_macroregion(set_macroregions(piemonte, nitp))
get_all_macroregion.set_macroregions <- function(x, ...) {
  names(x)
}
