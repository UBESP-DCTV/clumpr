#' @describeIn set_macroareas wrapper function to access to the all the
#'             names of macroareas appear in the `set_macroarea`,
#'             recursively.
#' @inheritParams get_all_macroarea
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
#' nord <- macroarea('Macroarea Nord',
#'   macroregions = set_macroregions(piemonte, nitp)
#' )
#'
#' roma   <- center('Roma', 'Lazio', 10, 0.9)
#' lazio  <- region(set_centers(roma))
#'
#' sud <- macroarea('Macroarea Sud',
#'   macroregions = set_macroregions(lazio)
#' )
#'
#' get_all_macroarea(set_macroareas(nord, sud))
get_all_macroarea.set_macroareas <- function(x, ...) {
  names(x)
}
