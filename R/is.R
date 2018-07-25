#' Test for inherithage on `macroregion`
#'
#' This function aimed to check if an object is of class `macroregion`
#'
#' @param x an object used to select a method.
#'
#' @return TRUE if the object \code{x} inherits from `macrorigion`, FASLE
#'         otherwise.
#'
#' @export
is_macroregion <- function(x) {
  inherits(x, 'macroregion')
}
