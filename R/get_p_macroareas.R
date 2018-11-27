#' Macroareas' probability of usage
#'
#' this function comput the probabilities for macroareas to use a "next" organ
#' based on the portion of organs offered and the probability to accept them
#' from the centers. The probability that the organ will be lost is also
#' returned.
#'
#' @param state an object of class \code{state}.
#'
#' @return a \code{data.frame} with two columns, the first reporting the
#'         macroareas' name or "lost", and the second reporting the probability
#'         that a "next" organ will be effectively used by that macroarea (or
#'         will be lost).
#' @export
#'
#' @examples
#' \dontrun{
#'   get_p_macroareas(italy)
#' }
get_p_macroareas <- function(state) {

  if (length(get_macroareas(state)) != 2) {
    stop('At the moment the package works only with two macroareas.')
  }

  # useful information
  total_offered <- get_offered(state)
  macroareas    <- names(get_macroareas(state)) %>% purrr::set_names(.)


  macros <- get_macroareas(state)
  purrr::map_df(macroareas, ~{

    (get_offered(macros[[.]])  / total_offered) *
      get_p_accept(macros[[.]]) +

    (get_offered(macros[macroareas != .][[1]]) / total_offered) *
      (1 - get_p_accept(macros[macroareas != .][[1]])) *
      get_p_accept(macros[[.]])

  }) %>%
    dplyr::mutate(
      lost = 1 - sum(.)
    ) %>%
    tidyr::gather('macroarea', 'prob')
}

