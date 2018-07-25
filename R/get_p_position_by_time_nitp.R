#' positions' probability by stage for a macroegion
#'
#' This function computes the matrices of positions' probability for each stage
#' requested.
#'
#' @param mr an object of class \code{macroregion}
#' @param n number of stages to compute (default is 1)
#' @param state an object of class \code{state}
#'
#' @return a list of length n in which element are stored the
#'         regions' positions' probability matrix
#' @export
get_p_position_by_time_nitp <- function(mr, n = 1, state) {


  strip_macroregion <- get_initial_strip(mr) %>% tolower()

  p_accept_macroregion <- get_regions(mr)[strip_macroregion] %>%
    purrr::map_dbl(get_p_accept)

  p_offered_macroregion <- get_offered(mr) / get_offered(state)

  positions_macroregion <- vector('list', n) %>%
    purrr::set_names(paste0('organ #', seq_len(n)))

  positions_macroregion[[1]] <- diag(length(strip_macroregion)) %>%
    `row.names<-`(strip_macroregion)


  # in the macroregion the strip is updated only if the organ come from the
  # outside of the macroregion and only if it is accepted by one of the
  # region into the macroregion itself. Hence, inside this loop we update
  # the position by considering the probability of accept given that the
  # organ is provided from outside.
  #
  for (m in seq_len(n)[-1]) {
    positions_macroregion[[m]] <- transition_position(
      positions_macroregion[[m - 1]],
      p_accept_macroregion * (1 - p_offered_macroregion)
    ) %>%
      `row.names<-`(strip_macroregion)
  }
  positions_macroregion
}
