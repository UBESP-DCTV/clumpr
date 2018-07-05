#' Acceptance matrices for macroregions' position
#'
#' In the case of the organ is offered to a macroregion, which are the
#' probability its reagions to obtains (accepting...) it if it is in
#' position j?
#'
#' @param macroregion an object of class \code{macroregion}
#' @param state an object of class \code{stae}
#'
#' @return a matrix
#' @export
get_p_accept_by_position_nitp <- function(macroregion, state) {

  strip_macroregion <- get_initial_strip(macroregion)

  p_offered_macroregion <- get_regions(macroregion)[strip_macroregion] %>%
    purrr::map_dbl(get_offered) %>%
    `/`(get_offered(state))

  p_accept_macroregion <- get_regions(macroregion)[strip_macroregion] %>%
    purrr::map_dbl(get_p_accept)

  last_appear <- purrr::map_lgl(seq_along(strip_macroregion), ~{
    !strip_macroregion[[.x]] %in% strip_macroregion[-(1:.x)]
  })

  prob_macroregion  <- vector('list', length(strip_macroregion))

  # La probabilità di accettare un eccedenza quando in prima (n-esima) posizione
  # è condizionata dal fatto che tale eccedenza non sia prodotta dal medesimo
  # centro.
  #
  prob_macroregion[[1]] <- (p_accept_macroregion * (1 - p_offered_macroregion)) %>%
    purrr::set_names(strip_macroregion)

  # la probabilità di  accettarlo in seconda posizione è condizionata (oltre a
  # che non sia stato prodotto dal centro stesso) dal fatto che i centri
  # precedenti abbiano rifiutato (ovviamente se sono centri duplicati e quello
  # in questione non è "l'ultimo" segue che il rifiuto da parte sua è
  # automatico)
  #
  for (i in seq_along(strip_macroregion)[-1]) {
    prob_macroregion[[i]] <- prob_macroregion[[i - 1]] *
      (1 -
         (wavethresh::guyrot(p_accept_macroregion * last_appear,  i - 1))
      )
  }

  for (i in seq_along(strip_macroregion)[-1]) {
    for (j in seq_along(strip_macroregion)) {
      prob_macroregion[[i]][[j]] <- prob_macroregion[[i]][[j]] *
        !strip_macroregion[[j]] %in% wavethresh::guyrot(strip_macroregion, i - j)[seq_len(i)-1]
    }
  }

  prob_macroregion %>%
    unlist %>%
    matrix(nrow = length(strip_macroregion)) %>%
    `row.names<-`(strip_macroregion)
}
