#' positions' probability by stage
#'
#' This function comput the matrices of positions' probability for each stage
#' requested.
#'
#' @param state an object of class \code{state}
#' @param n number of stages to compute (default is 1)
#'
#' @return a list of length n in which element is stored a list containing the
#'         macroareas' positions' probability matrix
#' @export
get_p_position_by_time <- function(state, n = 1L) {
  positions <- vector('list', n) %>%
    purrr::set_names(paste0('organ #', seq_len(n)))

  positions[[1]] <- get_macroareas(state) %>%
    purrr::map(~{
      strip <- get_initial_strip(.x)

      diag(length(strip)) %>%
        `row.names<-`(strip)
    })

  for (m in seq_len(n)[-1]) {
    positions[[m]] <- get_macroareas(state) %>%
      purrr::map(~{
        strip <- get_initial_strip(.x)

        base_probs <- .x %>%
          get_macroregions %>%
          `[`(strip) %>%
          purrr::map_dbl(get_p_accept)

        transition_position(positions[[m - 1]][[.x %>% tolower()]], base_probs) %>%
          `row.names<-`(strip)
      })
  }
  positions
}


