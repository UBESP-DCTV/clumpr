#' Acceptance matrices by macroregions' position
#'
#' In the case of the organ is offered to a macroarea, which are the
#' probability that a (macro)reagion i obtains (accepting...) it if it is in
#' position j?
#' @param state an object of class \code{state}
#'
#' @return a list of n matrix where n is the number of macroareas.
#' @export
get_p_accept_by_position <- function(state){

  # useful information
  macroareas    <- names(get_macroareas(state)) %>% purrr::set_names(.)

  p_offered <- get_macroregions(state) %>%
    purrr::map_dbl(get_offered) %>%
    `/`(get_offered(state))


  get_macroareas(state) %>%
    purrr::map(~{
      strip <- get_initial_strip(.x)
      prob  <- vector('list', length(strip))

      # la probabilità di accettare una eccedenza se in prima posizione è quella
      # originale moltiplicata per la probabilità che non sia proprio quel
      # centro ad aver prodotto l'eccedenza. Tale considerazione si propaga poi
      # automaticamente senza altri aggiustamenti alle posizioni successive
      #
      prob[[1]] <- .x %>%
        get_macroregions %>%
        `[`(strip) %>%
        purrr::map_dbl(get_p_accept) %>%
        `*`(1 - p_offered[strip])


      for (i in seq_along(strip)[-1]) {
      prob[[i]] <- prob[[i - 1]] *
        (1 - (
          .x %>%
            get_macroregions %>%
            `[`(wavethresh::guyrot(strip, i - 1)) %>%
            purrr::map_dbl(get_p_accept)
        ))
      }

      prob %>%
        unlist %>%
        matrix(nrow = length(strip)) %>%
        `row.names<-`(strip)
    })
}
