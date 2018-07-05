#' Final probability distribution of region in a macroregion
#'
#' @param mr an object of class \code{macroregion}
#' @param n number of stages to compute (default is 1)
#' @param state an object of class \code{state}
#'
#' @return a list of dataframes, each element of the list is a time (or an ID
#'         for every further organ), each dataframe report the final
#' @export
get_final_p_macroregion_by_time <- function(mr, n = 1L, state ) {

  get_p_position_by_time_nitp(mr, n, state) %>%
    purrr::map(function(stage) {
      (stage * get_p_accept_by_position_nitp(mr, state)) %>%
        rowSums() %>%
        dplyr::as_data_frame() %>%
        dplyr::mutate(
          region = get_initial_strip(mr)
        ) %>%
        dplyr::group_by(region) %>%
        dplyr::summarise_all(sum)
    })
}
