#' @describeIn get_p_except_for method to access to the detail
#'             "initial_strip".
#' @inheritParams get_initial_strip
#' @export
get_p_except_for.macroarea <- function(macroarea, offering_region) {

  assertive::assert_is_a_string(offering_region)
  offering_region <- tolower(offering_region)
  area_region <- get_all_region(macroarea)
  assertive::assert_is_subset(offering_region, area_region)

  other_reagion <- setdiff(area_region, offering_region)

  get_regions(macroarea)[other_reagion] %>%
    purrr::map_dbl(get_p_accept) %>%
    at_least_one()
}



#' @describeIn get_p_except_for method to access to the detail
#'             "initial_strip".
#' @inheritParams get_initial_strip
#' @export
get_p_except_for.default <- function(macroarea, offering_region) {
  stop(
    paste0(
      crayon::bold("macroarea"), " provided is of class ",
      crayon::red(class(macroarea)), ".\n",
      "It must be of class ", crayon::blue("macroarea")
    ),
    call. = FALSE
  )
}


