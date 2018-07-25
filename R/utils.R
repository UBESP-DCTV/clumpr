cat_line <- function(...) cat(..., "\n", sep = "")

neg_p <- function(p) {
  assertive::assert_all_are_proportions(p)
  1 - p
}

at_least_one <- function(...) {
  .dots <- list(...)
  .dots %>%
    purrr::walk(~ assertive::assert_all_are_proportions(.x))

  .dots <- unlist(.dots)
  1 - prod(neg_p(.dots))
}

unlist_1 <- function(x) {
    len_x  <- length(x)
    lens_x <- rep(1L, len_x)

    for (i in seq_len(len_x)) {
      if (rlang::is_list(x[[i]])) {
        lens_x[[i]] <- length(x[[i]])
      }
    }

    res <- vector('list', sum(lens_x))

    ends  <- cumsum(lens_x)
    starts <- ends - lens_x + 1L

    for (i in seq_along(lens_x)) {

      if (lens_x[[i]] == 1L) {
        res[starts[[i]]:ends[[i]]] <- x[i]
      } else {
        res[starts[[i]]:ends[[i]]] <- x[[i]]
      }
    }
    res
}


str_to_title_if_needed <- function(x) {
  purrr::map_if(x, x == stringr::str_to_lower(x), stringr::str_to_title) %>%
    unlist
}


# cat_centers <- function(region, sep = ', ') {
#   attr(region, 'centers') %>%
#   purrr::map_chr(crayon::blue) %>%
#   paste(collapse = sep)
# }
#
# cat_region <- function(macroregion, sep = '; ') {
#   attr(macroregion, 'regions') %>%
#   purrr::map_chr(crayon::blue) %>%
#   paste(collapse = sep)
#
# }

