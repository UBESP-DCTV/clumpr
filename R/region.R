#' Constructor for object of class \code{region}
#'
#' The function \code{\link{region}()} is the constructor function for the
#' homonymous class.
#'
#'
#' @param centers [lst] a list of objects of class \code{\link{center}},
#'        passed by \code{\link{set_centers}()}. Note: all the center in a
#'        region must be of the same region and state.
#' @param default_p [dbl] a single number between 0 and 1 (possibly
#'   included) representing the default probability for the centers in the
#'   region (if not explicitelly specified in the \code{\link{center}}
#'   properties) to accept an offered organ. Default is 1, which means that
#'   if offered, an organ is always accepted.
#'
#' @return An object of class \code{\link{region}}.
#' @export
#'
#' @examples
#' pavia   <- center('Pavia',   'Lombardia', 10, 0.8)
#' bergamo <- center('Bergamo', 'Lombardia', 10, 0.5)
#' milano  <- center('Milano',  'Lombardia', 10)
#'
#' lombardia <- region(set_centers(pavia, bergamo, milano), default_p = 0.2)
#'
#' pavia
#' bergamo
#' milano
#'
#' lombardia
region <- function(centers, default_p = 1) {

  # input check ---------------------------------------------------------

  ## centers
  if (!inherits(centers, 'set_centers')) stop(
    paste0('Argument ', crayon::silver("centers"), ' must be provide using ',
        crayon::green('set_centers()'), ', and it isn\'t.\n\n',

      'Hint : To set `center_1` and `center_2` use ',
        crayon::silver("set_centers(center_1, center_2)"), '.\n',
      '       (Note: all the center must be of the same region and states.)'
    ),
    call. = FALSE
  )

  ## p_defaul
  if (!is.null(default_p)) {
    assertive::assert_is_a_number(default_p)
    assertive::assert_all_are_proportions(default_p)
  }


  # constructor ----------------------------------------------------------

  p_accept_centers <- purrr::map_dbl(centers, get_p_accept)
  p_accept_centers[is.na(p_accept_centers)] <- default_p
  offered <- centers %>%
    purrr::map_int(get_offered) %>%
    sum()

  structure(.Data = c(region = get_region(centers[[1L]])),
    centers         = centers,
    default_p       = default_p,
    acceptance_rate = at_least_one(p_accept_centers),
    offered         = offered,
    state           = get_state(centers[[1L]]),
    class           = 'region'
  )
}







#' List of centers creator
#'
#' Function used to prepare and check list of centers to pass to
#' \code{centers} as needed to create \code{\link{region}} objects.
#'
#' @param ... a set of \code{\link{center}}
#'
#' @return The list of \code{\link{center}}s provided.
#' @export
#'
#' @examples
#' pavia  <- center('Pavia',  'Lombardia')
#' milano <- center('Milano', 'Lombardia')
#'
#' set_centers(pavia, milano)
set_centers <- function(...) {
  .dots <- list(...)

  if (length(.dots) == 1L & inherits(.dots[[1]][[1]], 'center')) {
    .dots <- .dots[[1]]
  }

  if (!all(purrr::map_lgl(.dots, inherits, 'center'))) stop(
    paste0('All ', crayon::silver("centers"), ' must be of class ',
        crayon::green('center'), ', and they aren\'t.\n\n',

      '       Have you create all of them using ',
        crayon::silver('center()'), '?'
    ),
    call. = FALSE
  )

  cntrs <- purrr::map_chr(.dots, 'center')
  if (length(cntrs) != length(unique(cntrs))) stop(
    paste0(crayon::silver("Centers"), ' must be different',
        ' and they aren\'t.\n\n',

        '       Centers provided are: ',
        paste(unique(cntrs),
              paste0(
                '(#',
                purrr::map_chr(table(cntrs),
                  ~ if (. == 1) crayon::green(.) else crayon::red(.)
                ),
                ')'
              ),
              collapse = ', '), '.'
    ),
    call. = FALSE
  )



  regs <- purrr::map_chr(.dots, get_region) %>% unique
  if (length(regs) != 1L) stop(
    paste0('All ', crayon::silver("centers"), ' must be in the same region',
        ' and they aren\'t.\n\n',

        '       Regions of the centers provided are: ',
        paste(crayon::red(regs), collapse = ', '), '.'
    ),
    call. = FALSE
  )

  sts <- purrr::map_chr(.dots, get_state) %>% unique
  if (length(sts) != 1L) stop(
    paste0('All ', crayon::silver("centers"), ' must be in the same state',
        ' and they aren\'t.\n\n',

        '       States of the centers provided are: ',
        paste(crayon::red(sts), collapse = ', '), '.'
    ),
    call. = FALSE
  )


  structure(
    setNames(.dots,
      purrr::map_chr(.dots, 'center') %>% stringr::str_to_lower()
    ),
    class = 'set_centers'
  )
}



#' @inheritParams base::print
#' @describeIn set_centers nice (and coloured, if supported) print method.
#'
#' @export
print.set_centers <- function(x, ...) {
  purrr::walk(x, ~ {print(.); cat_line()})
  invisible(x)
}






#' @inheritParams base::print
#' @describeIn region nice (and coloured, if supported) print method.
#'
#' @export
print.region <- function(x, ...) {
  cat_line('    ',
      crayon::bold('Region  : '),
      crayon::blue(stringr::str_to_title(x[['region']])),
      ' (', stringr::str_to_title(get_state(x)), ')'
  )

  stored_centers <- get_centers(x)
  cat_line('    ',
      crayon::bold('Centers : '),
      purrr::map_chr(stored_centers, 'center') %>%
        stringr::str_to_title() %>%
        crayon::blue() %>%
        paste(collapse = ', '),
      ' (#', length(stored_centers), ')'
  )

  acc_p <- get_p_accept(x)
  if (acc_p == 0) {
    cat_line('    ',
      crayon::bold('Acceptance rate : '), crayon::red(acc_p)
    )
  } else {
    cat_line('    ',
      crayon::bold('Acceptance rate : '), crayon::green(acc_p),
      ' (at least one center)'
    )
  }

  cat_line('    ',
      crayon::bold('Offered organs  : '),
      crayon::blue(get_offered(x)),
    ' (from all the centers)'
  )

  invisible(x)
}
