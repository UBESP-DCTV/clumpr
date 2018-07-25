#' Constructor for object of class \code{center}
#'
#' The function \code{\link{center}()} is the constructor function for the
#' homonymous class.
#'
#'
#' @param name [chr] the name of the (city) transplant center.
#' @param region [chr] The name of the (Italian) region in which the center
#'   inherits (see \code{\link{regions}}).
#' @param p_accept [dbl] a single number between 0 and 1 (possibly included)
#'   representing the probability to accept an offered organ. Default is
#'   \code{NULL} meaning that the probability should be considered at region
#'   level and not at center level.
#' @param offered [dbl] a single number between 0 and 1 (possibly included)
#'   representing the number of organ offered from the center in the
#'   previous period of reference. Default is 0.
#' @param state [chr] a name of available state, i.e. included in
#'   \code{\link{names}(\link{regions})} (note: both lowercase and Titlecase are
#'   fine for write states, i.e. "Italy" works as well as "italy").
#'
#' @return An object of class \code{\link{center}}.
#' @export
#'
#' @examples
#' center('Padova', 'Veneto', 10, 0.8)
#' center('Padova', 'Veneto', 10, 0)
#' center('Padova', 'Veneto', 10)
#' center('Padova', 'Veneto', 10, 0.8, 'Italy')
#'
#' padova <- center('Padova', 'Veneto')
center <- function(name, region, offered = 0L, p_accept = NA,
                   state = 'italy'
) {

  # input check ---------------------------------------------------------

  ## name
  assertive::assert_is_a_non_missing_nor_empty_string(name)

  ## state
  assertive::assert_is_a_non_missing_nor_empty_string(state)
  ### region in state
  if (!stringr::str_to_lower(state) %in% names(clumpr::regions)) stop(
    paste0('Argument ', crayon::silver("state"), ' must be a state in ',
        crayon::green("data(regions, package = 'clumpr')"), '.\n',
      '       It is "', crayon::red(state), '" which isn\'t.\n\n',

      '       Do you set variable ', crayon::silver("state"),
        ' correctly?\n\n',

      'Hint : Try to check ',
        crayon::silver("data(regions, package = 'clumpr')"), ' for\n',
      '       available states and corresponding regions.'
    ),
    call. = FALSE
  )

  ## region
  assertive::assert_is_a_non_missing_nor_empty_string(region)
  ### region in state
  if (!stringr::str_to_lower(region) %in%
       clumpr::regions[[stringr::str_to_lower(state)]]) stop(
    paste0('Argument ', crayon::silver("region"), ' must be a region of ',
        crayon::green(stringr::str_to_title(state)), '.\n',
      '       It is "', crayon::red(region), '" which isn\'t.\n\n',

      '       Do you set variable ', crayon::silver("state"),
        ' correctly?\n\n',

      'Hint : Try to check ',
        crayon::silver("data(regions, package = 'clumpr')"), ' for\n',
      '       available states and corresponding regions.'
    ),
    call. = FALSE
  )

  ## p_accept
  if (is.null(p_accept) || !is.na(p_accept)) {
    assertive::assert_is_a_number(p_accept)
    assertive::assert_all_are_proportions(p_accept)
  }

  ## offered
  assertive::assert_is_a_number(offered)
  if (offered != as.integer(offered)) stop(
    glue('variable `offered` must be an integer, it is: {offered}.'),
    call. = FALSE
  )
  assertive::assert_all_are_non_negative(offered)


  # constructor ----------------------------------------------------------

  structure(.Data = c(center = name),
    region   = stringr::str_to_lower(region),
    offered  = as.integer(offered),
    p_accept = p_accept,
    state    = stringr::str_to_lower(state),
    class    = 'center'
  )
}











#' @inheritParams base::print
#' @describeIn center nice (and coloured, if supported) print method.
#'
#' @export
print.center <- function(x, ...) {
  cat_line('    ',
      crayon::bold('Center           : '), crayon::blue(x[['center']]),
      ' (', stringr::str_to_title(attr(x, 'region')), ' --- ',
            stringr::str_to_title(attr(x, 'state')),
      ')'
  )

  acc_p <- get_p_accept(x)
  if (is.na(acc_p)) {
    cat_line('    ',
      crayon::bold('Acceptance rate  : '), '<inherit from the region rate>'
    )
  } else if (acc_p == 0) {
    cat_line('    ',
      crayon::bold('Acceptance rate  : '), crayon::red(acc_p)
    )
  } else {
    cat_line('    ',
      crayon::bold('Acceptance rate  : '), crayon::green(acc_p)
    )
  }

  cat_line('    ',
      crayon::bold('Offered organs   : '), get_offered(x)
  )

  invisible(x)
}
