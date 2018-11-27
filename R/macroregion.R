#' Constructor for object of class \code{macroregion}
#'
#' The function \code{\link{macroregion}()} is the constructor function for
#' the homonymous class.
#'
#' @note exclude a \code{\link{region}} from the \code{base_strip} is the
#'       same as include a \code{\link{region}} with zero
#'       \code{get_p_accept} parameter. Anyway, in such a case, it is
#'       always preferred to set \code{get_p_accept} to zero for those
#'       \code{\link{region}} (i.e. for all their \code{\link{center}}s) and
#'       also exclude them from the strip. Note also that if a region is
#'       included in a macroregion with a zero \code{get_p_accept} parameter
#'       and/or not in the strip, it will provide anyway its surplus organ
#'       with a priority for the \code{\link{macroregion}}.
#'
#'
#' @param name [chr] the name of the macroregion
#' @param regions [lst] a list of objects of class \code{\link{regions}},
#'        passed by \code{\link{set_regions}()}. Note: all the region in a
#'        macroregion must be of the same state.
#' @param initial_strip [chr] e sequence of \code{\link{region}}s' names in the
#'        \code{\link{macroregion}} representing the order in which organs
#'        started to be distributed in the \code{\link{macroregion}} at the
#'        beginning of the period. All the listed \code{\link{region}}s have
#'        to be included also in the \code{regions} field. Repetition are
#'        admitted as well as exculsions.
#'
#' @return An object of class \code{\link{macroregion}}.
#' @export
#'
#' @examples
#' pavia   <- center('Pavia',   'Lombardia', offered = 5, p_accept = 0.6)
#' bergamo <- center('Bergamo', 'Lombardia', 8)
#' milano  <- center('Milano',  'Lombardia', 10, 0.8)
#' lombardia <- region(set_centers(pavia, bergamo, milano), default_p = 0.7)
#'
#' padova <- center('Padova', 'Veneto', 8, 0.7)
#' veneto <- region(set_centers(padova))
#'
#' nitp <- macroregion('NITp',
#'   regions = set_regions(lombardia, veneto),
#'   initial_strip = c('lombardia', 'lombardia', 'veneto')
#' )
macroregion <- function(name,
                        regions,
                        initial_strip = purrr::map_chr(regions, 'region')
  ) {

  # input check ---------------------------------------------------------

  ## name
  assertive::assert_is_inherited_from(name, 'character')
  assertive::assert_is_a_non_missing_nor_empty_string(name)

  ## regions
  if (!inherits(regions, 'set_regions')) stop(
    paste0('Argument ', crayon::silver("regions"), ' must be provide using ',
        crayon::green('set_regions()'), ', and it isn\'t.\n\n',

      'Hint : To set `region_1` and `region_2` use ',
        crayon::silver("set_regions(region_1, region_2)"), '.\n',
      '       (Note: all the region must be of the same states.)'
    ),
    call. = FALSE
  )

  # constructor ----------------------------------------------------------

  p_accept_regions <- purrr::map_dbl(regions, get_p_accept)
  total_offered    <- purrr::map_dbl(regions, get_offered)

  structure(.Data = c(macroregion = name),
    regions         = regions,
    acceptance_rate = at_least_one(p_accept_regions),
    offered         = sum(total_offered),
    initial_strip   = initial_strip,
    current_strip   = initial_strip,
    current_time    = 0L,
    state           = get_state(regions[[1L]]),
    class           = 'macroregion'
  )
}


#' List of regions creator
#'
#' Function used to prepare and check list of regions to pass to
#' \code{regions} as needed to create \code{\link{macroregion}} objects.
#'
#' @param ... a set of \code{\link{region}}s
#'
#' @return The list of \code{\link{region}}s provided.
#' @export
#'
#' @examples
#' pavia  <- center('Pavia',  'Lombardia')
#' milano <- center('Milano', 'Lombardia')
#' lombardia <- region(set_centers(pavia, milano))
#'
#' padova <- center('Padova', 'Veneto', 8, 0.7)
#' veneto <- region(set_centers(padova))
#'
#' set_regions(lombardia, veneto)
set_regions <- function(...) {
  .dots <- list(...)

  if (length(.dots) == 1L & inherits(.dots[[1]][[1]], 'region')) {
    .dots <- .dots[[1]]
  }


  if (!all(purrr::map_lgl(.dots, inherits, 'region'))) stop(
    paste0('All ', crayon::silver("regions"), ' must be of class ',
        crayon::green('region'), ', and they aren\'t.\n\n',

      '       Have you create all of them using ',
        crayon::silver('region()'), '?'
    ),
    call. = FALSE
  )

  regs <- purrr::map_chr(.dots, 'region')
  if (length(regs) != length(unique(regs))) stop(
    paste0(crayon::silver("Regions"), ' must be different',
        ' and they aren\'t.\n\n',

        '       Regions provided are: ',
        paste(unique(regs),
              paste0(
                '(#',
                purrr::map_chr(table(regs),
                  ~ if (. == 1) crayon::green(.) else crayon::red(.)
                ),
                ')'
              ),
              collapse = ', '
        ),
        '.'
    ),
    call. = FALSE
  )

  sts <- purrr::map_chr(.dots, get_state) %>% unique
  if (length(sts) != 1L) stop(
    paste0('All ', crayon::silver("regions"), ' must be in the same state',
        ' and they aren\'t.\n\n',

        '       States of the regions provided are: ',
        paste(crayon::red(sts), collapse = ', '), '.'
    ),
    call. = FALSE
  )

  structure(
    setNames(.dots,
      purrr::map_chr(.dots, 'region') %>% stringr::str_to_lower()
    ),
    class = 'set_regions'
  )
}


#' @inheritParams base::print
#' @describeIn set_regions nice (and coloured, if supported) print method.
#'
#' @export
print.set_regions <- function(x, ...) {
  purrr::walk(x, ~ {print(.); cat_line()})
  invisible(x)
}



#' @inheritParams base::print
#' @describeIn macroregion nice (and coloured, if supported) print method.
#'
#' @export
print.macroregion <- function(x, ...) {
  cat_line('    ',
      crayon::bold('Macroregion : '),
      crayon::blue(str_to_title_if_needed(x[[1]])),
      ' (', stringr::str_to_title(get_state(x)), ')'
  )

  stored_regions <- get_regions(x)
  cat_line('    ',
      crayon::bold('Regions     : '),
      purrr::map_chr(stored_regions, 'region') %>%
        stringr::str_to_title() %>%
        crayon::blue() %>%
        paste(collapse = '; '),
      ' (#', length(stored_regions), ')'
  )

  stored_centers <- get_centers(x)
  cat_line('    ',
      crayon::bold('Centers     : '),
      purrr::map(stored_centers,
        ~ purrr::map_chr(.x, 'center')
      ) %>%
        purrr::map(stringr::str_to_title) %>%
        purrr::map(crayon::blue) %>%
        purrr::map(paste, collapse = ', ') %>%
        unlist %>%
        paste(collapse = '; '),
      ' (#', length(unlist(stored_centers, recursive = FALSE)), ')'
  )

  acc_p <- get_p_accept(x)
  if (acc_p == 0) {
    cat_line('    ',
      crayon::bold('Acceptance rate : '), crayon::red(acc_p)
    )
  } else {
    cat_line('    ',
      crayon::bold('Acceptance rate : '), crayon::green(acc_p),
      ' (at least one center in some region)'
    )
  }

  cat_line('    ',
      crayon::bold('Offered organs  : '),
      crayon::blue(get_offered(x)),
    ' (from every the centers of every region)'
  )

  cat_line('    ',
      crayon::bold('Initail strip   : '),
      crayon::blue(get_initial_strip(x) %>% stringr::str_to_title()) %>%
        paste(collapse = ' --> ')
  )

  cat_line('    ',
      crayon::bold('Current strip   : '),
      crayon::blue(get_current_strip(x) %>% stringr::str_to_title()) %>%
        paste(collapse = ' --> ')
  )

  cat_line('    ',
      crayon::bold('Time period     : '),
      crayon::blue(get_time(x))
  )


  invisible(x)
}
