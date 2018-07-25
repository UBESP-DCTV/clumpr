#' Constructor for object of class \code{macroarea}
#'
#' The function \code{\link{macroarea}()} is the constructor function for
#' the homonymous class.
#'
#' @note Even if improbable, a \code{\link{macroarea}} can be composed by a
#'       single \code{\link{macroregion}} which can be composed by a single
#'       \code{\link{region}} which can be composed by a single
#'       \code{\link{center}}
#'
#'
#' @param name [chr] the name of the macroarea
#' @param macroregions [lst] a list of objects of class \code{\link{region}}
#'        and/or \code{\link{macroregion}}, passed by
#'        \code{\link{set_macroregions}()}. Note: all the
#'        \code{\link{region}}s and \code{\link{macroregion}}s in a
#'        \code{\link{macroarea}} must be of the same state. Moreover, all
#'        \code{\link{region}}s (alone and into some
#'        \code{\link{macroregion}}) must be different.
#' @param initial_strip [chr] a sequence of \code{\link{macroregion}}s'
#'        names in the \code{\link{macroarea}} representing the order in
#'        which organs started to be distributed in the
#'        \code{\link{macroarea}} at the beginning of the period. All the
#'        listed \code{\link{region}}s and/or \code{\link{macroregion}}s
#'        have to be included also in the \code{macroregions} field.
#'        Exculsions are permitted but repetitions are not admitted.
#'        (default is the sequence in which \code{\link{region}}s and/or
#'        \code{\link{macroregion}}s has been included in the argument
#'        \code{macroregions})
#' @param initial_time [int] (default is `0`) the time in which the
#'        \code{\link{macroarea}} is defined start to be used
#' @param final_time [int] (default is `Inf`, i.e. forever) the time in
#'        which the macroarea finish to be used
#'
#'
#' @return An object of class \code{\link{macroarea}}.
#' @export
#'
#' @examples
#' pavia   <- center('Pavia',   'Lombardia', offered = 5, p_accept = 0.6)
#' pavia
#'
#' bergamo <- center('Bergamo', 'Lombardia', 8)
#' milano  <- center('Milano',  'Lombardia', 10, 0.8)
#'
#' lombardia <- region(set_centers(pavia, bergamo, milano), default_p = 0.7)
#' lombardia
#'
#' padova <- center('Padova', 'Veneto', 8, 0.7)
#' veneto <- region(set_centers(padova))
#'
#' nitp <- macroregion('NITp', regions = set_regions(lombardia, veneto),
#'   initial_strip = c('lombardia', 'lombardia', 'veneto')
#' )
#' nitp
#'
#' torino   <- center('Torino', 'Piemonte', 7, 0.6)
#' piemonte <- region(set_centers(torino))
#'
#' nord <- macroarea('Nord',
#'   macroregions = set_macroregions(piemonte, nitp)
#' )
#' nord
macroarea <- function(name, macroregions,
  initial_strip = get_all_macroregion(macroregions),
  initial_time = 0L, final_time = Inf
) {

  # input check ---------------------------------------------------------

  ## name
  assertive::assert_is_inherited_from(name, 'character')
  assertive::assert_is_a_non_missing_nor_empty_string(name)

  ## macroregions
  if (!inherits(macroregions, 'set_macroregions')) stop(
    paste0('Argument ', crayon::silver("macroregions"),
      ' must be provide using\n',
      '       ', crayon::green('set_macroregions()'),
      ', and it isn\'t.\n\n',

      'Hint : To set `region_1` and `macroregion_2` use\n',
      '       ',
      crayon::silver("set_macroregions(region_1, macroregion_2)"), '.\n',
      'Note : all the regions and/or macroregions must be of the\n',
      '       same states. No repetition is admitted in regions.'
    ),
    call. = FALSE
  )

  # constructor ----------------------------------------------------------

  p_accept_macro <- purrr::map_dbl(macroregions, get_p_accept)
  total_offered  <- purrr::map_dbl(macroregions, get_offered)

  structure(.Data = c(macroarea = name),
    macroregions    = macroregions,
    acceptance_rate = at_least_one(p_accept_macro),
    offered         = sum(total_offered),
    initial_strip   = initial_strip,
    current_strip   = initial_strip,
    initial_time    = initial_time,
    current_time    = 0L,
    final_time      = final_time,
    state           = get_state(macroregions[[1L]]),
    class           = 'macroarea'
  )
}



#' List of regions/macroregion creator
#'
#' Function used to prepare and check list of \code{\link{region}}s and/or
#' \code{\link{macroregion}}s to pass to \code{macroregions} as needed to
#' create \code{\link{macroarea}} objects.
#'
#' @param ... a set of \code{\link{region}}s and/or
#'            \code{\link{macroregion}}
#'
#' @return The list of \code{\link{region}}s and \code{\link{macroregion}}s
#'         provided.
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
#' nitp <- macroregion('NITp', set_regions(lombardia, veneto))
#'
#' torino   <- center('Torino', 'Piemonte', 7, 0.6)
#' piemonte <- region(set_centers(torino))
#'
#' set_macroregions(piemonte, nitp)
set_macroregions <- function(...) {
  .dots <- list(...)

  if (length(.dots) == 1L & inherits(.dots[[1]][[1]], c('region', 'macroregion'))) {
    .dots <- .dots[[1]]
  }

  if (!all(purrr::map_lgl(.dots, inherits, c('region', 'macroregion')))) {
    stop(
      paste0('All ', crayon::silver("macroregions"), ' must be of class ',
          crayon::green('region'), ' and/or ', crayon::green('macroregion'),
          ', and they aren\'t.\n\n',

        '       Have you create all of them using ',
          crayon::silver('region()'), ' and/or ',
          crayon::silver('macroregion()'), '?'
      ),
      call. = FALSE
    )
  }

  regs <- purrr::map(.dots, get_all_region) %>%
    unlist %>%
    unname

  if (length(regs) != length(unique(regs))) stop(
    paste0(crayon::silver("Regions"),
      ' (all inside or outside macroregion) must be different\n',
      '       and they aren\'t.\n\n',

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
      purrr::map_chr(.dots, 1L) %>% stringr::str_to_lower()
    ),
    class = 'set_macroregions'
  )
}











#' @inheritParams base::print
#' @describeIn set_macroregions nice (and coloured, if supported) print method.
#'
#' @export
print.set_macroregions <- function(x, ...) {
  purrr::walk(x, ~ {print(.); cat_line()})
  invisible(x)
}



#' @inheritParams base::print
#' @describeIn macroarea nice (and coloured, if supported) print method.
#'
#' @export
print.macroarea <- function(x, ...) {
  cat_line('    ',
      crayon::bold('Macroarea    : '),
      crayon::blue(stringr::str_to_title(x[[1]])),
      ' (', str_to_title_if_needed(get_state(x)), ')'
  )

  stored_macroregions <- get_macroregions(x)
  cat_line('    ',
      crayon::bold('Macroregions : '),
      purrr::map_chr(stored_macroregions, 1L) %>%
        str_to_title_if_needed() %>%
        crayon::blue() %>%
        paste(collapse = '; '),
      ' (#', length(stored_macroregions), ')'
  )

  # cat_line('    ',
  #     crayon::bold('Regions      : '),
  #     stored_regions %>%
  #       stringr::str_to_title() %>%
  #       crayon::blue() %>%
  #       paste(collapse = '; '),
  #     ' (#', length(stored_regions), ')'
  # )
  cat_line('    ',
      crayon::bold('Regions      : '),
      purrr::map(stored_macroregions, get_all_region) %>%
        purrr::map(str_to_title_if_needed) %>%
        purrr::map(crayon::blue) %>%
        purrr::map(paste, collapse = ', ') %>%
        unlist %>%
        paste(collapse = '; '),
      ' (#', length(get_all_region(x)), ')'
  )

  stored_centers <- get_centers(x) %>%
    purrr::map(get_center)
  cat_line('    ',
      crayon::bold('Centers      : '),
      purrr::map(stored_centers,
        ~ purrr::map_chr(.x, 1L)
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
    ' (from every centers of every region)'
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
