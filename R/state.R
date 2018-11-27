#' Constructor for object of class \code{state}
#'
#' The function \code{\link{state}()} is the constructor function for the homonymous
#' class.
#'
#' @param name [chr] the name of the state
#' @param macroareas [lst] a list of objects of class
#'        \code{\link{macroarea}}, passed by
#'        \code{\link{set_macroareas}()}. Note: all the macroarea in a
#'        state must be of the same state (obviously).
#' @param initial_strip [chr] a sequence of \code{\link{macroarea}}s'
#'        names in the \code{\link{state}} representing the order in
#'        which organs started to be distributed in the
#'        \code{\link{state}} at the beginning of the period. All the
#'        listed \code{\link{area}}s
#'        have to be included also in the \code{macroareas} field.
#'        Exculsions are permitted but repetitions are not admitted.
#'        (default is the sequence in which \code{\link{area}}s
#'        has been included in the argument \code{macroareas})
#'
#' @return An object of class \code{\link{state}}.
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
#'
#' roma   <- center('Roma', 'Lazio', 10, 0.9)
#' lazio  <- region(set_centers(roma))
#'
#' sud <- macroarea('Sud',
#'   macroregions = set_macroregions(lazio)
#' )
#' sud
#'
#' italy <- state('Italy',
#'   macroareas = set_macroareas(nord, sud)
#' )
#' italy
state <- function(name, macroareas,
                  initial_strip = get_all_macroarea(macroareas)
) {

  # input check ---------------------------------------------------------

  ## name
  assertive::assert_is_inherited_from(name, 'character')
  assertive::assert_is_a_non_missing_nor_empty_string(name)

  ## macroareas
  if (!inherits(macroareas, 'set_macroareas')) stop(
    paste0('Argument ', crayon::silver("macroareas"),
           ' must be provide using\n',
           '       ', crayon::green('set_macroareas()'),
           ', and it isn\'t.\n\n',

           'Hint : To set `macroarea_1` and `macroarea_2` use\n',
           '       ',
           crayon::silver("set_macroregions(macroarea_1, macroarea_2)"), '.\n',
           'Note : all the macroareas must be of the\n',
           '       same states. No repetition is admitted in macroareas.'
    ),
    call. = FALSE
  )

  ## state name
  if (tolower(name) != tolower(get_state(macroareas[[1L]]))) stop(
    paste0('State ', crayon::silver("name"),
           ' must be the same as the name of the macroareas\' name',
           ', and it isn\'t:\n',
           '       name provided is   : ',  crayon::red(name), ',\n',
           '       macroareas\' name is: ', crayon::green(
             get_state(macroareas[[1L]])
           ), '.\n',
           'Note : state\'s name check is not case sensitive.'
    ),
    call. = FALSE
  )

  # constructor ----------------------------------------------------------

  p_accept_area <- purrr::map_dbl(macroareas, get_p_accept)
  total_offered <- purrr::map_dbl(macroareas, get_offered)

  structure(.Data = c(state = name),
            macroareas      = macroareas,
            acceptance_rate = at_least_one(p_accept_area),
            offered         = sum(total_offered),
            initial_strip   = initial_strip,
            current_strip   = initial_strip,
            current_time    = 0L,
            state           = get_state(macroareas[[1L]]),
            class           = 'state'
  )
}






#' List of macroarea creator
#'
#' Function used to prepare and check list of \code{\link{macroarea}}s to
#' pass to \code{macroarea} as needed to create \code{\link{state}}
#' objects.
#'
#' @param ... a set of \code{\link{macroarea}}s.
#'
#' @return The list of \code{\link{macroarea}}s provided.
#' @export
#'
#' @examples
#' pavia     <- center('Pavia',   'Lombardia', offered = 5, p_accept = 0.6)
#' bergamo   <- center('Bergamo', 'Lombardia', 8)
#' milano    <- center('Milano',  'Lombardia', 10, 0.8)
#' lombardia <- region(set_centers(pavia, bergamo, milano), default_p = 0.7)
#'
#' padova <- center('Padova', 'Veneto', 8, 0.7)
#' veneto <- region(set_centers(padova))
#'
#' nitp <- macroregion('NITp', regions = set_regions(lombardia, veneto),
#'   initial_strip = c('lombardia', 'lombardia', 'veneto')
#' )
#'
#' torino   <- center('Torino', 'Piemonte', 7, 0.6)
#' piemonte <- region(set_centers(torino))
#'
#' nord <- macroarea('Macroarea Nord',
#'   macroregions = set_macroregions(piemonte, nitp)
#' )
#'
#' roma   <- center('Roma', 'Lazio', 10, 0.9)
#' lazio  <- region(set_centers(roma))
#' sud    <- macroarea('Macroarea Sud',
#'   macroregions = set_macroregions(lazio)
#' )
#'
#' set_macroareas(nord, sud)
set_macroareas <- function(...) {
  .dots <- list(...)

  if (length(.dots) == 1L & inherits(.dots[[1]][[1]], 'macroarea')) {
    .dots <- .dots[[1]]
  }

  if (!all(purrr::map_lgl(.dots, inherits, c('macroarea')))) {
    stop(
      paste0('All ', crayon::silver("macroareas"), ' must be of class ',
             crayon::green('macroarea'), ', and they aren\'t.\n\n',
             '       Have you create all of them using ',
             crayon::silver('macroarea()'), '?'
      ),
      call. = FALSE
    )
  }

  regs <- purrr::map(.dots, get_all_region) %>%
    unlist() %>%
    unname()

  if (length(regs) != length(unique(regs))) stop(
    paste0(crayon::silver("Regions"),
      ' must be different and they aren\'t.\n\n',
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

  sts <- purrr::map_chr(.dots, get_state) %>%
    unique
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
      purrr::map_chr(.dots, 1L) %>%
        stringr::str_to_lower()
    ),
    class = 'set_macroareas'
  )
}


#' @inheritParams base::print
#' @describeIn set_macroareas nice (and coloured, if supported) print method.
#'
#' @export
print.set_macroareas <- function(x, ...) {
  purrr::walk(x, ~ {print(.); cat_line()})
  invisible(x)
}




#' @inheritParams base::print
#' @describeIn state nice (and coloured, if supported) print method.
#'
#' @export
print.state <- function(x, ...) {
  cat_line('    ',
    crayon::bold('State        : '),
    crayon::blue(stringr::str_to_title(x[[1]]))
  )

  stored_macroareas <- get_macroareas(x)
  cat_line('    ',
           crayon::bold('Macroareas   : '),
           purrr::map_chr(stored_macroareas, 1L) %>%
             str_to_title_if_needed() %>%
             crayon::blue() %>%
             paste(collapse = '; '),
           ' (#', length(stored_macroareas), ')'
  )


  stored_macroregions <- get_macroregions(x)
  cat_line('    ',
           crayon::bold('Macroregions : '),
           purrr::map(stored_macroareas, get_macroregions) %>%
             purrr::map(str_to_title_if_needed) %>%
             purrr::map(crayon::blue) %>%
             purrr::map(paste, collapse = ', ') %>%
             unlist %>%
             paste(collapse = '; '),
           ' (#', length(get_macroregions(x)), ')'
  )

  stored_regions <- get_macroregions(x) %>%
    purrr::map(get_regions)
  cat_line('    ',
           crayon::bold('Regions      : '),
           purrr::map(stored_regions,
                ~purrr::map_chr(.x, 1L)
           ) %>%
             purrr::map(str_to_title_if_needed) %>%
             purrr::map(crayon::blue) %>%
             purrr::map(paste, collapse = ', ') %>%
             unlist %>%
             paste(collapse = '; '),
           ' (#', length(unlist(stored_regions, recursive = FALSE)), ')'
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
