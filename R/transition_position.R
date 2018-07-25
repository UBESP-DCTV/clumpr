#' Transition of positions for a next organ
#'
#' @param Tpast base position matrix of positions' probability
#' @param p acceptance probabilities
#'
#' @return the matrix with the updated positions' probability
transition_position <- function(Tpast, p) {
    n <- length(p)
    if (n < 2) return(Tpast)
    Tnow <- purrr::map(seq_len(n), ~numeric(n))

    for (i in seq_len(n-1)) {
      for (j in seq_len(n-1)) {
        fill <- purrr::map(seq_len(n), ~ {
          from_max <- (rev(seq_len(n)) %>% wavethresh::guyrot(i - j))
          c(
            p[[from_max[[1]]]],
            purrr::map_dbl(seq_len(n - 1) + 1,
                    function(.x) {1 - p[[from_max[[.x]]]]}
            )
          )[seq_len(.)]
        })

        fill[[n]] <- fill[[n]][2:n]

        fill <- fill %>%
          wavethresh::guyrot(j) %>%
          purrr::map(~prod(.,na.rm = TRUE))

        Tnow[[i]][[j]] <- purrr::map_dbl(seq_len(n),
                                  ~ {
                                    Tpast[i, .] * fill[[.]]
                                  }
        ) %>%
          sum()
      }
    }

    for (i in seq_len(n-1)) {
      Tnow[[i]][[n]] <- 1 - sum(Tnow[[i]][1:(n-1)])
    }
    for (j in seq_len(n)) {
      Tnow[[n]][[j]] <- 1 - sum(purrr::map_dbl(seq_len(n-1), ~Tnow[[.]][[j]]))
    }

    Tnow %>% unlist %>% matrix(nrow = n, byrow = TRUE)
  }
