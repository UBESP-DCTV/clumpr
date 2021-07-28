library(tidyverse)
library(here)
library(polynom)

library(clumpr)
library(patchwork)



setup_clumpr <- function(acceptance_rate) {
  centers_table <- here("inst/shiny_interface/data/2015_italy.RDS") %>%
    read_rds() %>%
    mutate(across(p_accept, ~if_else(.x != 0, acceptance_rate, .x)))


  merge_macroregion <- function(..., macro_area) {
    regions <- list(...)[[1]]

    actual_mr <- unique(
      centers_table[centers_table[['region']] %in% names(regions), 'mr_name']
    ) %>%
      setdiff("")


    singles <- names(regions) %in%
      centers_table[
        centers_table[['region']] %in% names(regions) & !centers_table[['mr']],
        'region'
      ]

    purrr::map(actual_mr,
               function(mr) {

                 macroregion(mr,
                             regions = set_regions(
                               regions[
                                 names(regions) %in%
                                   centers_table[centers_table[['mr_name']] == mr, 'region']
                               ] %>%
                                 '['(unique(names(.)))
                             ),
                             initial_strip = mr_strip(centers_table, mr)
                 )
               }
    ) %>%
      c(regions[singles])
  }


  ma_strip_f <- function(data_full, macar) {
    data_full %>%
      filter(ma == macar) %>%
      mutate(
        strip_rank = if_else(is.na(regpos), macropos, regpos),
        strip_name = if_else(is.na(regpos), mr_name, region)
      ) %>%
      filter(!is.na(strip_rank)) %>%
      group_by(strip_rank) %>%
      summarize(
        mr = strip_name[[1]]
      ) %>%
      ungroup %>%
      as.data.frame() %>%
      `[[`('mr')
  }



  # find the strip for the macroarea
  mr_strip <- function(data, macror) {
    rank_data <- data %>%
      filter(mr_name == macror, center) %>%
      group_by(region) %>%
      filter(row_number() == 1) %>%
      select(region, inmacropos) %>%
      mutate(
        mr_rank = inmacropos %>%
          map(~stringr::str_split(., "[^\\w]+") %>%
                unlist() %>%
                as.integer()
          )
      ) %>%
      ungroup()

    mr_rank <- vector('character', sum(purrr::map_int(rank_data$mr_rank, length)))
    for(i in seq_along(rank_data[['mr_rank']])) {
      mr_rank[rank_data[['mr_rank']][[i]]] <- rank_data[["region"]][[i]]
    }
    mr_rank
  }


  ma <- set_macroareas(
    purrr::map(
      unique(centers_table[["ma"]]),

      function(ma) {
        macroarea(
          name = ma,
          macroregions = set_macroregions(
            purrr::map(
              centers_table[centers_table[["ma"]] == ma, "region"] %>%
                purrr::set_names(.),

              function(reg) {
                region(
                  set_centers(
                    purrr::map(
                      centers_table[centers_table[["region"]] == reg, "center_name"] %>%
                        purrr::set_names(.),

                      function(cent) {
                        center(
                          name = cent %>% tolower(),
                          region = reg %>% tolower(),
                          offered = centers_table[centers_table[["center_name"]] == cent, "offered"][[1]],
                          p_accept = centers_table[centers_table[["center_name"]] == cent, "p_accept"][[1]] / 100
                        ) # center creatred
                      }
                    ) # all centers of the region created
                  )
                ) # end of region()
              } # end of the region function
            ) %>% # all region created
              ## region created, here we have to aggregate the macroregions!
              ## befor to pass them to set_macroregions()
              merge_macroregion(macro_area = ma)
          ), # end set_macroregions() for macroarea definition
          initial_strip = ma_strip_f(centers_table, ma) %>% tolower()
        ) # end macroarea()
      } # end function for create macroareas inside purrr::map()
    ) # end purrr::map() for set_macroareas()
  )

  state_2015 <- state(
    name       = centers_table[['state']][[1]],
    macroareas = ma
  )

  pma <- state_2015 %>%
    get_p_macroareas() %>%
    tidyr::spread('macroarea', 'prob') %>%
    dplyr::select(-lost) %>%
    as.list()

  # pma %>%
  #   as_data_frame %>%
  #   mutate(lost = 1 - (sum(., na.rm = TRUE))) %>%
  #   gather("ma", "prob") %>%
  #   ggplot(aes(x = ma, y = prob, fill = ma)) +
  #   geom_bar(stat = 'identity') +
  #   xlab("Macroareas") +
  #   ylab("Probability") +
  #   ggtitle(
  #     'Probability for a lung to be offered and accepted in each macroares (or lost).'
  #   )

  # Probability to accept by position
  pap <- state_2015 %>%
    get_p_accept_by_position()



  ricacct <- state_2015 %>%
    get_p_position_by_time(get_offered(.)) %>%
    map(function(stage) {
      pmap(.l = list(stage, pap, pma), function(.x1, .x2, .x3) {
        (.x1 * .x2) %>%
          rowSums() %>%
          `*`(.x3) %>%
          as_tibble() %>%
          mutate(region = row.names(.x1))
      })
    })

  ricacct

}


make_p_by_n <- function(ricacct, acceptance_rate, save = TRUE) {

  pmr <- function(.state) {
    map_dfr(
      .x  = seq_len(get_offered(.state)),
      .f  = ~bind_rows(ricacct[[.x]], .id = "ma"),
      .id = "n"
    )
  }


  df_mr <- pmr(state_2015)

  p <- df_mr %>%
    mutate(n = as.integer(n)) %>%
    ggplot(aes(n, value, colour = region)) +
    geom_line(alpha = 0.4) +
    geom_smooth(se = FALSE) +
    xlab(expression(paste("n"^{th},' lung offered'))) +
    ylab("P") +
    ggtitle("Probability for each n-th lung of being offered to and accepted by the regions",
            subtitle = paste0("Year: 2015; mean acceptance rate: ",
                              acceptance_rate)) +
    theme(legend.position = "top")

  if (save) {
    ggplot2::ggsave(here("analyses", "output",
                         paste0("2015_p-by-n_", acceptance_rate, "accept.png")),
                    plot = p,
                    width = 11.7, height = 8.3
    )
  }

  p
}




make_p_at_least <- function(ricacct, acceptance_rate, save = TRUE) {



  tidy_probs <- ricacct %>%
    map(~bind_rows(., .id = "ma"))

  region_probs <- setNames(
    map(seq_along(tidy_probs[[1]][["region"]]), ~{
      map_dbl(seq_along(tidy_probs),
              function(z) tidy_probs[[z]][["value"]][[.]]
      )
    }),
    tidy_probs[[1]][["region"]]
  )


  p_atleast_n <- function(n) {
    map_dbl(
      seq_along(region_probs) %>% set_names(names(region_probs)), ~ {
        map(
          seq_along(region_probs[[1]]),
          function(z) {
            polynomial(c(1 - region_probs[[.]][[z]], region_probs[[.]][[z]]))
          }
        ) %>%
          as.polylist() %>%
          prod(na.rm = TRUE) %>%
          coef() %>%
          `[`(min(n, length(.), na.rm = TRUE):length(.)) %>%
          # `[`(-seq_len(input$atleastn - 1)) %>%
          sum(na.rm = TRUE)
      }
    ) %>%
      tibble(region = names(.), ma = tidy_probs[[1]][["ma"]]) %>%
      rename(cum_prob = ".")
  }


  df_atleast <- map_dfr(
    .x  = seq_len(get_offered(state_2015)),
    .f  = p_atleast_n,
    .id = "n"
  )

  p <- df_atleast %>%
    mutate(n = as.integer(n)) %>%
    ggplot(aes(n, cum_prob, colour = region)) +
    geom_line() +
    xlab("n lungs offered") +
    ylab("P") +
    ggtitle("Probability, for each n, that at least n lungs would be offered to and accepted by the regions",
            subtitle = paste0("Year: 2015; mean acceptance rate: ",
                              acceptance_rate)) +
    theme(legend.position = "top") +
    coord_cartesian(xlim = c(0, 30))

  if (save) {
    ggplot2::ggsave(here("analyses", "output",
                         paste0("2015_p_at-least-n_", acceptance_rate, "accept.png")),
                    plot = p,
                    width = 11.7, height = 8.3
    )
  }


  p

}


make_plots <- function(acceptance_rate, save = FALSE) {

  ricacct <- setup_clumpr(acceptance_rate)

  list(
    p_at_least = make_p_at_least(ricacct, acceptance_rate, save = save),
    p_by_n = make_p_by_n(ricacct, acceptance_rate, save = save)
  )
}



ninety_ok <- make_plots(90)
all_ok <- make_plots(100)


p_by_n_90 <- ninety_ok$p_by_n +
  labs(tag = "A") +
  theme(legend.position = c(.8, 0.65))

p_at_least_90 <- ninety_ok$p_at_least +
  labs(tag = "B") +
  theme(legend.position = "none")



p_by_n_100 <- all_ok$p_by_n +
  labs(tag = "C") +
  theme(legend.position = "none")

p_at_least_100 <- all_ok$p_at_least +
  labs(tag = "D") +
  theme(legend.position = "none")


((p_by_n_90 | p_by_n_100) / (p_at_least_90 | p_at_least_100)) &
  ggtitle("", subtitle = "")

ggplot2::ggsave(here("analyses", "output",
                     paste0("2015_patchwork_p_90-100_accept.png")),
                width = 11.7, height = 8.3
)






