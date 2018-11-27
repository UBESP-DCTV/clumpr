shinyServer(function(input, output, session) {


  observeEvent(input$datafile, {
    if (is.null(input$datafile)) return(NULL)
    centers_table <<- read_rds(input$datafile$datapath)
  })


  # Macroarea selector
  macro_areas <- reactive({
    req(input$macroareas)
    stringr::str_split(input$macroareas, "[^\\w|-]+") %>%
      unlist
  })

  observeEvent(input$macroareas, {
    updateSelectInput(session, "ma",
      choices = c("Choose" = "", macro_areas()))
  })


  # Macroregion selector
  macro_regions <- reactive({
    req(input$macroregions)
    stringr::str_split(input$macroregions, "[^\\w|-]+") %>%
      unlist
  })

  observeEvent(input$macroregions, {
    updateSelectInput(session, "mr_name",
      choices = c("Choose" = "", macro_regions()))
    updateSelectInput(session, "mr_name_macro",
      choices = c("Choose" = "", macro_regions()))
  })


  # Max position for a (macro)region in the area strip
  max_pos_ma <- reactive({
    req(input$ma)
    if (exists("centers_table") && nrow(centers_table) > 0L) {
      (
        centers_table[
          centers_table$ma == input$ma &
            centers_table$center   &
            centers_table$mr_name != "",                                      # rows

          "mr_name"                                                          # col
          ] %>%
          unique() %>%
          length()
      ) +
        (
          centers_table[
            centers_table$ma == input$ma &
              centers_table$center   &
              centers_table$mr_name == "",                                      # rows

            "region"                                                           # col
            ] %>%
            unique() %>%
            length()
        ) +
        1L
    } else {
      1L
    }
  })


  # Max position for a region in a macroregion strip
  max_pos_mr <- reactive({
    req(input$mr_name)
    req(input$mr)
    req(input$ma)
    if (exists("centers_table") && nrow(centers_table) > 0L) {
      ((
        centers_table[
          centers_table$mr_name == input$mr_name &
          centers_table$center,
          "region"                                                         # col
          ] %>%
          unique() %>%
          length()
      ) +
        1
      ) %>% as.character()
    } else {
      "1"
    }
  })


  # Check macroregion -> disable the input
  observeEvent({
    input$mr
    input$mr_name
    input$ma
    input$center
    }, {
    if ((!input$mr)) {
      shinyjs::disable("mr_name")
      shinyjs::disable("macropos")
      updateNumericInput(session, "macropos",
        label = paste0("Strip-position"),
        value = integer(0),
        min   = 0L,
        max   = 0L
      )

      shinyjs::disable("inmacropos")
      updateTextInput(session, "inmacropos",
                      label = "Strip-position",
                      value = character(0)
      )

      shinyjs::enable("regpos")
      updateNumericInput(session, "regpos",
        label = paste0("Strip-position (", input$ma, ")"),
        value = max_pos_ma(),
        min   = 1L,
        max   = max_pos_ma()
      )

    } else {
      shinyjs::enable("mr_name")

      shinyjs::enable("macropos")
      updateNumericInput(session, "macropos",
        label = paste0("Strip-position (", input$ma, ")"),
        value = max_pos_ma(),
        min   = 1L,
        max   = max_pos_ma()
      )

      shinyjs::disable("regpos")
      updateNumericInput(session, "regpos",
        label = paste0("Strip-position"),
        value = integer(0),
        min   = 0L,
        max   = 0L
      )

      shinyjs::enable("inmacropos")
      updateTextInput(session, "inmacropos",
        label = paste0("Strip-position (", input$mr_name, ")"),
        value = max_pos_mr()
      )
    }
  })

  observeEvent(input$center, {
    if ((!input$center)) {
      shinyjs::disable("regpos")
      updateNumericInput(session, "regpos",
        label = paste0("Strip-position"),
        value = integer(0),
        min   = 0L,
        max   = 0L
      )

      shinyjs::disable("macropos")
      updateNumericInput(session, "macropos",
        label = paste0("Strip-position"),
        value = integer(0),
        min   = 0L,
        max   = 0L
      )


      shinyjs::disable("inmacropos")
      updateTextInput(session, "inmacropos",
        label = "Strip-position",
        value = character(0)
      )

    }
  })

  # region selector
  regions <- reactive({
    req(input$state)
    clumpr::regions[[tolower(input$state)]]
  })

  observeEvent(input$macroregions, {
    updateSelectInput(session, "region",
      choices = c("Choose" = "", stringr::str_to_title(regions())))
  })

  # input fields are treated as a group
  formData <- reactive({
    sapply(names(GetTableMetadata()$fields), function(x) input[[x]])
  })



  # Check center -> disable the input
  observeEvent(input$center, {
    if (!input$center) {
      shinyjs::disable("center_name")
      updateTextInput(session, "center_name", value = 'no_trasplant_centers')

      shinyjs::disable("p_accept")
      updateNumericInput(session, "p_accept", value = 0)
    } else {
      shinyjs::enable("center_name")
      shinyjs::enable("p_accept")
    }
  })


  # Click "Submit" button -> save data on R
  observeEvent(input$submit, {
    if (input$id != "0") {
      UpdateData(formData())
    } else {
      CreateData(formData())
      UpdateInputs(CreateDefaultRecord(), session)
    }
  }, priority = 1)



  # Press "New" button -> display empty record
  observeEvent(input$new, {
    UpdateInputs(CreateDefaultRecord(), session)
  })



  # Press "Delete" button -> delete from data
  observeEvent(input$delete, {
    DeleteData(formData())
    UpdateInputs(CreateDefaultRecord(), session)
  }, priority = 1)


  # Click "Save data" button -> save data on disk
  observeEvent(input$savedata, {
    save_data(centers_table)
  }, priority = 1)


  # Select row in table -> show details in inputs
  observeEvent(input$centers_table_rows_selected, {
    if (length(input$centers_table_rows_selected) > 0) {
      data <- ReadData()[input$centers_table_rows_selected, ]
      UpdateInputs(data, session)
    }
  })


  # display table
  output$centers_table <- DT::renderDataTable({

    # update after inputed datafile
    input$datafile

    # update after submit is clicked
    input$submit

    # update after delete is clicked
    input$delete

    ReadData()
  }, server = FALSE, selection = "single",
  colnames = unname(GetTableMetadata()$fields)[-1]
  )





  # Create state
  my_state <- reactive({
    if (!exists("centers_table") & is.null(input$datafile)) return(NULL)

    input$makeit

    state(
      name       = centers_table[['state']][[1]],
      macroareas = set_macroareas(
        purrr::map(unique(centers_table[['ma']]),

          function(ma) {
            macroarea(
              name         = ma,
              macroregions = set_macroregions(
                purrr::map(
                  centers_table[centers_table[['ma']] == ma, 'region'] %>%
                    purrr::set_names(.),

                  function(reg) {
                    region(
                      set_centers(
                        purrr::map(
                          centers_table[centers_table[['region']] == reg, 'center_name'] %>%
                            purrr::set_names(.),

                          function(cent) {
                           center(
                              name     = cent %>% tolower(),
                              region   = reg %>% tolower(),
                              offered  = centers_table[centers_table[['center_name']] == cent, 'offered'][[1]],
                              p_accept = centers_table[centers_table[['center_name']] == cent, 'p_accept'][[1]] / 100
                            ) # center creatred
                          }
                        ) # all centers of the region created
                      )
                    ) # end of region()
                  } # end of the region function
                ) %>%  # all region created
                  ## region created, here we have to aggregate the macroregions!
                  ## befor to pass them to set_macroregions()
                  merge_macroregion(macro_area = ma)
              ), # end set_macroregions() for macroarea definition
              initial_strip = ma_strip_f(centers_table, ma) %>% tolower()
            ) # end macroarea()
          } # end function for create macroareas inside purrr::map()
        ) # end purrr::map() for set_macroareas()
      ), # end set_macroareas()
      initial_strip = macro_areas() %>% tolower()
    ) # end state()
  }) # end reactive()


  # Press "Make it!" button -> create the state
  observeEvent(input$makeit, {
    my_state()
    UpdateInputs(CreateDefaultRecord(), session)
  })


 # Probability for a lung to be accepted by a macroarea
  pma <- reactive({
    req(my_state())

    my_state() %>%
      get_p_macroareas() %>%
      tidyr::spread('macroarea', 'prob') %>%
      dplyr::select(-lost) %>%
      as.list()
  })

  # Probability to accept by position
  pap <- reactive({
    req(my_state())

    my_state() %>%
      get_p_accept_by_position()
  })


  output$pma <- renderPlot({
    req(pma())

    pma() %>%
      as_data_frame %>%
      mutate(lost = 1 - (sum(., na.rm = TRUE))) %>%
      gather("ma", "prob") %>%
      ggplot(aes(x = ma, y = prob, fill = ma)) +
      geom_bar(stat = 'identity') +
      xlab("Macroareas") +
      ylab("Probability") +
      ggtitle(
        'Probability for a lung to be offered and accepted in each macroares (or lost).'
      )
  })


  max_offered <- reactive({
    req(my_state())

    my_state() %>%
      get_offered()
  })

  # probabilità di ricevere e accettare al tempo t
  ricacct <- reactive({
    req(my_state())

    my_state() %>%
      get_p_position_by_time(max_offered()) %>%
      map(function(stage) {
        pmap(.l = list(stage, pap(), pma()), function(.x1, .x2, .x3) {
          (.x1 * .x2) %>%
            rowSums() %>%
            `*`(.x3) %>%
            as_data_frame() %>%
            mutate(region = row.names(.x1))
        })
      })
  })



  output$ricacct <- renderPlot({
    input$datafile

    ricacct()[[input$nth]] %>%
      bind_rows(.id = "ma") %>%
      ggplot(aes(x = region, y = value, fill = ma)) +
      geom_bar(stat = 'identity') +
      xlab("(Macro)regions") +
      ylab("Probability") +
      ggtitle(
        paste0(
          'Probabilty for the lung number ', input$nth,
          ' (of ', length(ricacct()), ') to be offered and accepted by the (macro)regions.'
        )
      )
  })

  tidy_probs <- reactive(
    ricacct() %>%
      map(~bind_rows(., .id = "ma"))
  )

  region_probs <- reactive(
    setNames(
      map(seq_along(tidy_probs()[[1]][["region"]]), ~{
        map_dbl(seq_along(tidy_probs()),
          function(z) tidy_probs()[[z]][["value"]][[.]]
        )
      }),
      tidy_probs()[[1]][["region"]]
    )
  )


  output$atleastplot <- renderPlot({

    map_dbl(
      seq_along(region_probs()) %>% set_names(names(region_probs())), ~ {
        map(seq_along(region_probs()[[1]]),
            function(z) {
              polynomial(c(1 - region_probs()[[.]][[z]], region_probs()[[.]][[z]]))
            }
        ) %>%
          as.polylist %>%
          prod(na.rm = TRUE) %>%
          coef %>%
          `[`(min(input$atleastn, length(.), na.rm = TRUE):length(.)) %>%
          # `[`(-seq_len(input$atleastn - 1)) %>%
          sum(na.rm = TRUE)
      }
    ) %>%
      data_frame(region = names(.), ma = tidy_probs()[[1]][["ma"]]) %>%
      rename(cum_prob = ".") %>%
      ggplot(aes(x = region, y = cum_prob, fill = ma)) +
      geom_bar(stat = 'identity') +
      xlab("(Macro)regions") +
      ylab("Probability") +
      ggtitle(paste0(
        'Probability that at least ', input$atleastn,
        ' (out of ', length(ricacct()),
        ') lungs will be offered and accepted by the (macro)regions.'
      ))
  })




  max_offered_macro <- reactive({
    req(my_state)
    req(input$mr_name_macro)

    max_offered() -
      my_state() %>%
        get_regions() %>%
        purrr::map_dbl(get_offered) %>%
        `[`(centers_table[
          centers_table$mr_name == input$mr_name_macro &
          centers_table$center,
          "region"
         ] %>% unique() %>% tolower()) %>%
        sum()
  })


  ricacct_macro <- reactive({
    req(my_state())
    req(input$mr_name_macro)
    req(input$nth_macro)

    my_state() %>%
      get_macroregions() %>%
      `[[`(input$mr_name_macro %>% tolower()) %>%
      get_p_position_by_time_nitp(
        input$nth_macro,
        my_state() %>% tolower
      ) %>%
      map(function(stage) {
        (
          stage *
          my_state() %>%
            get_macroregions() %>%
            `[[`(input$mr_name_macro %>% tolower()) %>%
            get_p_accept_by_position_nitp(my_state() %>% tolower)
        ) %>%
          rowSums() %>%
          as_data_frame() %>%
          mutate(
            region = my_state() %>%
                get_macroregions() %>%
                `[[`(input$mr_name_macro %>% tolower()) %>%
                get_initial_strip()
          ) %>%
          group_by(region) %>%
          summarise_all(sum)
      })
  })

  output$ricacct_macro <- renderPlot({
    req(ricacct_macro())

    ricacct_macro()[[input$nth_macro]] %>%
      ggplot(aes(x = region, y = value, fill = region)) +
      geom_bar(stat = 'identity') +
      xlab("Regions") +
      ylab("Probability") +
      scale_fill_discrete(guide = "none") +
      ggtitle(
        paste0(
          'Probabilty for the lung number ', input$nth_macro,
          ' (of ', max_offered_macro(), ') to be offered and accepted by the (macro)regions.'
        )
      )
  })


  tidy_probs_macro <- reactive(
    ricacct_macro() %>%
      map(bind_rows)
  )

  region_probs_macro <- reactive(
    setNames(
      map(seq_along(tidy_probs_macro()[[1]][["region"]]), ~{
        map_dbl(seq_along(tidy_probs_macro()),
                function(z) tidy_probs_macro()[[z]][["value"]][[.]]
        )
      }),
      tidy_probs_macro()[[1]][["region"]]
    )
  )

  output$atleastplot_macro <- renderPlot({

    map_dbl(
      seq_along(region_probs_macro()) %>% set_names(names(region_probs_macro())), ~ {
        map(seq_along(region_probs_macro()[[1]]),
            function(z) {
              polynomial(c(1 - region_probs_macro()[[.]][[z]], region_probs_macro()[[.]][[z]]))
            }
        ) %>%
          as.polylist %>%
          prod(na.rm = TRUE) %>%
          coef %>%
          `[`(min(input$atleastn_macro, length(.), na.rm = TRUE):length(.)) %>%
          sum(na.rm = TRUE)
      }
    ) %>%
      data_frame(region = names(.)) %>%
      rename(cum_prob = ".") %>%
      ggplot(aes(x = region, y = cum_prob, fill = region)) +
      geom_bar(stat = 'identity') +
      xlab("Regions") +
      ylab("Probability") +
      ggtitle(paste0(
        'Probability that at least ', input$atleastn_macro,
        ' (out of ', max_offered_macro(),
        ') lungs will be offered and accepted by the regions (in ',
        input$mr_name_macro,
        ').'
      ))
  })


  observeEvent({
     my_state()
     input$mr_name_macro
    },{
    updateSliderInput(session, "nth",
      min = 1L, max = max_offered(), step = 1L
    )
    updateSliderInput(session, "atleastn",
      min = 1L, max = max_offered(), step = 1L
    )
    updateSliderInput(session, "nth_macro",
      min = 1L, max = max_offered_macro(), step = 1L
    )
    updateSliderInput(session, "atleastn_macro",
      min = 1L, max = max_offered_macro(), step = 1L
    )
  })

})
