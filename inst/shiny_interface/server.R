shinyServer(function(input, output, session) {


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


  # Click "Submit" button -> save data
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



  # Select row in table -> show details in inputs
  observeEvent(input$centers_table_rows_selected, {
    if (length(input$centers_table_rows_selected) > 0) {
      data <- ReadData()[input$centers_table_rows_selected, ]
      UpdateInputs(data, session)
    }
  })


  # display table
  output$centers_table <- DT::renderDataTable({

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
    cat("11111111111")
    if (!exists("centers_table")) return(NULL)

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
                              name     = cent,
                              region   = reg,
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
              initial_strip = ma_strip(centers_table, ma)
            ) # end macroarea()
          } # end function for create macroareas inside purrr::map()
        ) # end purrr::map() for set_macroareas()
      ), # end set_macroareas()
      initial_strip = input$macroareas
    ) # end state()
  }) # end reactive()


  # Press "Make it!" button -> create the state
  observeEvent(input$makeit, {
    my_state()
    UpdateInputs(CreateDefaultRecord(), session)
  })






})
