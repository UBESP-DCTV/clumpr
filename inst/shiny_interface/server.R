shinyServer(function(input, output, session) {



  # input fields are treated as a group
  formData <- reactive({
    sapply(names(GetTableMetadata()$fields), function(x) input[[x]])
  })

  # Check macroregion -> disable the input
  observeEvent(input$mr, {
    if (!input$mr) {
      shinyjs::disable("mr_name")
      updateTextInput(session, "mr_name", value = '')
    } else {
      shinyjs::enable("mr_name")
      updateTextInput(session, "mr_name", value = 'NIT-p')
    }
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



  # Press "Make it!" button -> create the state
  observeEvent(input$makeit, {
    CreateState()
    UpdateInputs(CreateDefaultRecord(), session)
  })


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

})
