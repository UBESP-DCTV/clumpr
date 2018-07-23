library(shinyjs)

shinyUI(fluidPage(
  # Use shiny js to disable fields
  shinyjs::useShinyjs(),

    # Application title
  titlePanel("Current transpLant sUrplus Management Protocol in R"),

  tabsetPanel(
    # Inserimento dati
    tabPanel('Setup',
       # flowLayout(
       #  verticalLayout(
            # Input fields
            disabled(textInput("id", "Id", "0")),


            textInput("state", "State", "Italy"),


            textInput("ma", "Macroarea", ""),


        # (Macro)regions --------------------------------------------
            checkboxInput("mr", "In macroregion?", FALSE),
            textInput("mr_name", "Macroregion", ""),
            textInput("region", "Region", ""),

        # Centers ---------------------------------------------------

            checkboxInput("center", "Has centers?", TRUE),
            textInput("center_name", "Center", ""),
            sliderInput("p_accept", "Acceptance rate", 0, 100, 100,
              step = 0.1,
              post = " %"
            ),


            sliderInput("offered", "Number of surplus", 0L, 365L, 1L,
              step = 1L, post = " organs"
            ),


            #action buttons
            actionButton("new", "New"),
            actionButton("submit", "Submit"),
            actionButton("delete", "Delete"),
            actionButton("makeit", "Make it!"),
        # ), # end verticalLayout
        #  verticalLayout(
            #data table
            DT::dataTableOutput("centers_table", width = 300)
       #   ) # end verticalLayout
       # ) # end flowLayout
    ), # end tabPanel
    tabPanel('Graph'
    ) # end tabPanel
  ) # end tabsetPanel
)) # end fluidPage and shinyUI
