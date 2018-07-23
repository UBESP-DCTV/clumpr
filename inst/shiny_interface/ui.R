library(shinyjs)

shinyUI(fluidPage(
  # Application title
  titlePanel("Current transpLant sUrplus Management Protocol in R"),

  # Use shiny js to disable fields
  useShinyjs(),

  tabsetPanel(
    # Inserimento dati
    tabPanel('Setup',
       flowLayout(
        verticalLayout(
            # Input fields
            disabled(textInput("id", "Id", "0")),


            textInput("state", "State", "Italy"),


            textInput("ma", "Macroarea", ""),


            checkboxInput("mr", "In macroregion?", FALSE),
            conditionalPanel(
              condition = "input.mr", textInput("mr_name", "Macroregion", "NIT-P")
            ),
            conditionalPanel(
              condition = "!input.mr", disabled(textInput("mr_name", "Macroregion", ""))
            ),


            textInput("region", "Region", ""),


            checkboxInput("center", "Has centers?", TRUE),
            conditionalPanel(
              condition = "input.center", textInput("center_name", "Center", ""),
              sliderInput("p_accept", "Acceptance rate", 0, 100, 100,
                step = 0.1, post = " %"
              )
            ),
            conditionalPanel(
              condition = "!input.center",
              disabled(
                textInput("center_name", "Center", "no-trasplant_centers")
              ),
              disabled(
                sliderInput("p_accept", "Acceptance rate", 0, 100, 0,
                  step = 0.1, post = " %"
                )
              )
            ),

            sliderInput("offered", "Number of surplus", 0L, 365L, 1L,
              step = 1L, post = " organs"
            ),


            #action buttons
            actionButton("new", "New"),
            actionButton("submit", "Submit"),
            actionButton("delete", "Delete")
         ), # end verticalLayout
         verticalLayout(
            #data table
            DT::dataTableOutput("centers_table", width = 300)
         ) # end verticalLayout
       ) # end flowLayout
    ), # end tabPanel
    tabPanel('Graph'
    ) # end tabPanel
  ) # end tabsetPanel
)) # end fluidPage and shinyUI
