library(shinyjs)

shinyUI(
  fluidPage(
    # Use shiny js to disable fields
    shinyjs::useShinyjs(),
    title = "UBESP/Shiny/CLUMPR",
    titlePanel("Current transpLant sUrplus Management Protocol in R"),

    tabsetPanel(


      # Inserimento dati --------------------------------------------
      tabPanel('Setup',
        sidebarLayout(
         sidebarPanel(

          fileInput("datafile",
            label  = "Load data choosing an RDS file (optional)",
            accept = ".RDS"
          ),

          strong("GLOBAL INFORMATION"),

          selectInput("state", "State",
            stringr::str_to_title(names(clumpr::regions))
          ),
          textInput("macroareas", "Macroareas (strip-ordered)", "Nord, Sud"),
          textInput("macroregions", "Macroregions", "NIT-p"),

          hr(),

          strong("LOCAL INFORMATION"),
          disabled(textInput("id", "Id", "0")),

          selectInput("ma", "Macroarea", character(0)),

          fluidRow(
            column(4, checkboxInput("mr", "In macroregion?", FALSE)),
            column(4, selectInput("mr_name", "Macroregion", character(0))),
            column(4, numericInput("macropos", "Strip-position", 0L,
              min = 0L, max = 1L, step = 1L
            ))
          ),

          fluidRow(
            column(4, selectInput("region", "Region", character(0))),
            column(4, textInput("inmacropos", "Strip-position",
              character(0)
            )),
            column(4, numericInput("regpos",
              label = "Strip-position",
              value = 0L,
              min   = 0L,
              max   = 1L,
              step  = 1L
            ))
          ),

          fluidRow(
            column(4, checkboxInput("center", "Has centers?", TRUE)),
            column(8, textInput("center_name", "Center", character(0)))
          ),


          fluidRow(
            column(6, numericInput("p_accept", "Acceptance rate",
                min   = 0,
                max   = 1,
                value = 1,
                step  = 0.01
            )),
            column(6, numericInput("offered",
                "Number of surplus organs",
                min   = 0L,
                max   = 365L,
                value = 1L,
                step  = 1L
              )
            )
          ),

          hr(),

          actionButton("new", "New"),
          actionButton("submit", "Submit"),
          actionButton("delete", "Delete"),
          actionButton("makeit", "Make it!")
         ), # end sidebarPanel

         mainPanel(
            DT::dataTableOutput("centers_table")
         ) # end mainPanel
       ) # end sidebarLayout
    ), # end tabPanel




    tabPanel('Graphs',
      navbarPage(title = "Level of detail: ",
        tabPanel("Macroarea", plotOutput("pma")),
        tabPanel("State",
          navbarPage(title = "Probability for: ",
            tabPanel("the n-th lung",
              sidebarLayout(
                sidebarPanel(
                  sliderInput("nth", "Lung number:",
                    min = 1L, max = 1L, value = 1L
                  )
                ),
                mainPanel(plotOutput("ricacct"))
              )
            ),
            tabPanel("at least n lungs",
              sidebarLayout(
                sidebarPanel(
                  sliderInput("atleastn", "Minimum number of lung(s):",
                    min = 1L, max = 1L, value = 1L
                  )
                ),
                mainPanel(plotOutput("atleastplot"))
              )
            )
          )
        ),
        tabPanel("Macroregions' details",
          selectInput("mr_name_macro", "Macroregion", character(0)),
          hr(),
          navbarPage(title = "Probability for: ",
            tabPanel("the n-th lung",
              sidebarLayout(
                sidebarPanel(
                  sliderInput("nth_macro", "Lung number:",
                    min = 1L, max = 1L, value = 1L
                  )
                ),
                mainPanel(plotOutput("ricacct_macro"))
              )
            ),
            tabPanel("at least n lungs",
              sidebarLayout(
                sidebarPanel(
                  sliderInput("atleastn_macro", "Minimum number of lung(s):",
                    min = 1L, max = 1L, value = 1L
                  )
                ),
                mainPanel("DO THIS HAVE REALY SENSE?! (HOW?)")#plotOutput("atleastplot_macro"))
              )
            )
          )
        )
      )
    ) # end tabPanel
  ) # end tabsetPanel
)) # end fluidPage and shinyUI
