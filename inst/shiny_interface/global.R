library(shiny)

# Helper functions --------------------------------------------------------

# This method casts from the inputs to a one-row data.frame. We use it, for
# instance, when the user creates a new record by typing in values into the
# inputs, and then clicks “Submit”:
CastData <- function(data) {
  # datar <-
  data.frame(
    state       = data[["state"]],
    ma          = data[["ma"]],
    mr          = as.logical(data[["mr"]]),
    mr_name     = data[["mr_name"]],
    region      = data[["region"]],
    center      = as.logical(data[["center"]]),
    center_name = data[["center_name"]],
    p_accept    = as.numeric(data[["p_accept"]]),
    offered     = as.integer(data[["offered"]]),

    stringsAsFactors = FALSE,
    row.names        = data[["id"]]
  )
#
#   rownames(datar) <-
#   datar
}


# This creates an empty record, to be used e.g. to fill the inputs with the
# default values when the user clicks the “New” button:
CreateDefaultRecord <- function() {
  CastData(list(
    id          = "0",
    state       = "Italy",
    ma          = NA_character_,
    mr          = FALSE,
    mr_name     = NA_character_,
    region      = NA_character_,
    center      = TRUE,
    center_name = NA_character_,
    p_accept    = 100,
    offered     = 1L
  ))
}




# And this method takes the data as selected in the DataTable, and updates the
# inputs with the respective values:
UpdateInputs <- function(data, session) {
  updateTextInput(session,     "id",          value = unname(rownames(data)))
  updateTextInput(session,     "state",       value = data[["state"]])
  updateTextInput(session,     "ma",          value = data[["ma"]])
  updateCheckboxInput(session, "mr",          value = data[["mr"]])
  updateTextInput(session,     "mr_name",     value = data[["mr_name"]])
  updateTextInput(session,     "region",      value = data[["region"]])
  updateCheckboxInput(session, "center",      value = data[["center"]])
  updateTextInput(session,     "center_name", value = data[["center_name"]])
  updateSliderInput(session,   "p_accept",    value = data[["p_accept"]])
  updateSliderInput(session,   "offered",     value = data[["offered"]])
}






# This function finds the next ID of a new record. In mysql, this could be done
# by an incremental index, automatically. And then this method could be used to
# fetch the last insert ID. But here, we manage the ID ourselves
GetNextId <- function() {
  if (exists("centers_table") && nrow(centers_table) > 0L) {
    max(as.integer(rownames(centers_table))) + 1L
  } else {
    1L
  }
}





# It’s just a method that defines the names of the columns in our table:
GetTableMetadata <- function() {
  list(
    fields = c(
      id          = "Id",
      state       = "State",
      ma          = "Macroarea",
      mr          = "In macroregion?",
      mr_name     = "Macroregion",
      region      = "Region",
      center      = "Has centers?",
      center_name = "Center",
      p_accept    = "Acceptance rate",
      offered     = "Number of surplus"
    )
  )
}








# CRUD METHODS-------------------------------------------------------------
## CREATE
CreateData <- function(data) {

  data <- CastData(data)
  rownames(data) <- GetNextId()

  if (exists("centers_table")) {
    centers_table <<- rbind(centers_table, data)
  } else {
    centers_table <<- data
  }
}




## READ
ReadData <- function() {
  if (exists("centers_table")) {
    centers_table
  }
}




## UPDATE
UpdateData <- function(data) {
  data <- CastData(data)
  centers_table[row.names(centers_table) == row.names(data), ] <<- data
}





## DELETE
DeleteData <- function(data) {
centers_table <<- centers_table[row.names(centers_table) != unname(data[["id"]]), ]
}











# Create state
CreateState <- function() {
  if (!exists("centers_table")) {
    return(invisible(NULL))
  }

  my_state <<- state(centers_table[['state']][[1]],
    set_macroareas(
      purrr::map(unique(centers_table[['ma']]),

        function(ma) {
          macroarea(ma,
            set_macroregions(
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
                  )
                } # end of the region function
              ) %>%  # all region created
              ## region created, here we have to aggregate the macroregions!
              ## befor to pass them to set_macroregions()
              merge_macroregion(macro_area = ma)
            )
          )
        }
      )
    )
  )
}


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
        initial_strip = names(regions)
      )
    }
  ) %>%

  c(regions[singles])
}





# CREDITS -----------------------------------------------------------------

# https://ipub.com/shiny-crud-app/

