library(shiny)

# Helper functions --------------------------------------------------------

# This method casts from the inputs to a one-row data.frame. We use it, for
# instance, when the user creates a new record by typing in values into the
# inputs, and then clicks “Submit”:
CastData <- function(data) {
  datar <- data.frame(
    state       = data[["state"]],
    ma          = data[["ma"]],
    mr          = as.logical(data[["mr"]]),
    mr_name     = data[["mr_name"]],
    region      = data[["region"]],
    center      = as.logical(data[["center"]]),
    center_name = data[["center_name"]],
    p_accept    = as.numeric(data[["p_accept"]]),
    offered     = as.integer(data[["offered"]]),

    stringsAsFactors = FALSE
  )

  rownames(datar) <- data[["id"]]
  datar
}

# This creates an empty record, to be used e.g. to fill the inputs with the
# default values when the user clicks the “New” button:
CreateDefaultRecord <- function() {
  CastData(list(
    id          = "0",
    state       = "Italy",
    ma          = "",
    mr          = FALSE,
    mr_name     = "",
    region      = "",
    center      = TRUE,
    center_name = "",
    p_accept    = 100,
    offered     = 1L
  ))
}

# And this method takes the data as selected in the DataTable, and updates the
# inputs with the respective values:
UpdateInputs <- function(data, session) {
  updateTextInput(session,     "id",          value = unname(rownames(data)))
  updateTextInput(session,     "state",       value = unname(data[["state"]]))
  updateTextInput(session,     "ma",          value = unname(data[["ma"]]))
  updateCheckboxInput(session, "mr",          value = as.logical(data[["mr"]]))
  updateTextInput(session,     "mr_name",     value = unname(data[["mr_name"]]))
  updateTextInput(session,     "region",      value = unname(data[["region"]]))
  updateCheckboxInput(session, "center",      value = as.logical(data[["center"]]))
  updateTextInput(session,     "center_name", value = unname(data[["center_name"]]))
  updateSliderInput(session,   "p_accept",    value = as.numeric(data[["p_accept"]]))
  updateSliderInput(session,   "offered",     value = as.integer(data[["offered"]]))


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


# CRUD METHODS-------------------------------------------------------------
# CREATE
CreateData <- function(data) {
  data <- CastData(data)
  rownames(data) <- GetNextId()
  if (exists("centers_table")) {
    centers_table <<- dplyr::bind_rows(centers_table, data)
  } else {
    centers_table <<- data
  }
}

# READ
ReadData <- function() {
  if (exists("centers_table")) {
    centers_table
  }
}

# UPDATE
UpdateData <- function(data) {
  data <- CastData(data)
  centers_table[row.names(centers_table) == row.names(data), ] <<- data
}

# DELETE
DeleteData <- function(data) {
centers_table <<- centers_table[row.names(centers_table) != unname(data[["id"]]), ]
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







# CREDITS -----------------------------------------------------------------

# https://ipub.com/shiny-crud-app/

