library(polynom)
library(clumpr)
library(magrittr)
library(tidyverse)
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
    regpos      = as.integer(data[["regpos"]]),
    macropos    = as.integer(data[["macropos"]]),
    inmacropos  = data[["inmacropos"]],

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
    offered     = 1L,
    regpos      = NA_integer_,
    macropos    = NA_integer_,
    inmacropos  = NA_character_
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
  updateNumericInput(session,  "regpos",      value = as.integer(data[["regpos"]]))
  updateNumericInput(session,  "macropos",    value = as.integer(data[["macropos"]]))
  updateTextInput(session,     "inmacropos",  value = data[["inmacropos"]])
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
      offered     = "Number of surplus",
      regpos      = "Area-strip region-position",
      macropos    = "Area-strip macroregion-position",
      inmacropos  = "Macroregion-strip position(s)"
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









# function to merge macroregion used in the creation of the reactive state

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



# find the strip for the macroarea
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
# CREDITS -----------------------------------------------------------------

# https://ipub.com/shiny-crud-app/

