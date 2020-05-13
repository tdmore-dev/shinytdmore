#' Input element representing the current time
#' 
#' @param id unique identifier
#' @name nowInput
nowInputUI <- function(id) {
  ns <- NS(id)
  editableCombodate(inputId=ns("now"), value="Undefined")
}

#' @name nowInput
#' @param input input object
#' @param output output object
#' @param session session object
#' @param state reactiveValues object with value `now`. The server logic ensures a synchronization between `state$now` and the nowInput element.
#' @param debug whether to print debugging messages
nowInput <- function(input, output, session, state, debug=FALSE) {
  # synchronize input$now to state$now
  format="%Y-%m-%d %H:%M" # format: "2000-01-01 00:00"
  log <- function(...) if(debug) cat(...)
  observeEvent(state$now, {
    value <- strftime(lubridate::round_date(state$now, unit="minute"), format=format)
    log("Updating FROM STATE to HTML (value=", value, "...")
    if(!is.na(value) && !isTRUE(all.equal(input$now, value))) {
      updateEditableInput(session, "now", value)
      log("UPDATED\n")
    } else {
      log("IGNORED\n")
    }
  })
  observeEvent(input$now, {
    value <- as.POSIXct(input$now, format=format)
    log("Updating FROM HTML to STATE (value=", value, "...")
    if(!is.na(value) && !isTRUE(all.equal(lubridate::round_date(state$now, unit="minute"), value))) {
      state$now <- value
      log("UPDATED\n")
    } else {
      log("IGNORED\n")
    }
  }, ignoreInit=TRUE) #do not synchronize when just starting; updateEditableInput does not modify input$now in the same service loop
}