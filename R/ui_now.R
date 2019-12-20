#' Input element representing the current time
#' 
#' @param id unique identifier
#' @name nowInput
nowInputUI <- function(id) {
  ns <- NS(id)
  editableCombodate(inputId=ns("now"), value="2000-01-01 00:00")
}

#' @name nowInput
#' @param input input object
#' @param output output object
#' @param session session object
#' @param state reactiveValues object with value `now`. The server logic ensures a synchronization between `state$now` and the nowInput element.
nowInput <- function(input, output, session, state) {
  # synchronize input$now to state$now
  format="%Y-%m-%d %H:%M" # format: "2000-01-01 00:00"
  observeEvent(state$now, {
    value <- strftime(state$now, format=format)
    if(!is.na(value) && !isTRUE(all.equal(input$now, value))) updateEditableInput(session, "now", value)
  })
  observeEvent(input$now, {
    value <- as.POSIXct(input$now, format=format)
    if(!is.na(value) && !isTRUE(all.equal(state$now, value))) state$now <- value
  })
}