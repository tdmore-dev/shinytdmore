#'
#' Reports tab user interface.
#'
#' @param id namespace id
#' @return a panel
#' 
#' @export
#'
reportsTabUI <- function(id) {
  ns <- NS(id)
  panel <- tabPanel("Reports",
                    icon = icon("file-text"),
                    HTML("Reports tab under construction.")
                    )
  return(panel)
}

#' Reports tab server.
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' 
#' @export
#' 
reportsTab <- function(input, output, session) {
  # Nothing to do
}