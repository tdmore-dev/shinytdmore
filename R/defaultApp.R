#' Default full-featured application
#' 
#' @param ... default values for the application (e.g. `regimen`, `observed`, etc)
#' 
#' @export
defaultApp <- function(...) {
  options(shiny.reactlog=TRUE)
  options(useFancyQuotes = FALSE)
  
  defaultValues <- list(...)
  ui <- navbarPage(
    "Shinytdmore",
                   predictionTabUI("prediction", height="80vh"),
                   modelTabUI("model"),
                   aboutTabUI("about")
                   )
  server <- function(input, output, session) {
    state <- do.call(reactiveValues, defaultValues)
    callModule(predictionTab, "prediction", state)
    callModule(modelTab, "model", state)
    callModule(aboutTab, "about")
  }
  
  app <- shiny::shinyApp(ui = ui, server = server)
  app
}
