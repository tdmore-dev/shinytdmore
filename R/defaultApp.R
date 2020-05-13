#' Default full-featured application
#' 
#' @inheritParams shinytdmore-data
#' @param ... default values for the application (e.g. `regimen`, `observed`, etc)
#' 
#' @export
defaultApp <- function(model=tdmore::getModel(name="pheno", dir=system.file("models", package="shinytdmore")), now=Sys.time(), ...) {
  options(shiny.reactlog=TRUE)
  options(useFancyQuotes = FALSE)
  
  defaultValues <- list(model=model, now=now, ...)
  ui <- navbarPage(
    "Shinytdmore",
                   predictionTabUI("prediction", height="80vh"),
                   modelTabUI("model"),
                   aboutTabUI("about"),
    tabPanel("Debug", shinyjs::runcodeUI())
  )
  server <- function(input, output, session) {
    shinyjs::runcodeServer()
    state <- do.call(reactiveValues, defaultValues)
    callModule(predictionTab, "prediction", state)
    callModule(modelTab, "model", state)
    callModule(aboutTab, "about")
  }
  
  app <- shiny::shinyApp(ui = ui, server = server)
  app
}
