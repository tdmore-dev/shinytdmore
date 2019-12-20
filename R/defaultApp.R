#' Default full-featured application
#' @export
defaultApp <- function(...) {
  defaultValues <- list(...)
  ui <- navbarPage(
    "Shinytdmore",
                   predictionTabUI("prediction"),
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
}

#' Launches the shiny app for a single patient as a gadget
#' This allows you to easily explore a patient data.
#' @export
showPatient <- function(patient) {
  ui <- predictionTabUI(id="predictionTabId")
  server <- function(input, output, session) {
    state <- reactiveValues()
    setPatient(patient, val)
    callModule(predictionTab, state)
  }
  
  app <- shiny::shinyApp(ui = ui, server = server)
  shiny::runGadget(app)
}

