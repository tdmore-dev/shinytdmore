### This file contains a default, full-featured application using the shinytdmore framework

#' List of default server modules
#' @export
defaultApp <- function(db) {
  if(missing(db)) db <- InMemoryDatabase$new()
  
  list(selectPatient=loadPatientFromUrl)
}

#' Launches the shiny app for a single patient
#' @export
showPatient <- function(patient) {
  ui <- predictionTabUI(id="predictionTabId")
  
  conf <- list(save=list(module=saveProject, id="saveProjectId"),
               new_patient=list(module=newPatientDialog, id="newPatientDialogId"),
               patients=list(module=patientsTab, id="patientsTabId"),
               prediction=list(module=predictionTab, id="predictionTabId"),
               model=list(module=modelTab, id="modelTabId"),
               reports=list(module=reportsTab, id="reportsTabId"),
               about=list(module=aboutTab, id="aboutTabId"))
  conf$selectPatient <- list(module=function(input, output, session, val, db) {
    observeEvent(session$clientData, {
      setPatient(patient, val)
    })
  }, id="selectPatient")
  
  server <- function(input, output, session) {
    shinyTdmore(input, output, session, conf)
  }
  
  app <- shiny::shinyApp(ui = ui, server = server)
  shiny::runGadget(app)
}

