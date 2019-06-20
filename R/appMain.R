#' Get UI component of Shiny TDMore.
#'
#' @return UI component
#' @export
#' 
shinyTdmoreUI <- function() {
  ui <- navbarPage("TDMore mockup", 
                   id="tabs",
                   getPatientsTabPanel(),
                   getPredictionTabPanel(),
                   getReportsTabPanel(),
                   aboutTabUI("aboutTabId"),
                   inverse=TRUE,
                   collapsible = TRUE)
}

#' Get server component of Shiny TDMore.
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @return server component
#' @export
#' 
shinyTdmoreServer <- function(input, output, session) {
  
  # Call module new patient dialog
  onNewPatientAdded <- reactiveValues()  
  callModule(module=newPatientDialog, id="newPatientDialogId", nsId="newPatientDialogId", onNewPatientAdded)
  
  # Create the main reactive container
  val <- reactiveValues()
  
  # Patients tab server
  patientsTabServer(input, output, session, val, onNewPatientAdded)
  
  # Select first patient by default, needed for predictions tab
  isolate({
    if (is.null(val$patient)) {
      id <- DTtable$patients[1,]$ID
      setPatient(getPatient(id), val) ## sensible default
    }
  })
  
  # Prediction tab server
  predictionTabServer(input, output, session, val)
  
  # Save project server
  saveProjectServer(input, val)
  
  # Call module about tab (currently no logic)
  callModule(module=aboutTab, id="aboutTabId")
}