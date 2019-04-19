#' Get UI component of Shiny TDMore.
#'
#' @return UI component
#' @export
#' 
shinyTdmoreUI <- function() {
  options(testShinyTdmore=F) # Make sure test mode is disabled when UI is created
  ui <- navbarPage("TDMore mockup", 
                   id="tabs",
                   getPatientsTabPanel(),
                   getPredictionTabPanel(),
                   getReportsTabPanel(),
                   getAboutTabPanel(),
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
  # Patient form dialog server
  newPatientDialogServer(input, output, session)
  
  # Main reactive container
  val <- reactiveValues()
  
  # Patients tab server
  patientsTabServer(input, output, session, val)
  
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
}