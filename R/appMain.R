#' Get UI component of Shiny TDMore.
#'
#' @return UI component
#' @export
#' 
shinyTdmoreUI <- function() {
  ui <- navbarPage("TDMore mockup", 
                   id="tabs",
                   patientsTabUI(id="patientsTabId"),
                   predictionTabUI(id="predictionTabId"),
                   reportsTabUI(id="reportsTabId"),
                   aboutTabUI(id="aboutTabId"),
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
  # Change last tab
  onTabChanged <- reactiveValues()
  observe({
    isolate({onTabChanged$lastTab <- onTabChanged$currentTab})
    onTabChanged$currentTab <- input$tabs
  })
  callModule(module=saveProject, id="saveProjectId", onTabChanged, val)
  
  # Call module new patient dialog
  onNewPatientAdded <- reactiveValues()
  callModule(module=newPatientDialog, id="newPatientDialogId", onNewPatientAdded)
  
  # Create the main reactive container
  val <- reactiveValues()
  
  # Call module patients tab
  callModule(module=patientsTab, id="patientsTabId", parentSession=session, val, onNewPatientAdded)
  
  # Select first patient by default, needed for predictions tab
  isolate({
    if (is.null(val$patient)) {
      id <- DTtable$patients[1,]$ID
      setPatient(getPatient(id), val) ## sensible default
    }
  })
  
  # Call module prediction tab
  callModule(module=predictionTab, id="predictionTabId", val)

  # Call module reports tab (currently no logic)
  callModule(module=reportsTab, id="reportsTabId")
  
  # Call module about tab (currently no logic)
  callModule(module=aboutTab, id="aboutTabId")
}