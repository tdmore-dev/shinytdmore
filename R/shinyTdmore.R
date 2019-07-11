#' Get UI component of Shiny TDMore.
#'
#' @param title shiny app title
#' @param ... UI functions
#' @return UI component
#' @export
#' 
shinyTdmoreUI <- function(title, ...) {
  ui <- navbarPage(title, 
                   id="tabs",
                   ...,
                   inverse=TRUE,
                   collapsible = TRUE,
                   theme="bootstrap.css")
}

#' Get server component of Shiny TDMore.
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param conf modules configuration
#' @param db database, uses in-memory database if missing
#' @return server component
#' @export
#' 
shinyTdmore <- function(input, output, session, conf, db) {
  if(missing(db)) db <- InMemoryDatabase$new()
  # Change last tab
  onTabChanged <- reactiveValues()
  observe({
    isolate({onTabChanged$lastTab <- onTabChanged$currentTab})
    onTabChanged$currentTab <- input$tabs
  })
  callModule(module=conf$save$module, id=conf$save$id, onTabChanged, val, db=db)
  
  # Call module new patient dialog
  onNewPatientAdded <- reactiveValues()
  callModule(module=conf$new_patient$module, id=conf$new_patient$id, onNewPatientAdded, db=db)
  
  # Create the main reactive container
  val <- reactiveValues()
  
  # Call module patients tab
  callModule(module=conf$patients$module, id=conf$patients$id, parentSession=session, val, onNewPatientAdded, db=db)
  
  # Select first patient by default, needed for predictions tab
  isolate({
    if (is.null(val$patient)) {
      id <- DTtable$patients[1,]$ID
      setPatient(db$get(id), val) ## sensible default
    }
  })
  
  # Call module prediction tab
  callModule(module=conf$prediction$module, id=conf$prediction$id, val)

  # Call module reports tab (currently no logic)
  callModule(module=conf$reports$module, id=conf$reports$id, val)
  
  # Call module about tab (currently no logic)
  callModule(module=conf$about$module, id=conf$about$id)
  
  # Select a patient from the URL
  selectPatientFromURL(session, val)
}

#' Select patient from URL logic.
#'
#' @param session shiny session
#' @param val main reactive container
#' 
selectPatientFromURL <- function(session, val) {
  observeEvent(session$clientData$url_search, {
    query <- parseQueryString(session$clientData$url_search)
    value <- query[["patient"]]
    if (!is.null(value)) {
      patientId <- value
      patient <- db$get(patientId)
      if (!is.null(patient)) {
        setPatient(patient, val)
        updateTabsetPanel(session, "tabs", selected="Prediction")
      }
    }
  })
}