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

#' Server component of shinytdmore
#' 
#' @details 
#' This function is the core server function for shiny tdmore. It expects a named list with modules to be called.
#' All modules are called with arguments `id`, `state` and `db`.
#' 
#' `db` represents a backend database connection. If missing, an In-Memory database is used.
#' 
#' Every application session uses a reactiveValues `state` that captures
#' the application state. All modules can use this `state` to communicate amongst each other.
#' 
#' `state` contains the following values:
#' - `doses`
#' - `obs`
#' - `now`
#' - `currentTab`
#' - `onNewPatientAdded`
#' 
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param conf named list with modules
#' @param db database
#' @return server component
#' @export
#' 
shinyTdmore <- function(input, output, session, modules, db=InMemoryDatabase$new()) {
  # Create the main reactive container
  val <- reactiveValues(
    doses=tibble(date=numeric(), time=numeric(), dose=numeric(), fix=logical()),
    obs=tibble(date=numeric(), time=numeric(), measure=numeric(), use=logical()),
    now=Sys.time(),
    currentTab=NULL,
    onNewPatientAdded=0
  )
  
  # Change last tab
  observe({
    val$currentTab <- input$tabs
  })
  
  for(id in names(modules)) {
    callModule( module=modules[[id]], id=id, val=val, db=db)
  }
}

#' Select patient (last in DB or from URL).
#'
#' @param session shiny session
#' @param val main reactive container
#' @param db database
#' @export
#' 
loadPatientFromUrl <- function(input, output, session, val, db) {
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
    if (is.null(val$patient)) {
      if(nrow(DTtable$patients) > 0) {
        id <- DTtable$patients[1,]$ID
        patient <- db$get(id)
      } else {
        patient <- createFakePatient()
      }
      setPatient(patient, val) ## sensible default
    }
  })
}