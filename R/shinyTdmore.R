#' Launches the shiny app for a single patient
#' 
#' @export
showPatient <- function(patient) {
  dir <- tempfile()
  dir.create(dir)
  db <- FileDatabase$new(dir)
  
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
    shinyTdmore(input, output, session, conf, db)
  }
  
  app <- shiny::shinyApp(ui = ui, server = server)
  shiny::runGadget(app)
}

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

#' Server component of Shiny TDMore.
#' 
#' The server expects the following UI elements:
#' - input$tabs
#' - 
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param conf modules configuration
#' This is a named list of extra shiny modules to load. Each module will receive the arguments 
#' `val` (a reactiveValues() set) and `db` (the database)
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
  
  # Call module new patient dialog
  onNewPatientAdded <- reactiveValues()
  callModule(module=conf$new_patient$module, id=conf$new_patient$id, onNewPatientAdded, db=db)
  
  # Create the main reactive container
  val <- reactiveValues(
    doses=tibble(date=numeric(), time=numeric(), dose=numeric(), fix=logical()),
    obs=tibble(date=numeric(), time=numeric(), measure=numeric(), use=logical()),
    now=Sys.time()
    )
  
  # Call module patients tab
  callModule(module=conf$patients$module, id=conf$patients$id, parentSession=session, val, onNewPatientAdded, db=db)
  
  # Call module prediction tab
  callModule(module=conf$prediction$module, id=conf$prediction$id, val)
  
  # Call model prediction tab
  callModule(module=conf$model$module, id=conf$model$id, val, onTabChanged)
  
  # Call module reports tab (currently no logic)
  callModule(module=conf$reports$module, id=conf$reports$id, val)
  
  # Call module about tab (currently no logic)
  callModule(module=conf$about$module, id=conf$about$id)
  
  # Call save module
  callModule(module=conf$save$module, id=conf$save$id, onTabChanged, val, db=db)
  
  if(is.null(conf$selectPatient)) {
    conf$selectPatient <- list(module=selectPatient, id="selectPatient")
  }
  # Select a patient (last in DB or from URL)
  callModule(module=conf$selectPatient$module, id=conf$selectPatient$id, val, db=db)
}

#' Select patient (last in DB or from URL).
#'
#' @param session shiny session
#' @param val main reactive container
#' @param db database
#' 
selectPatient <- function(input, output, session, val, db) {
  observeEvent(session$clientData$url_search, {
    query <- parseQueryString(session$clientData$url_search)
    value <- query[["patient"]]
    updated <- F
    if (!is.null(value)) {
      patientId <- value
      patient <- db$get(patientId)
      if (!is.null(patient)) {
        setPatient(patient, val)
        updateTabsetPanel(session, "tabs", selected="Prediction")
        updated <- T
      }
    }
    if (!updated && is.null(val$patient)) {
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