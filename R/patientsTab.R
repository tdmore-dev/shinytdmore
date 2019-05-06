#'
#' Get the patients tab panel.
#'
#' @return a panel
#'
getPatientsTabPanel <- function() {
  panel <- tabPanel(
    "Patients",
    icon = icon("users"),
    titlePanel("Patients"),
    DTOutput('patientTable'),
    hr(),
    actionButton("newPatientButton", "New patient")
  )
  return(panel)
}

#'
#' Collect data from the mongoDB.
#'
#'
initDB <- function() {
  initiateDb()
  patients <- getAllPatients()
  nb <- nrow(patients)
  patients$Name <- shinyInput(FUN=actionLink, id="viewPatientButton", label=paste(patients$firstname, patients$lastname))
  patients$NameNoHyperlink <- paste(patients$firstname, patients$lastname)
  patients$Admitted <- patients$created_at
  patients$ID <- patients$id
  patients$Remove <- shinyInput(FUN=actionButton, id="removePatientButton", label=rep("",nb), icon=icon("trash-alt"))
  DTtable <<- reactiveValues(patients = patients %>% select(ID, NameNoHyperlink, Name, Admitted, Remove))
}

#'
#' Render the patients table.
#'
#' @param input the shiny input object
#'
renderPatientTable <- function(input) {
  DT::renderDataTable(expr = {
    input$modalFormOK
    input$removePatientButton
    initDB()
    return(DTtable$patients)
  }, escape = FALSE, selection = 'single', options = list(
    info = FALSE, paging = FALSE, scrollX = FALSE, scrollY = FALSE,
    columnDefs = list(list(visible = FALSE, targets = c(0, 1))) # Hide ID, NameNoHyperlink columns
  ), rownames= FALSE)
}

#'
#' Set patient to shiny app.
#'
#' @param patient the patient to be set
#' @param val main reactive container
#'
setPatient <- function(patient, val) {
  val$patient <- patient
  val$plot_type <- "population" # default
  
  # Copy model
  modelId <- patient$model
  envir <- new.env()
  data(list=modelId, package="shinytdmore", envir=envir)
  if(is.null(envir[[modelId]])) stop("Model ", modelId, " not available...")
  val$model <- envir[[modelId]]
  
  # Copy covariates
  val$covs <- patient$covariates
  
  # Copy doses
  db_dose <- patient$doses
  val$db_dose <- autoSortByDate(db_dose) # Auto-sort the first time
  
  # Copy measures
  db_obs <- val$patient$measures
  if(nrow(db_obs) == 0){
    db_obs$use <- logical() # Add column type
  } else {
    db_obs$use <- TRUE # All measures used by default
  }
  val$db_obs <- autoSortByDate(db_obs) # Auto-sort the first time
  
  # Copy now_date
  val$now_date <- patient$now_date
  
  # Define target
  target <- getMetadataByName(val$model, "TARGET")
  if(!is.null(target)) {
    val$target <- list(min=target$min, max=target$max)
  } else {
    val$target <- list(min=10, max=15) # Default values
  }
  
  # Set patient counter
  if(is.null(val$set_patient_counter)) {
    val$set_patient_counter <- 1
  } else {
    val$set_patient_counter <- val$set_patient_counter + 1
  }
}

#'
#' Patients tab server.
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param val main reactive container
#'
patientsTabServer <- function(input, output, session, val) {
  # Unnecessary call but currently needed to have at least 1 patient in the table
  initDB()
  
  # Open patient form dialog if new patient button is clicked
  observeEvent(input$newPatientButton, {
    showModal(patientFormModalDialog())
  })
  
  # Remove patient button
  observeEvent(input$removePatientButton, {
    row <- as.numeric(strsplit(input$removePatientButton, "_")[[1]][2])
    patientRow <- DTtable$patients[row,]
    print(paste0("Remove patient ", patientRow$NameNoHyperlink, " (ID=", patientRow$ID, ")"))
    removePatient(as.numeric(patientRow$ID))
    output$DTtable <- renderTable(input)
  })
  
  # View button
  observeEvent(input$viewPatientButton, {
    row <- as.numeric(strsplit(input$viewPatientButton, "_")[[1]][2])
    patientRow <- DTtable$patients[row,]
    print(paste0("View patient ", patientRow$NameNoHyperlink, " (ID=", patientRow$ID, ")"))
    setPatient(getPatient(as.numeric(patientRow$ID)), val)
    updateTabsetPanel(session, "tabs", selected="Prediction")
  })
  
  # Render the patients table
  output$patientTable <- renderPatientTable(input)
}
