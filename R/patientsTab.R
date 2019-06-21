#'
#' Patients tab user interface.
#'
#' @param id namespace id
#' @return a panel
#' 
#' @export
#'
patientsTabUI <- function(id) {
  ns <- NS(id)
  panel <- tabPanel(
    "Patients",
    icon = icon("users"),
    titlePanel("Patients"),
    DTOutput(ns("patientTable")),
    hr(),
    actionButton(ns("newPatientButton"), "New patient")
  )
  return(panel)
}

#'
#' Collect data from the mongoDB.
#'
#' @param ns namespace
#'
initDB <- function(ns) {
  initiateDb()
  patients <- getAllPatients()
  nb <- nrow(patients)
  patients$Name <- shinyInput(FUN=actionLink, id=ns("viewPatientButton"), label=paste(patients$firstname, patients$lastname))
  patients$NameNoHyperlink <- paste(patients$firstname, patients$lastname)
  patients$Admitted <- POSIXToPrettyString(stringToPOSIX(patients$created_at))
  patients$ID <- patients$id
  patients$Remove <- shinyInput(FUN=actionButton, id=ns("removePatientButton"), label=rep("",nb), icon=icon("trash-alt"))
  DTtable <<- reactiveValues(patients = patients %>% select(ID, NameNoHyperlink, Name, Admitted, Remove))
}

#'
#' Render the patients table.
#'
#' @param input the shiny input object
#' @param ns namespace
#'
renderPatientTable <- function(input, ns) {
  DT::renderDataTable(expr = {
    input$modalFormOK
    input$removePatientButton
    initDB(ns)
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
  
  # Copy model
  modelId <- patient$model
  model <- get(modelId)
  if(is.null(model)) stop("Model ", modelId, " not available...")
  val$model <- model
  
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
#' @param parentSession shiny parent session
#' @param val main reactive container
#' @param onNewPatientAdded reactive value
#' 
#' @export
#'
patientsTab <- function(input, output, session, parentSession, val, onNewPatientAdded) {
  ns <- session$ns
  
  # Unnecessary call but currently needed to have at least 1 patient in the table
  initDB(ns)
  
  # Open patient form dialog if new patient button is clicked
  observeEvent(input$newPatientButton, {
    showModal(newPatientDialogUI(id="newPatientDialogId"))
  })
  
  # Remove patient button
  observeEvent(input$removePatientButton, {
    row <- as.numeric(strsplit(input$removePatientButton, "_")[[1]][2])
    patientRow <- DTtable$patients[row,]
    patientName <- patientRow$NameNoHyperlink
    showModal(removePatientConfirmationDialog(patientName, ns))
  })
  
  observeEvent(input$removePatientConfirmationDialogCancel, {
    removeModal(session)
  })
  
  observeEvent(input$removePatientConfirmationDialogOK, {
    row <- as.numeric(strsplit(input$removePatientButton, "_")[[1]][2])
    patientRow <- DTtable$patients[row,]
    print(paste0("Remove patient ", patientRow$NameNoHyperlink, " (ID=", patientRow$ID, ")"))
    removePatient(as.numeric(patientRow$ID))
    removeModal(session)
    output$patientTable <- renderPatientTable(input, ns)
  })
  
  observeEvent(onNewPatientAdded$trigger, {
    output$patientTable <- renderPatientTable(input, ns)
  })
  
  # View button
  observeEvent(input$viewPatientButton, {
    row <- as.numeric(strsplit(input$viewPatientButton, "_")[[1]][2])
    patientRow <- DTtable$patients[row,]
    print(paste0("View patient ", patientRow$NameNoHyperlink, " (ID=", patientRow$ID, ")"))
    setPatient(getPatient(as.numeric(patientRow$ID)), val)
    updateTabsetPanel(parentSession, "tabs", selected="Prediction")
  })
  
  # Render the patients table
  output$patientTable <- renderPatientTable(input, ns)
}

#'
#' Remove patient confirmation dialog.
#'
#' @param patientName the patient name
#' @param ns namespace
#' @return a modal dialog
#'
removePatientConfirmationDialog <- function(patientName, ns) {
  modalDialog(
    h3("Confirmation"),
    hr(),
    h5(paste0("Are you sure to remove ", patientName, "?")),
    footer = tagList(
      actionButton(ns("removePatientConfirmationDialogCancel"), "Cancel"),
      actionButton(ns("removePatientConfirmationDialogOK"), "OK")
    )
  )
}
