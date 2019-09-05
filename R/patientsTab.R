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
    DT::DTOutput(ns("patientTable")),
    hr(),
    actionButton(ns("newPatientButton"), "New patient")
  )
  return(panel)
}

#'
#' Collect data from the mongoDB.
#'
#' @param ns namespace
#' @param db database
#' @importFrom purrr map_dfr
#'
initDataTable <- function(ns, db) {
  displayPatientsDf <- data.frame(
    ID=character(),
    NameNoHyperlink=character(),
    Name=character(),
    Admitted=character(),
    Remove=character()
  )
  
  patients <- db$patients
  if(length(patients) > 0) {
    patients <- patients %>%
      purrr::map_dfr(function(x) {tibble::as_tibble(x[c("firstname", "lastname", "created_at", "id", "private")])}) %>%
      dplyr::filter(!private) # Only show patients who are not 'private'
    nb <- nrow(patients)
    
    patients$Name <- shinyInput(FUN=actionLink, id=ns("viewPatientButton"), label=paste(patients$firstname, patients$lastname))
    patients$NameNoHyperlink <- paste(patients$firstname, patients$lastname)
    patients$Admitted <- patients$created_at %>% purrr::map(stringToPOSIX) %>% purrr::map(POSIXToPrettyString)
    patients$ID <- patients$id
    patients$Remove <- shinyInput(FUN=actionButton, id=ns("removePatientButton"), label=rep("",nb), icon=icon("trash-alt"))
    displayPatientsDf <- patients %>% dplyr::select(ID, NameNoHyperlink, Name, Admitted, Remove)
  }
  DTtable <<- reactiveValues(patients = displayPatientsDf)
}

#'
#' Render the patients table.
#'
#' @param input the shiny input object
#' @param ns namespace
#' @param db database
#'
renderPatientTable <- function(input, ns, db) {
  DT::renderDataTable(expr = {
    input$modalFormOK
    input$removePatientButton
    initDataTable(ns, db)
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
#' @export
#'
setPatient <- function(patient, val) {
  val$patient <- patient

  # Copy model
  model_id <- patient$model
  model <- get(model_id)
  if(is.null(model)) stop("Model ", model_id, " not available...")
  val$model <- model
  val$model_id <- model_id
  
  # Copy covariates
  val$covs <- autoSortByDate(patient$covariates) # Auto-sort the first time
  
  # Copy doses
  val$doses <- autoSortByDate(patient$doses) # Auto-sort the first time
  
  # Copy measures
  obs <- patient$measures

  if(nrow(obs) == 0){
    obs$use <- logical() # Add column type
  } else {
    obs$use <- TRUE # All measures used by default
  }
  val$obs <- autoSortByDate(obs) # Auto-sort the first time
  
  # Copy now_date
  val$now <- patient$now_date
  
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
#' @param db database
#' 
#' @export
#'
patientsTab <- function(input, output, session, parentSession, val, onNewPatientAdded, db) {
  ns <- session$ns
  
  # Unnecessary call but currently needed to have at least 1 patient in the table
  initDataTable(ns, db)
  
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
    db$remove(patientRow$ID)
    removeModal(session)
    output$patientTable <- renderPatientTable(input, ns, db)
  })
  
  observeEvent(onNewPatientAdded$trigger, {
    output$patientTable <- renderPatientTable(input, ns, db)
  })
  
  # View button
  observeEvent(input$viewPatientButton, {
    row <- as.numeric(strsplit(input$viewPatientButton, "_")[[1]][2])
    patientRow <- DTtable$patients[row,]
    print(paste0("View patient ", patientRow$NameNoHyperlink, " (ID=", patientRow$ID, ")"))
    setPatient(db$get(patientRow$ID), val)
    updateTabsetPanel(parentSession, "tabs", selected="Prediction")
  })
  
  # Render the patients table
  output$patientTable <- renderPatientTable(input, ns, db)
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
