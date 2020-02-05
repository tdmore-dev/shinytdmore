#'
#' Save data of both user and covariate forms.
#'
#' @param userData data from user from, named vector
#' @param modelName model name
#' @param covariateData data from covariate form, named vector
#' @param db db
#' 
saveData <- function(userData, modelName, covariateData, db) {
  if (!is.null(covariateData)) {
    covs <- setNames(as.numeric(covariateData), names(covariateData)) # String to numeric conversion
    covs <- as.list(covs)
  } else {
    covs <- NULL
  }
  
  date <- Sys.Date()
  time <- c("08:00")
  now <- Sys.time()

  patient <- createPatient(firstname = userData[["firstname"]], lastname = userData[["lastname"]])
  patient <- updatePatientModel(patient, modelName)
  formulations <- tdmore::getMetadataByClass(get(modelName),"tdmore_formulation")
  doseMetadata <- tdmore::getMetadataByName(get(modelName), formulations[[1]]$name)
  dose <- if(is.null(doseMetadata)) {0} else {doseMetadata$default_value}
  formulation <- if(is.null(doseMetadata)) {"Unknown"} else {doseMetadata$name}
    
  # Add by default a first dose at 8am
  doses <- tibble(date=date, time=time, dose=dose, formulation=doseMetadata$name, fix=F)
    
  # Create a empty measure data frame
  measures <- tibble(date=date, time=character(), measure=numeric())
    
  patient <- updatePatientMeasures(patient, measures)
  patient <- updatePatientDoses(patient, doses)
  patient <- updateNowDate(patient, now)
  patient <- updateCovariates(patient, covs, date, time)

  # Add patient into DB
  db$add(patient)
}

#' Update covariates in patient model (convert numeric vector to tibble).
#' 
#' @param patient patient model
#' @param covs numeric vector (or null if no covariates)
#' @param date date
#' @param time time (character)
#' @return an updated patient model
#' 
updateCovariates <- function(patient, covs, date, time) {
  if (!is.null(covs)) {
    covariates <- tibble::tibble(date=date, time=time)
    covariates <- bind_cols(covariates, as.list(covs))
    patient <- updatePatientCovariates(patient, covariates)
  }
  return(patient)
}

#' 
#' Create the user form.
#' 
#' @param ns namespace
#' 
createUserForm <- function(ns) {
  list(textInput(inputId = ns("firstname"), label = "Firstname", value = ""),
       textInput(inputId = ns("lastname"), label = "Lastname", value = ""))
}

#' 
#' Create a patient form in a modal dialog.
#' 
#' @param id namespace id
#' 
#' @export
#'
newPatientDialogUI <- function(id) {
  ns <- NS(id)
  modalDialog(
    h4("Patient"),
    createUserForm(ns),
    h4("Model"),
    selectInput(ns("modelCombobox"), "Choose your model", getModelList()),
    h4("Covariates"),
    tags$div(id="placeholder"),
    footer = tagList(
      actionButton(ns("modalFormCancel"), "Cancel"),
      actionButton(ns("modalFormOK"), "OK")
    )
  )
}

#'
#' Hack select input (update it with fake values when dialog is closed).
#' Without this workaround, covariate inputs do not appear when 'new patient' is clicked for the second time.
#'
#' @param session shiny session
#'
hackSelectInput <- function(session) {
  updateSelectInput(session, "modelCombobox",
                    label = "Choose your model",
                    choices = c("fake_value_1", "fake_value_2"),
                    selected = "fake_value_2")
}

#'
#' New patient dialog server.
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param onNewPatientAdded reactive value
#' @param db database
#' 
#' @export
#'
newPatientDialog <- function(input, output, session, onNewPatientAdded, db) {
  # Modal form OK button
  observeEvent(input$modalFormOK, {
      # Retrieve user form data
      userFormData <- reactive({
        fields <- c("firstname", "lastname")
        return(unlist(sapply(fields, function(x) input[[x]])))
      })
      
      # Retrieve covariate form data
      covariateFormData <- reactive({
        model <- get(input$modelCombobox)
        covariates <- getCovariateNames(model)
        data <- sapply(covariates, function(x) {
          str <- input[[x]]
        })
        return(unlist(data))
      })
      
      userData <- userFormData()
      covariateData <- covariateFormData()
      valuesAllNumeric <- length(which(is.na(suppressWarnings(as.numeric(covariateData))))) == 0
      
      if (valuesAllNumeric) {
        model <- get(input$modelCombobox)
        saveData(userData, input$modelCombobox, covariateData, db)
        
        # Use of a reactive value to trigger patients table refresh in patientsTab
        if (is.null(onNewPatientAdded$trigger)) {
          onNewPatientAdded$trigger <- 1
        } else {
          onNewPatientAdded$trigger <- onNewPatientAdded$trigger + 1
        }
        
        # Close modal dialog
        hackSelectInput(session)
        removeModal(session)
      } else {
        selectedModel <- input$modelCombobox
        ## TODO: we should modify the existing one with a FAILED state, instead of showing a NEW modal dialog...
        showModal(newPatientDialogUI(id="newPatientDialogId"))
        hackSelectInput(session)
        updateSelectInput(session, "modelCombobox",
                          label = "Choose your model",
                          choices = getModelList(),
                          selected = selectedModel)
      }
  })
  
  # Modal form Cancel button
  observeEvent(input$modalFormCancel, {
    # Close modal dialog
    hackSelectInput(session)
    removeModal(session)
  })
}
