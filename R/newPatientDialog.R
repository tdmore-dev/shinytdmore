#'
#' Save data of both user and covariate forms.
#'
#' @param userData data from user from, named vector
#' @param modelName model name
#' @param covariateData data from covariate form, named vector
#' 
saveData <- function(userData, modelName, covariateData) {
  # See if patient already exists (in this case, model and covariates will be updated)
  patient <- getPatient(firstname = userData[["firstname"]], lastname = userData[["lastname"]])
  if (!is.null(covariateData)) {
    covs <- setNames(as.numeric(covariateData), names(covariateData)) # String to numeric conversion
    covs <- as.list(covs)
  } else {
    covs <- NULL
  }
  
  if (is.null(patient)) {
    patient <- createPatient(firstname = userData[["firstname"]], lastname = userData[["lastname"]])
    patient <- updatePatientModel(patient, modelName, covs)
    tmp <- createFakePatient()
    patient <- updatePatientDoses(patient, tmp$doses) # Temporarily, app needs at least a dose
    patient <- updatePatientMeasures(patient, tmp$measures) # Temporarily, app needs at least a measure
    addPatient(patient)
  } else {
    patient <- updatePatientModel(patient, modelName, covs)
    updatePatient(patient$id, patient)
  }
}

#' 
#' Create the user form.
#' 
createUserForm <- function() {
  list(textInput(inputId = "firstname", label = "Firstname", value = ""),
       textInput(inputId = "lastname", label = "Lastname", value = ""))
}

#' 
#' Create the covariate form.
#' 
#' @param input shiny input
#'
createCovariateForm <- function(input) {
  model <- get(input$modelCombobox)
  covariates <- model$covariates
  retValue <- NULL
  for (covariate in covariates) {
    retValue <- list(retValue, covariate=textInput(inputId = covariate, label = covariate, value = ""))
  }
  retValue
}

#' 
#' Create a patient form in a modal dialog.
#' 
#' @param failed logical value, can be true if there was an error in the form
#'
patientFormModalDialog <- function(failed = FALSE) {
  modalDialog(
    h4("Patient"),
    createUserForm(),
    h4("Model"),
    selectInput("modelCombobox", "Choose your model", getModelList()),
    h4("Covariates"),
    tags$div(id="placeholder"),
    if (failed)
      div(tags$b("Do something", style = "color: red;")),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("modalFormOK", "OK")
    )
  )
}

#'
#' Get the list of available models in this package.
#'
#' @return a list of all model names
#'
getModelList <- function() {
  models <- data(package = "shinytdmore", envir = envir)$results[, "Item"]
  toDelete <- c()
  return(models[!(models %in% toDelete)])
}

#'
#' New patient dialog server.
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#'
newPatientDialogServer <- function(input, output, session) {
  
  # Observe event on modal window
  observeEvent(input$modalFormOK, {
    if (TRUE) {
      # Retrieve user form data
      userFormData <- reactive({
        fields <- c("firstname", "lastname")
        return(unlist(sapply(fields, function(x) input[[x]])))
      })
      
      # Retrieve covariate form data
      covariateFormData <- reactive({
        model <- get(input$modelCombobox)
        covariates <- model$covariates
        data <- sapply(covariates, function(x) {
          str <- input[[x]]
          if(str == "") NULL else str
        })
        return(unlist(data))
      })
      saveData(userFormData(), input$modelCombobox, covariateFormData())
      removeModal()
    } else {
      showModal(dataModal(failed = TRUE))
    }
  })
  
  # Observe combobox
  observeEvent(input$modelCombobox, {
    removeUI(
      selector = "#my_cov_form"
    )
    insertUI(
      selector = "#placeholder",
      where = "afterEnd",
      ui = tags$div(createCovariateForm(input), id="my_cov_form")
    )
  })
}
