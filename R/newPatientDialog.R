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
    doseMetadata <- getMetadataByName(get(modelName), "DOSE")
    dose=if(is.null(doseMetadata)) {0} else {doseMetadata$default_value}
    
    # Add by default a first dose at 8am
    doses <- tibble(date=Sys.Date(), time=c("08:00"), dose=dose)
    
    # Create a empty measure data frame
    measures <- tibble(date = date(), time = character(), measure=numeric())
    
    patient <- updatePatientMeasures(patient, measures)
    patient <- updatePatientDoses(patient, doses)
    patient <- updateNowDate(patient, Sys.time())
    addPatient(patient)
  } else {
    patient <- updatePatientModel(patient, modelName, covs)
    updatePatient(patient$id, patient)
  }
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
#' Create the covariate form.
#' 
#' @param ns namespace
#' @param input shiny input
#'
createCovariateForm <- function(ns, input) {
  if (!(input$modelCombobox %in% getModelList())) {
    return()
  }
  model <- get(input$modelCombobox)
  covariates <- getCovariateNames(model)
  retValue <- NULL
  for (covariate in covariates) {
    metadata <- getMetadataByName(model, covariate)
    if (inherits(metadata, "tdmore_covariate")) {
      choices <- metadata$choices
      if (is.null(choices)) {
        component <- sliderInput(inputId=ns(covariate), label=metadata$label, min=metadata$min, max=metadata$max, value=(metadata$min+metadata$max)/2)
      } else {
        component <- selectInput(inputId=ns(covariate), label=metadata$label, choices=choices)
      }
    } else {
      component <- textInput(inputId=ns(covariate), label=covariate, value="")
    }
    retValue <- list(retValue, covariate=component)
  }
  return(retValue)
}

#' 
#' Create a patient form in a modal dialog.
#' 
#' @param id namespace id
#' @param failed logical value, can be true if there was an error in the form
#'
newPatientDialogUI <- function(id, failed=FALSE) {
  ns <- NS(id)
  modalDialog(
    h4("Patient"),
    createUserForm(ns),
    h4("Model"),
    selectInput(ns("modelCombobox"), "Choose your model", getModelList()),
    h4("Covariates"),
    tags$div(id="placeholder"),
    if (failed)
      div(tags$b("Some covariates are missing or not numeric", style = "color: red;")),
    footer = tagList(
      actionButton(ns("modalFormCancel"), "Cancel"),
      actionButton(ns("modalFormOK"), "OK")
    )
  )
}

#'
#' Get the list of available models in this package.
#'
#' @return a list of all model names
#'
getModelList <- function() {
  models <- modelLibrary
  toDelete <- c()
  return(models[!(models %in% toDelete)])
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
#' @param nsId namespace id
#' @param onNewPatientAdded reactive value
#'
newPatientDialog <- function(input, output, session, nsId, onNewPatientAdded) {
  ns <- NS(nsId)
  
  # Modal form OK button
  observeEvent(input$modalFormOK, {
      print("OK CLICKED")
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
        if (inherits(model, "tdmore_mpc")) {
          covariateData <- c(covariateData, model$mpc_theta)
        }
        saveData(userData, input$modelCombobox, covariateData)
        
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
        showModal(newPatientDialogUI(id="newPatientDialogId", failed = TRUE))
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
  
  # Observe combobox
  observeEvent(input$modelCombobox, {
    removeUI(
      selector = "#my_cov_form"
    )
    insertUI(
      selector = "#placeholder",
      where = "afterEnd",
      ui = tags$div(createCovariateForm(ns, input), id="my_cov_form")
    )
  })
}

getCovariateNames <- function(model) {
  covariates <- model$covariates
  if (inherits(model, "tdmore_mpc")) {
    includedCovariates <- names(model$mpc_theta)
    return(covariates[!(covariates %in% includedCovariates)])
  } else {
    return(covariates)
  }
}
