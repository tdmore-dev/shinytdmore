library(shiny)
library(tdmore)
library(assertthat)
library(dplyr)

model <- tacrolimus_storset
covariates <- model$covariates
fields <- c("firstname", "lastname")

#'
#' Save data of both user and covariate forms.
#'
#' @param userData data from user from, named vector
#' @param covariateData data from covariate form, named vector
#' 
saveData <- function(userData, covariateData) {
  patient <- getPatient(firstname = userData[["firstname"]], lastname = userData[["lastname"]])
  if(!is.null(covariateData)) {
    covs <- setNames(as.numeric(covariateData), names(covariateData)) # String to numeric conversion
    covs <- as.list(covs)
  } else {
    covs <- NULL
  }
  if (is.null(patient)) {
    patient <- createPatient(firstname = userData[["firstname"]], lastname = userData[["lastname"]], covariates = covs)
    addPatient(patient)
  } else {
    patient$covariates <- covs
    updatePatient(patient$id, patient)
  }
}

#' 
#' Load necessary data for the output table.
#' 
loadData <- function() {
  patients <- getAllPatients()
  nb <- nrow(patients)
  if(nb > 0) {
    print(nrow(patients))
    myDf<- patients %>% select(id, firstname, lastname, created_at, modified_at) %>% rename(ID=id, Firstname=firstname, Lastname=lastname, 'Created'=created_at, 'Modified'=modified_at)
    myDf$Action <- shinyInput(actionButton, nb, 'button_', label = "Edit", onclick = 'Shiny.onInputChange(\"edit_button\",  this.id)' )
    myDf$Remove <- shinyInput(actionButton, nb, 'button_', label = "Remove", onclick = 'Shiny.onInputChange(\"remove_button\",  this.id)' )
    DTtable <<- reactiveValues(patientDf = myDf)
  }
}

shinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
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
createCovariateForm <- function() {
  retValue <- NULL
  for (covariate in covariates) {
    retValue <- list(retValue, covariate=textInput(inputId = covariate, label = covariate, value = ""))
  }
  retValue
}

#' 
#' Erase the covariate form.
#' 
#' @param session the shiny session object
#'
eraseCovariateForm <- function(session) {
  for (covariate in covariates) {
    tryCatch({
      updateTextInput(session, inputId=covariate, label = covariate, value = "")
    }, error = function(e) {
      print(paste("Covariate", covariate, "not found"))
    })
  }
}

#' 
#' Render the output table.
#' 
#' @param input the shiny input object
#'
renderTable <- function(input) {
  DT::renderDataTable(expr = {
    input$submit
    input$remove_button
    loadData()
    return(DTtable$patientDf)
  }, escape = FALSE, selection = 'none')  
}

#' 
#' Define the user interface components
#'
ui <- fluidPage(
  DT::dataTableOutput(outputId = "DTtable"),
  hr(),
  fluidRow(
    column(3,
      h4("User information"),
      createUserForm(),
      actionButton("submit", "Save / Create")
    ),
    column(3, offset = 1,
      h4("Covariates"),
      createCovariateForm(),
      actionButton("erase", "Erase")
    )
  )
)

#' 
#' Server inputs/outputs.
#' 
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#'
server <- function(input, output, session) {
  
  # User form data
  userFormData <- reactive({
    return(unlist(sapply(fields, function(x) input[[x]])))
  })
  
  # Covariate form data
  covariateFormData <- reactive({
    data <- sapply(covariates, function(x) {
      str <- input[[x]]
      if(str == "") NULL else str
    })
    return(unlist(data))
  })
  
  # Submit button
  observeEvent(input$submit, {
    saveData(userFormData(), covariateFormData())
  })
  
  # Remove button
  observeEvent(input$remove_button, {
    row <- as.numeric(strsplit(input$remove_button, "_")[[1]][2])
    patientRow <- DTtable$patientDf[row,]
    removePatient(as.numeric(patientRow$ID))
    output$DTtable <- renderTable(input)
  })
  
  # Edit button
  observeEvent(input$edit_button, {
    eraseCovariateForm(session)
    row <- as.numeric(strsplit(input$edit_button, "_")[[1]][2])
    patientRow <- DTtable$patientDf[row,]
    patient <- getPatient(as.numeric(patientRow$ID))
    for (covariate in covariates) {
      tryCatch({
        updateTextInput(session, inputId=covariate, value = patient$covariates[[covariate]])
      }, error = function(e) {
        print(paste("Covariate", covariate, "not found"))
      })
    }
    for (field in fields) {
      tryCatch({
        updateTextInput(session, inputId=field, value = patient[[field]])
      }, error = function(e) {
        print(paste("Field", field, "not found"))
      })
    }
  })
  
  # Erase button
  observeEvent(input$erase, {
    eraseCovariateForm(session)
  })
  
  # Render data table
  output$DTtable <- renderTable(input)
}

shinyApp(ui, server)
