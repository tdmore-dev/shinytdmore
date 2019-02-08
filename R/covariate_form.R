library(shiny)
library(tdmore)


# Reference model
model <- tacrolimus_storset
covariates <- model$covariates

# Patient
firstname <- "Nicolas"
lastname <- "Luyckx"


# Fields of covariates form
fields <- c(covariates)

saveData <- function(data) {
  data <- as.data.frame(t(data))
  if (exists("responses")) {
    responses <<- data
    patient <- getPatient(firstname = firstname, lastname = lastname)
    patient$covariates <- as.list(data)
    updatePatient(id=patient$id, patient=patient)
  }
}

loadData <- function() {
  patient <- getPatient(firstname = firstname, lastname = lastname)
  if (!is.null(patient)) {
    responses <<- data.frame(patient$covariates)
  }
}

createCovariateForm <- function() {
  retValue <- NULL
  for (covariate in covariates) {
    retValue <- list(retValue, covariate=textInput(inputId = covariate, label = covariate, value = ""))
  }
  retValue
}

shinyApp(
  ui = fluidPage(
    DT::dataTableOutput(outputId = "responses", width = 300), tags$hr(),
    createCovariateForm(),
    actionButton("submit", "Submit")
  ),
  server = function(input, output, session) {
    
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
      data <- sapply(fields, function(x) input[[x]])
      data
    })
    
    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
      saveData(formData())
    })
    
    # Show the previous responses
    # (update with current response when Submit is clicked)
    output$responses <- DT::renderDataTable({
      input$submit
      loadData()
    })     
  }
)
