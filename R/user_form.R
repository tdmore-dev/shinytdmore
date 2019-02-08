library(shiny)
library(tdmore)

# Fields of user form
fields <- c("firstname", "lastname")

saveData <- function(data) {
  patient <- getPatient(firstname = data[["firstname"]], lastname = data[["lastname"]])
  if (is.null(patient)) {
    patient <- createPatient(firstname = data[["firstname"]], lastname = data[["lastname"]], covariates = c())
    addPatient(patient)
  }
  loadData()
}

loadData <- function() {
  patients <- getAllPatients()
  if(nrow(patients) > 0) {
    responses <<- patients %>% select(id, firstname, lastname)
  }
}

createUserForm <- function() {
  list(textInput(inputId = "firstname", label = "Firstname", value = ""),
       textInput(inputId = "lastname", label = "Lastname", value = ""))
}

shinyApp(
  ui = fluidPage(
    DT::dataTableOutput(outputId = "responses", width = 300), tags$hr(),
    createUserForm(),
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
