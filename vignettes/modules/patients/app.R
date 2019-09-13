library(tdmore)
library(shinytdmore)
db <- InMemoryDatabase$new()
db$add(createFakePatient())
db$add(createFakePatient())
db$add(createFakePatient())

ui <- navbarPage(
  tabsetPanel(type = "pills",
    patientsTabUI("patients")
  )
)


server <- function(input, output, session) {
  patientsTab <- callModule(patientsTab, "patients", db=db)
  observeEvent(patientsTab$viewPatient(), {
    ## When a patient is selected in the table
    patientId <- patientsTab$viewPatient()
    updatePredictionTab(session, "myPredictionTab", patientId)
    cat( "Patient ID ", patientId, " selected\n" ) 
  })
}

shinyApp(ui = ui, server = server)