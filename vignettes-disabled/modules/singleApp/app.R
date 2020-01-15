library(shinytdmore)

db <- InMemoryDatabase$new()
ui <- shinyTdmoreUI(title="shinyTDMore",
                    patientsTabUI("patients"),
                    predictionTabUI("prediction"),
                    reportsTabUI("reports"),
                    aboutTabUI("about"))



conf <- list(save=list(module=saveProject, id="saveProjectId"),
             new_patient=list(module=newPatientDialog, id="newPatientDialogId"),
             patients=list(module=patientsTab, id="patientsTabId"),
             prediction=list(module=predictionTab, id="predictionTabId"),
             reports=list(module=reportsTab, id="reportsTabId"),
             about=list(module=aboutTab, id="aboutTabId"))

server <- function(input, output, session) {
  shinyTdmore(input, output, session, conf, db)
}

shinyApp(ui = ui, server = server)