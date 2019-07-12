library(shinytdmore)

dir <- tempfile()
dir.create(dir)
db <- FileDatabase$new(dir)

ui <- shinyTdmoreUI(title="shinyTDMore",
                    patientsTabUI(id="patientsTabId"),
                    predictionTabUI(id="predictionTabId"),
                    reportsTabUI(id="reportsTabId"),
                    aboutTabUI(id="aboutTabId"))

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