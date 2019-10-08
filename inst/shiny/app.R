##
## Script name: app.R
##
## Purpose of script:
## Main user interface for Tacrolimus dose recommendation
##
## Author: Quentin Leirens, Ruben Faelens, Nicolas Luyckx
##
## Date Created: Thu Mar 21 11:51:10 2019
##
## Copyright (c) Ruben Faelens, 2019; Quentin Leirens, 2019
## Email: ruben.faelens@gmail.com
##
## ---------------------------
##
## Notes:
## notes
##
## ---------------------------

library(shiny)
library(shinyBS)
library(DT)
library(tidyverse)
library(plotly)
library(rhandsontable)
library(tdmore)
library(shinytdmore)
library(assertthat)

# Default config added to environment
db <- MongoDatabase$new(collection="test", db="test")
if(length(db$patients) == 0) db$add(createFakePatient())

ui <- shinyTdmoreUI(title="shinyTDMore",
                    patientsTabUI(id="patientsTabId"),
                    predictionTabUI(id="predictionTabId"),
                    modelTabUI(id="modelTabId"),
                    reportsTabUI(id="reportsTabId"),
                    aboutTabUI(id="aboutTabId"))

conf <- list(save=list(module=saveProject, id="saveProjectId"),
             new_patient=list(module=newPatientDialog, id="newPatientDialogId"),
             patients=list(module=patientsTab, id="patientsTabId"),
             prediction=list(module=predictionTab, id="predictionTabId"),
             model=list(module=modelTab, id="modelTabId"),
             reports=list(module=reportsTab, id="reportsTabId"),
             about=list(module=aboutTab, id="aboutTabId"))

server <- function(input, output, session) {
  shinyTdmore(input, output, session, conf, db=db)
}
shinyApp(ui = ui, server = server)
