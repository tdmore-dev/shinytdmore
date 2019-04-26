##
## Script name: app.R
##
## Purpose of script:
## Main user interface for Tacrolimus dose recommendation
##
## Author: Quentin Leirens, Ruben Faelens
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
library(DT)
library(tidyverse)
library(plotly)
library(rhandsontable)
library(tdmore)
library(shinytdmore)
library(assertthat)

# Default config added to environment
toConfig(key="db_config", value=defaultDBConfig())

ui <- shinyTdmoreUI()
server <- function(input, output, session) {
  shinyTdmoreServer(input, output, session)
}
shinyApp(ui = ui, server = server)
