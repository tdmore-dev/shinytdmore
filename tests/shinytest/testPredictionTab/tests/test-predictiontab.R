#rstudioapi::getActiveDocumentContext()$path %>% dirname() %>% setwd()
library(shinytest)
app <- ShinyDriver$new("..", loadTimeout = 120*1000, seed=1234)
app$snapshotInit("test-predictiontab")
source("../../../testthat/helperShinytest.R")

waitUntilPresent("prediction-doses-table-table")
waitUntilPresent("prediction-observation-table-table")
app$snapshot(filename="start.json", items=list(input=TRUE))

# add a covariate
el <- app$findElement(".panel[value='Covariates'] h4 a")
el$click(); Sys.sleep(1) #wait until animation completes

app$setInputs(`prediction-covariates-add`="click", values_=FALSE)

app$waitFor("$('#prediction-covariates-table-table .handsontable td').length == 3", timeout=10*1000)
cells <- app$findElements("#prediction-covariates-table-table .handsontable td")
cells[[1]]$click(); cells[[1]]$setValue("2000-01-02")
cells[[2]]$click(); cells[[2]]$setValue("08:00")
cells[[3]]$click(); cells[[3]]$setValue("70"); cells[[3]]$sendKeys("\n")
app$takeScreenshot()

# add a dose
app$setInputs(`prediction-doses-add`="click", values_=FALSE) #do not request values_; fails because of plotly output / shiny::req issue

app$waitFor("$('#prediction-doses-table-table .handsontable td').length == 5", timeout=10*1000)
cells <- app$findElements("#prediction-doses-table-table .handsontable td")
cells[[1]]$click(); cells[[1]]$setValue("2000-01-02")
cells[[2]]$click(); cells[[2]]$setValue("08:00")
cells[[3]]$click(); cells[[3]]$setValue("150"); cells[[3]]$sendKeys("\n")
app$takeScreenshot()


lapply( app$getAllValues(output=FALSE), lapply, names)


app$takeScreenshot()

# shut down table
p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()
