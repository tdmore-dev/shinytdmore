#rstudioapi::getActiveDocumentContext()$path %>% dirname() %>% setwd()
library(shinytest)
app <- ShinyDriver$new("..", loadTimeout = 30000, seed=1234)
app$snapshotInit("test-plots")
source("../../../testthat/helperShinytest.R")

app$waitFor("$('.recalculating').length == 0")

## snapshot population plot
app$snapshot(filename="population.json")
#snapshotSource("population") #SVG files contain random ID attributes

## snapshot fit plot
app$setInputs(`plots-active`="fit")
app$snapshot(filename="fit.json")
#snapshotSource("fit")

## snapshot recommendation plot
app$setInputs(`plots-active`="recommendation")
app$snapshot(filename="recommendation.json")
#snapshotSource("recommendation")

# shut down table
p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()
