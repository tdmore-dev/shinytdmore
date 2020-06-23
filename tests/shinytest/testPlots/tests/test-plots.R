#rstudioapi::getActiveDocumentContext()$path %>% dirname() %>% setwd()
library(shinytest)
app <- ShinyDriver$new("..", loadTimeout = 120*1000, seed=1234)
app$snapshotInit("test-plots")
source("../../../testthat/helperShinytest.R")
app$waitFor("$('.recalculating').length == 0")

## It takes a while for the plot to render
## during this rendering, plotly sets the attribute
## .clientValue-default-plotlyCrosstalkOpts
## See https://github.com/ropensci/plotly/blob/bcae42bbeea8d8368fb3ca881243e6acd0c661b4/inst/htmlwidgets/plotly.js#L35
## And https://github.com/rstudio/crosstalk/blob/68b0b617ee82e5d6b738f26106933254ce5ede53/R/crosstalk.R#L97
## and https://github.com/rstudio/crosstalk/blob/master/javascript/src/var.js#L37
app$waitFor("$('.plotly .plot-container .svg-container').length > 0")

waitUntilPresent(".clientValue-default-plotlyCrosstalkOpts")


## snapshot population plot
app$snapshot(filename="population.json")
#snapshotSource("population") #SVG files contain random ID attributes

## snapshot fit plot
app$setInputs(`plots-active`="fit")
waitUntilPresent("plotly_afterplot-plots-fit") #wait for plotly event to be registered in input list
app$snapshot(filename="fit.json")
#snapshotSource("fit")

## snapshot recommendation plot
app$setInputs(`plots-active`="recommendation")
waitUntilPresent("plotly_afterplot-plots-recommendation") #wait for plotly event to be registered in input list
app$snapshot(filename="recommendation.json")
#snapshotSource("recommendation")

# shut down table
p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()

