# Test observation table
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(shinytest)
app <- ShinyDriver$new("..", loadTimeout = 30000, seed = 1234)
app$snapshotInit("test-obsTable")

source("../../../testthat/helperShinytest.R")

tableId <- "myObs-table-table"

waitUntilReady()
app$snapshot(filename = "start.json")

## table starts out blank
observed <- app$getAllValues()$export$observed
expect_null(observed)
#expect_equal(colnames(observed), c("time", "dv", "use"))
#expect_equal(nrow(observed), 0)

## Add a new observation
app$setInputs(`myObs-add` = "click") #also ensures there is an update

observed <- app$getAllValues()$export$observed
expect_equal(colnames(observed), c("time", "dv", "use"))
expect_equal(nrow(observed), 1)
expect_equal(observed$time, as.POSIXct(NA) )
expect_equal(observed$dv, 0)
expect_equal(observed$use, TRUE)

## cell1 test default value
tableCore <- app$findElement("table.htCore")
cell <- function(i) {tableCore$findElements("td")[[i]]}

table <- app$getAllValues()$input$`myObs-table-table`$data
expect_equal(table[[1]][[1]], NULL)

cellValue <- sub("<div.*</div>", "", cell(1)$getAttribute(name="innerHTML"))
testthat::expect_equal(cellValue, "")

## cell1 set invalid value
## Not really a valid test; see testDoseTable for more info
# readyShouldUpdate(tableId)
# cell(1)$click(); Sys.sleep(1)
# cell(1)$setValue("invalid_value")
# cell(1)$sendKeys("invalid_value")
# cell(1)$sendKeys("\t")
# cellValue <- sub("<div.*</div>", "", cell(1)$getAttribute(name="innerHTML"))
# testthat::expect_equal(cellValue, "") #no change
# expect_error( waitUntilReady(testUpdate=TRUE),
#               regex="Object.*myObs-table.*did not update") #there should be no update!

## cell1 set correct value
### This often does not work!!!
cell(4)$click() #click on different cell to ensure we do not open the date-picker control!
cell(1)$click()
web <- app$.__enclos_env__$private$web ### TODO FIXME: the two Sleep commands are required, unfortunately...
web$getActiveElement()$sendKeys("1987-12-11\t"); Sys.sleep(2)
web$getActiveElement()$sendKeys("12:15\t"); Sys.sleep(2)

tryCatch(
  testthat::try_again(30, { #wait long enough!
    Sys.sleep(0.2)
    observed <- app$getAllValues()$export$observed
    expect_equal(observed$time, as.POSIXct("1987-12-11 12:15"), tol=1E-4)
  }),
  error=function(e) {
    app$takeScreenshot()
    browser()
    stop(e)
  })

# set time
cell(2)$click()
cell(2)$sendKeys("13:23\t")

testthat::try_again(20, {
  Sys.sleep(0.2)
  app$takeScreenshot()
  
  observed <- app$getAllValues()$export$observed
  expect_equal(observed$time, as.POSIXct("1987-12-11 13:23"), tol=1E-4)
})

# set observation
cell(3)$click()
cell(3)$setValue("15")
cell(4)$click()

testthat::try_again(10, {
  Sys.sleep(0.2)
  observed <- app$getAllValues()$export$observed
  expect_equal(observed$dv, 15)
})

# add 2 new doses
app$setInputs(`myObs-add`="click")
app$setInputs(`myObs-add`="click")

testthat::try_again(10, {
  Sys.sleep(0.2)
  observed <- app$getAllValues()$export$observed # new dose should be copy of old dose, but with +24h
  expect_equal(nrow(observed), 3)
  expect_equal( observed$time[1] + lubridate::days(1), observed$time[2])
  expect_equal(observed$dv, rep(15, 3))
})

# table should sort values
cell(3)$click()
cell(3)$setValue("999")
cell(1)$click()
cell(1)$setValue("1987-12-14\t") #should be later

testthat::try_again(10, {
  Sys.sleep(0.2)
  observed <- app$getAllValues()$export$observed
  expect_true(!is.unsorted(observed$date))
  expect_equal(observed$dv, c(15, 15, 999))
})

# Table does not freak out when doing manipulations in quick succession
button <- app$findWidget("myObs-add")
button$getElement()$click()
button$getElement()$click()
button$getElement()$click()
button$getElement()$click()
#ensure all clicks have passed through
Sys.sleep(5)

# should not enter in infinite loop
nrow <- lapply(1:10, function(i) {
  Sys.sleep(0.2)
  observed <- app$getAllValues()$export$observed
  nrow(observed)
})
nrow <- unlist(nrow)
expect_equal(nrow, rep(median(nrow), length(nrow)))


# shut down table
p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()
