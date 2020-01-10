# Test dose table
library(shinytest)
app <- ShinyDriver$new("..", loadTimeout = 30000, seed = 1234)
app$snapshotInit("test-doseTable")

source("../../../testthat/helperShinytest.R")

tableId <- "myDose-table-table"

app$snapshot(filename = "start.json")

regimen <- app$getAllValues()$export$regimen ## table starts out blank
expect_null(regimen)
#expect_equal(colnames(regimen), c("time", "dose", "formulation", "fix"))
#expect_equal(nrow(regimen), 0)

## Add a new dose
app$setInputs(`myDose-add` = "click") #also ensures there is an update

regimen <- app$getAllValues()$export$regimen
expect_equal(colnames(regimen), c("time", "dose", "formulation", "fix"))
expect_equal(nrow(regimen), 1)
expect_equal(regimen$time, as.POSIXct(NA))
expect_equal(regimen$dose, 0)
expect_equal(regimen$formulation, "")
expect_equal(regimen$fix, FALSE)

## Fill in table
tableCore <- app$findElement("table.htCore")
cell <- function(i) {tableCore$findElements("td")[[i]]}

## cell1 test default value
cell(1)$click()
table <- app$getAllValues()$input$`myDose-table`$data
expect_equal(table[[1]][[1]], NULL)

cellValue <- sub("<div.*</div>", "", cell(1)$getAttribute(name = "innerHTML"))
testthat::expect_equal(cellValue, "")

## cell1 set invalid value
readyShouldUpdate(tableId) #but it should actually not! see later
cell(1)$click(); Sys.sleep(1)
cell(1)$setValue("invalid_value")
cell(1)$sendKeys("invalid_value")
cell(1)$sendKeys("\t"); Sys.sleep(1)
cellValue <- sub("<div.*</div>", "", cell(1)$getAttribute(name = "innerHTML"))
testthat::expect_equal(cellValue, "") #no change
testthat::expect_error( waitUntilReady(testUpdate = TRUE),
                        regex = "Object.*myDose-table.*did not update")
readyShouldUpdate(NULL) #reset


## cell1 set correct value
cell(1)$click()
cell(1)$sendKeys("1987-12-11\t")
cell(2)$click()
cell(2)$sendKeys("12:00\t")
cell(4)$click()

testthat::try_again(100, {
  Sys.sleep(0.2)
  regimen <- app$getAllValues()$export$regimen
  expect_equal(as.Date(regimen$time), as.Date("1987-12-11"), tol = 1E-3)
  expect_equal(regimen$time, as.POSIXct("1987-12-11 12:00"), tol = 1E-3)
})

# set dose
cell(3)$click()
cell(3)$setValue("15\t")

testthat::try_again(100, {
  Sys.sleep(0.2)
  regimen <- app$getAllValues()$export$regimen
  expect_equal(regimen$dose, 15)
})

# add 2 new doses
app$setInputs(`myDose-add`="click")
app$setInputs(`myDose-add`="click")

app$takeScreenshot()

testthat::try_again(10, {
  Sys.sleep(0.2)
  regimen <- app$getAllValues()$export$regimen
})

# new dose should be copy of old dose, but with +24h
regimen <- app$getAllValues()$export$regimen
expect_equal(nrow(regimen), 3)
expect_equal( regimen$time[1] + lubridate::days(1), regimen$time[2])
expect_equal(regimen$dose, rep(15, 3))

# table should sort values
cell(3)$click()
cell(3)$setValue("999")
cell(1)$click()
cell(1)$setValue("1987-12-14") #should be later
cell(4)$click()
cell(4)$click()

testthat::try_again(10, {
  Sys.sleep(0.2)
  regimen <- app$getAllValues()$export$regimen
  expect_true(!is.unsorted(regimen$date))
  expect_equal(regimen$dose, c(15, 15, 999))
})

# Table does not freak out when doing manipulations in quick succession
button <- app$findWidget("myDose-add")
button$getElement()$click()
button$getElement()$click()
button$getElement()$click()
button$getElement()$click()
button$getElement()$click()
waitUntilReady()

# shut down table
p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()
