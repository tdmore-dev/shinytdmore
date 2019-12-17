library(shinytest)
app <- ShinyDriver$new("..", loadTimeout = 30000, seed=1234)
app$snapshotInit("test")
app$waitFor("$('.ht_master').length > 0") #wait until rhandsontable rendered

snapshotSource <- function(id) {
  source <- app$getSource()
  snapshot(app$getSource(), paste0(id,".html"))
}
snapshot <- function(text, id) {
  destDir <- paste0(app$getSnapshotDir(), "-current")
  file <- file.path(destDir, paste0(id,".download"))
  writeLines(text, file) #make sure downloaded HTML matches
}

## Filenames should end in either .json, .download or .png
app$snapshot(filename="start.json")
app$waitFor("$('.recalculating').length == 0")
snapshotSource("start")

## table starts out blank
observed <- app$getAllValues()$export$observed
expect_equal(colnames(observed), c("date", "time", "dv", "use"))
expect_equal(nrow(observed), 0)

## Add a new observation
time <- Sys.time()
app$setInputs(`myObs-add`="click") #also ensures there is an update

observed <- app$getAllValues()$export$observed
expect_equal(colnames(observed), c("date", "time", "dv", "use"))
expect_equal(nrow(observed), 1)
expect_equal(as.Date(observed$date), as.Date(time), tol=1E-3)
expect_equal(observed$time, format(time, format="%H:%M") )
expect_equal(observed$dv, 0)
expect_equal(observed$use, TRUE)

## Fill in table
tableCore <- app$findElement("table.htCore")
cell <- function(i) {tableCore$findElements("td")[[i]]}

## cell1 test default value
cell(1)$click()

table <- app$getAllValues()$input$`myObs-table`$data
expect_equal(table[[1]][[1]], strftime(time, "%Y-%m-%d"))

cellValue <- sub("<div.*</div>", "", cell(1)$getAttribute(name="innerHTML"))
testthat::expect_equal(cellValue, strftime(time, "%Y-%m-%d"))

## cell1 set invalid value
cell(1)$click(); Sys.sleep(1)
cell(1)$setValue("invalid_value")
cell(1)$sendKeys("invalid_value")
cell(1)$sendKeys("\t"); Sys.sleep(1)
cellValue <- sub("<div.*</div>", "", cell(1)$getAttribute(name="innerHTML"))
testthat::expect_equal(cellValue, strftime(time, "%Y-%m-%d")) #no change

## cell1 set correct value
cell(1)$click()
cell(1)$sendKeys("1987-12-11\t"); Sys.sleep(1)
cell(2)$click()
Sys.sleep(1) #wait a bit for the exported value to update...

observed <- app$getAllValues()$export$observed
expect_equal(as.Date(observed$date), as.Date("1987-12-11"), tol=1E-3)

# set time
cell(2)$click(); Sys.sleep(1)
cell(2)$sendKeys("13:23")
cell(2)$sendKeys("\t"); Sys.sleep(1)
app$takeScreenshot()

observed <- app$getAllValues()$export$observed
expect_equal(observed$time, "13:23")

# set dose
cell(3)$click()
cell(3)$setValue("15")
cell(4)$click(); Sys.sleep(0.5)
observed <- app$getAllValues()$export$observed
expect_equal(observed$dv, 15)

# add 2 new doses
app$setInputs(`myObs-add`="click") #also ensures there is an update
app$setInputs(`myObs-add`="click") #also ensures there is an update

# new dose should be copy of old dose, but with +24h
observed <- app$getAllValues()$export$observed
expect_equal(nrow(observed), 3)
expect_equal( observed$date[1] + lubridate::days(1), observed$date[2])
expect_equal(observed$dv, rep(15, 3))

# table should sort values
cell(3)$click()
cell(3)$setValue("999")
cell(1)$click()
cell(1)$setValue("1987-12-14") #should be later
cell(4)$click(); Sys.sleep(0.5)
cell(4)$click(); Sys.sleep(0.5)

observed <- app$getAllValues()$export$observed
expect_true(!is.unsorted(observed$date))
expect_equal(observed$dv, c(15, 15, 999))

# shut down table
p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()
