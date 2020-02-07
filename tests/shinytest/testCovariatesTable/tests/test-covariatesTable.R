# Test covariates table
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(shinytest)
app <- ShinyDriver$new("..", loadTimeout = 30000, seed = 1234)
app$snapshotInit("test-covariatesTable")

source("../../../testthat/helperShinytest.R")

tableId <- "myCov-table-table"
app$waitFor("$('#myCovs-table-table .htCore').length > 0") #wait until covariatesTable is rendered
app$snapshot(filename = "start.json")

## table starts out blank
covs <- app$getAllValues()$export$covs
expect_null(covs)

## Add a new observation
app$setInputs(`myCovs-add` = "click") #also ensures there is an update

covs <- app$getAllValues()$export$covs
expect_equal(colnames(covs), c("time", "WT"))
expect_equal(nrow(covs), 1)
expect_equal(covs$time, as.POSIXct(NA) )
expect_equal(covs$WT, as.numeric(NA))

## Check header

headers <- app$findElements("#myCovs-table-table > div.ht_master.handsontable > div > div > div > table > thead > tr > th > div > span.colHeader")
wtHeader <- headers[[3]]
expect_equal(
  wtHeader$getAttribute(name="innerHTML"),
  "Weight (kg)"
)

app$setInputs(`metadata`=FALSE)
expect_equal(
  wtHeader$getAttribute(name="innerHTML"),
  "WT"
)

## cell1 test default value
tableCore <- app$findElement("table.htCore")
cell <- function(i) {tableCore$findElements("td")[[i]]}

table <- app$getAllValues()$input$`myCovs-table-table`$data
expect_equal(table[[1]][[1]], NULL)

cellValue <- sub("<div.*</div>", "", cell(1)$getAttribute(name="innerHTML"))
testthat::expect_equal(cellValue, "")

## cell1 set correct value
cell(1)$click()
cell(1)$sendKeys("1987-12-11\t")
cell(2)$sendKeys("12:15\t")

testthat::try_again(10, {
  Sys.sleep(0.2)
  covs <- app$getAllValues()$export$covs
  expect_equal(covs$time, as.POSIXct("1987-12-11 12:15"), tol=1E-4)
})

# set weight
cell(3)$click()
cell(3)$setValue("15")
cell(1)$click()
app$takeScreenshot()

testthat::try_again(10, {
  Sys.sleep(0.2)
  covs <- app$getAllValues()$export$covs
  expect_equal(covs$WT, 15)
})

# enable metadata, existing weight stays
app$setInputs(`metadata`=TRUE)
covs <- app$getAllValues()$export$covs
expect_equal(covs$WT, 15)

# try to change it to 1000 kg
cell(3)$click()
cell(3)$sendKeys("2000\t")
cell(1)$click()

Sys.sleep(1) #allow enough time for values to update
covs <- app$getAllValues()$export$covs
expect_equal(covs$WT, 15) #WT remains the same, you cannot enter an invalid weight (out of range)

# cancel the attempted edit
cell(3)$sendKeys(webdriver::key$escape) #ESCAPE, see https://selenium.dev/selenium/docs/api/py/webdriver/selenium.webdriver.common.keys.html


## Now let's try a different model
app$setInputs(`alternate`=TRUE)
  # the covariates table should be maintained unchanged

headers <- app$findElements("#myCovs-table-table > div.ht_master.handsontable > div > div > div > table > thead > tr > th > div > span.colHeader")
expect_equal(length(headers), 3) #only 3 columns
expect_equal(app$getAllValues()$export$covs,
             data.frame(time=as.POSIXct("1987-12-11 12:15"),
                    WT=15
             )
)

### Add row
app$setInputs(`myCovs-add` = "click")
# there should now be an extra SEX column
headers <- app$findElements("#myCovs-table-table > div.ht_master.handsontable > div > div > div > table > thead > tr > th > div > span.colHeader")
expect_equal(length(headers), 4) #only 3 columns
expect_equal(
  headers[[4]]$getAttribute("innerHTML"),
  "Sex"
)
expect_equal(app$getAllValues()$export$covs,
             data.frame(time=c(as.POSIXct("1987-12-11 12:15"), as.POSIXct("1987-12-12 12:15")),
                        WT=rep(15, 2),
                        SEX=rep(as.numeric(NA), 2)
             )
)

# set first cell as Male
cell(4)$click()
cell(4)$sendKeys("Ma", webdriver::key$enter)
app$takeScreenshot()

expect_equal(app$getAllValues()$export$covs,
             data.frame(time=c(as.POSIXct("1987-12-11 12:15"), as.POSIXct("1987-12-12 12:15")),
                        WT=rep(15, 2),
                        SEX=c(0, as.numeric(NA)) #the first value is '0'
             )
)


# shut down table
p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()
