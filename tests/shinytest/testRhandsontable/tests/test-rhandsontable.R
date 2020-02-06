library(shinytest)
library(magrittr)

source("../../../testthat/helperShinytest.R")

app <- ShinyDriver$new("..", loadTimeout = 30000, seed=1234)
app$snapshotInit("test-rhandsontable")
app$snapshot(filename = "start.json", items=list(input=TRUE, output=FALSE, export=TRUE)) #do not take the plot into account

# Test modifying cells in rapid succession --------------------------------
## - Without any binding on the server side, this works perfectly
## - If we do not propagate the change from input$table -a-> state$df -b-> output$table, it works fine too
## - However, if we propagate the change input$table -a-> state$df -b-> output$table, it creates a race
##   condition where some modifications to the hotTable are lost in the meantime...
## This can be solved by adding a check on propagation (b). If state$df and input$table are the same 
## (i.e. the change to state$df was caused by the handson-table), then the handsontable should not be re-rendered.
## 
## I recommend you add all of these extra checks to a module "synchronizedHandsontable".

Nstart <- app$getAllValues()$export$Nstart
PlotSleep <- app$getAllValues()$export$PlotSleep
tableCore <- app$findElement("table.htCore")
cell <- tableCore$findElement("td")
cell$click()
for(i in 1:Nstart)
  cell$sendKeys("99\n")

app$takeScreenshot()
# df first column should all be '99'
df <- app$getAllValues()$export$df #this waits until all rendering has stopped
testthat::expect_equal( df[, 1], rep(99, Nstart) )
app$takeScreenshot()

# Test Add Row in rapid succession ----------------------------------------
el <- app$findWidget("add")$getElement()
#click 15 times
for(i in 1:15) el$click()

p <- app$.__enclos_env__$private$shinyProcess

#p$get_error_file() %>% readLines() %>% paste(collapse="\n") %>% cat()
#p$get_output_file() %>% readLines() %>% paste(collapse="\n") %>% cat()

## app$waitFor("!window.shinytest.busy", timeout=3000) #this is not the right way to wait. Shiny becomes busy/idle/busy/idle/busy/idle
## see app$getDebugLog() to see this effect
## Better to wait like this:
app$waitFor(paste0( "$('#table .ht_master .htCore tbody tr').length == ", (Nstart+15)), timeout=15*PlotSleep*1000)
app$executeScript("return( $('#table .ht_master .htCore tbody tr').length );" )
app$takeScreenshot()

# should have received 15 clicks
testthat::expect_equal(
  p$get_output_file() %>% readLines() %>% grep("EVENT input\\$add", .) %>% length(),
  15
)

# rendering should have stopped
Nlogfile1 <- p$get_output_file() %>% readLines() %>% length()
Sys.sleep(1)
Nlogfile2 <- p$get_output_file() %>% readLines() %>% length() #check logs 1 second later
testthat::expect_equal(
  Nlogfile1,
  Nlogfile2,
  tol=1 #can differ by 1 (render of plot)
)

# and df should have 15 rows more
df <- app$getAllValues()$export$df
testthat::expect_equal( nrow(df), Nstart + 15 )


# teardown ----------------------------------------------------------------
p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()