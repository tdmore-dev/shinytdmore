library(shinytest)
app <- ShinyDriver$new("..", loadTimeout = 30000, seed=1234)
app$snapshotInit("test")
destDir <- paste0(app$getSnapshotDir(), "-current")

source("../../../testthat/helperShinytest.R")

## Filenames should end in either .json, .download or .png

app$snapshot(filename="snapshot.json")
#app$getDebugLog()
el <- app$findElement(xpath="//div[@data-value='aboutTabId']")
el$findElement(xpath="p")$getText()

## The snapshot will contain the build dates of each package, and will *constantly* change
##snapshotSource("source") #make sure downloaded HTML matches

p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()