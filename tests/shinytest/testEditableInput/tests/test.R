library(shinytest)

app <- ShinyDriver$new("..", loadTimeout = 30000, seed=1234)
app$snapshotInit("test")
destDir <- paste0(app$getSnapshotDir(), "-current")

source("../../../testthat/helperShinytest.R")

## Filenames should end in either .json, .download or .png

app$snapshot(filename="snapshot.json")
#app$getDebugLog()
el <- app$findElement(xpath="//a[@id='myText']")
el$click()
app$takeScreenshot()
el <- app$findElement(xpath="//input")
el$setValue("Example Text")
app$findElement(xpath="//button[@type='submit']")$click()

Sys.sleep(2) #make sure all animations have completed...

el <- app$findElement(xpath="//pre[@id='bar']")
snapshot( el$getText(), "verbatimOut" ) #make sure downloaded HTML matches
app$snapshot(filename="after-myText-input.json")
snapshotSource( "source") #make sure downloaded HTML matches

p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()
