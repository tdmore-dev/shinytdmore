library(shinytest)

app <- ShinyDriver$new("..", loadTimeout = 30000, seed=1234)
app$snapshotInit("test")
destDir <- paste0(app$getSnapshotDir(), "-current")

## Filenames should end in either .json, .download or .png

app$snapshot(filename="snapshot.json")
#app$getDebugLog()
el <- app$findElement(xpath="//a[@id='myText']")
el$click()
app$takeScreenshot()
el <- app$findElement(xpath="//input")
el$setValue("Example Text")
app$findElement(xpath="//button[@type='submit']")$click()

el <- app$findElement(xpath="//pre[@id='bar']")
writeLines( el$getText(), file.path(destDir, "verbatimOut.download") ) #make sure downloaded HTML matches
app$snapshot(filename="after-myText-input.json")


## make sure download HTML matches
Sys.sleep(1) #make sure all animations have completed...
writeLines( app$getSource(), file.path(destDir, "source.html.download") ) #make sure downloaded HTML matches

p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()
