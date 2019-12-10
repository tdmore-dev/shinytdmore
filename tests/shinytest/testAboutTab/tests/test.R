app <- ShinyDriver$new("..", loadTimeout = 30000, seed=1234)
app$snapshotInit("test")

## Filenames should end in either .json, .download or .png

app$snapshot(filename="snapshot.json")
#app$getDebugLog()
el <- app$findElement(xpath="//div[@data-value='aboutTabId']")
el$findElement(xpath="p")$getText()

destDir <- paste0(app$getSnapshotDir(), "-current")
writeLines( app$getSource(), file.path(destDir, "source.html.download") ) #make sure downloaded HTML matches

p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()

Sys.sleep(120)