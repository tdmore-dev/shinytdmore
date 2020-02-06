library(shinytest)
app <- ShinyDriver$new("..", loadTimeout = 30000, seed=1234)
app$snapshotInit("test")
destDir <- paste0(app$getSnapshotDir(), "-current")

## Filenames should end in either .json, .download or .png

app$snapshot(filename="snapshot.json")
#app$getDebugLog()
el <- app$findElement(xpath="//div[@data-value='aboutTabId']")
el$findElement(xpath="p")$getText()

snapshotSource("source") #make sure downloaded HTML matches

p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()