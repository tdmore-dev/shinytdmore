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

# shut down table
p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()
