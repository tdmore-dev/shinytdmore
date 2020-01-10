## The below are utility functions useful when running the actual tests
readyShouldUpdate <- function(id) {
  app <- get("app", envir=parent.frame())
  js <- paste0(
    "window.shinytest.updating=[", paste("'", id, "'", sep="", collapse=", "),"]"
  )
  app$executeScript(js)
}
waitUntilReady <- function(timeout=3000, testUpdate=FALSE) {
  app <- get("app", envir=parent.frame())
  Sys.sleep(0.2)
  stopifnot( app$waitFor("$('.ht_master').length > 0", timeout=timeout) ) #wait until rhandsontable rendered
  stopifnot( app$waitFor("$('.shiny-busy').length == 0", timeout=timeout) )
  stopifnot( app$waitFor("$('.recalculating').length == 0", timeout=timeout) )
  stopifnot( app$waitFor("!window.shinytest.busy", timeout=timeout) )
  if(testUpdate) {
    if(!app$waitFor("window.shinytest.updating.length == 0", timeout=timeout) ) {
      # app$takeScreenshot()
      # print(app$getEventLog())
      # print(app$getDebugLog())
      stop("Objects ", app$executeScript("return(window.shinytest.updating);"), " did not update...")
    }
  }
  if(timeout != 0) waitUntilReady(timeout=0, testUpdate=testUpdate)
}

with_update <- function(id, code, timeout=3000) {
  app <- get("app", envir=parent.frame())
  readyShouldUpdate(id)
  force(code)
  waitUntilReady(testUpdate=TRUE, timeout=timeout)
}

snapshotSource <- function(id) {
  app <- get("app", envir=parent.frame())
  snapshot(app$getSource(), paste0(id,".html"))
}

snapshot <- function(text, id) {
  app <- get("app", envir=parent.frame())
  destDir <- paste0(app$getSnapshotDir(), "-current")
  file <- file.path(destDir, paste0(id,".download"))
  writeLines(text, file) #make sure downloaded HTML matches
}