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
  #app needs to be present for "readyShouldUpdate" and "waitUntilReady" to find it
  app <- get("app", envir=parent.frame())
  app #to get rid of warning about unused "app"
  
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

## ensure input/output/export all have a consistent order
### YOU HAVE GOT TO BE KIDDING ME!
### Sorting order is different between en_US and C locales!
### See ?Comparison
### Fix is to force the locale to C for collation
Sys.setlocale(category="LC_COLLATE", "C")

sortByName <- function(list) {
  cat("Current names: ", paste(names(list), collapse=", "), "\n")
  cat("Sorted names: ", paste(sort(names(list)), collapse=", "), "\n")
  list[ sort(names(list)) ]
}
normalize <- function(filename, app=get("app", envir=parent.frame()) ) {
  cat("Normalizing JSON file ",filename,"\n")
  if(is.null(app)) {
    file <- filename
  } else {
    current_dir <- paste0(app$getSnapshotDir(), "-current")
    file <- file.path(current_dir, filename)
  }
  json <- jsonlite::read_json(file)
  cat("Read file: \n------\n",capture.output(print(json)),"\n------\n")
  json$input <- sortByName( json$input )
  json$output <- sortByName( json$output )
  json$export <- sortByName( json$export )
  content <- jsonlite::toJSON(json, pretty=2, auto_unbox=TRUE)
  cat("Saving file: \n------\n",content,"\n------\n")
  jsonlite::write_json(json, path=file, pretty=2, auto_unbox=TRUE)
  cat("Saved file ", file, "\n")
}