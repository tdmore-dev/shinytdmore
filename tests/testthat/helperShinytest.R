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
  list[ sort(names(list)) ]
}
normalize <- function(filename, app=get("app", envir=parent.frame()) ) {
  if(is.null(app)) {
    file <- filename
  } else {
    current_dir <- paste0(app$getSnapshotDir(), "-current")
    file <- file.path(current_dir, filename)
  }
  json <- jsonlite::read_json(file)
  json$input <- sortByName( json$input )
  json$output <- sortByName( json$output )
  json$export <- sortByName( json$export )
  jsonlite::write_json(json, path=file, pretty=2, auto_unbox=TRUE)
}

setupHtmlwidgetsDebug <- function(app) {
  f = "
  function tryEval(code) {
    var result = null;
    try {
      result = eval(\"(\" + code + \")\");
    } catch(error) {
      if (!error instanceof SyntaxError) {
        throw error;
      }
      try {
        result = eval(code);
      } catch(e) {
        if (e instanceof SyntaxError) {
          throw error;
        } else {
          throw e;
        }
      }
    }
    return result;
  }
  function splitWithEscape(value, splitChar, escapeChar) {
    var results = [];
    var escapeMode = false;
    var currentResult = \"\";
    for (var pos = 0; pos < value.length; pos++) {
      if (!escapeMode) {
        if (value[pos] === splitChar) {
          results.push(currentResult);
          currentResult = \"\";
        } else if (value[pos] === escapeChar) {
          escapeMode = true;
        } else {
          currentResult += value[pos];
        }
      } else {
        currentResult += value[pos];
        escapeMode = false;
      }
    }
    if (currentResult !== \"\") {
      results.push(currentResult);
    }
    return results;
  }
  
window.HTMLWidgets.evaluateStringMember = function(o, member) {
  var parts = splitWithEscape(member, '.', '\\\\');
  for (var i = 0, l = parts.length; i < l; i++) {
    var part = parts[i];
    // part may be a character or 'numeric' member name
    if (o !== null && typeof o === \"object\" && part in o) {
      if (i == (l - 1)) { // if we are at the end of the line then evalulate
        if (typeof o[part] === \"string\") {
          console.log('DEBUG: Evaluating string member '+o[part]);
          o[part] = tryEval(o[part]);
        }
      } else { // otherwise continue to next embedded object
        o = o[part];
      }
    }
  }
};
"
  app$executeScript(f)
}