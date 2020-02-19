waitUntilPresent <- function(id, where=c("input", "output", "export"), times=20, sleep=2) {
  app <- get("app", envir=parent.frame())
  where <- match.arg(where)
  testthat::try_again(times, {
    values <- app$getAllValues(input=(where=="input"), output=(where=="output"), export=(where=="export"))
    inputList <- values[[where]]
    value <- inputList[[id]]
    if(is.null(value)) {
      Sys.sleep(sleep)
      fail(paste0("Value ", id, " not found in ", where, "; available names: ", paste(names(inputList), collapse=",")))
    }
  })
}

waitUntilReady <- function(timeout=30000, testUpdate=FALSE) {
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

snapshotSource <- function(id) {
  app <- get("app", envir=parent.frame())
  snapshot(app$getSource(), paste0(id,".html"))
}

snapshot <- function(text, id) {
  app <- get("app", envir=parent.frame())
  destDir <- paste0(app$getSnapshotDir(), "-current")
  file <- file.path(destDir, paste0(id,".download"))
  
  fileCon <- base::file(file, open="wb")
  ## writeLines writes either \n or \r\n depending on the platform
  ## For more control, open a binary connection and specify the precise value
  writeLines(text, fileCon, sep="\n") #make sure downloaded HTML matches
  close(fileCon)
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