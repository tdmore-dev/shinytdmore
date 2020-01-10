## This file contains a module to synchronize a Handsontable to a reactive value
## This is not as easy as it seems, because there is the potential for race conditions:
## When the user edits cell A and then continues to edit cell B, there is a race condition:
## 1) the user has edited cell B
## 2) browser send update regarding cell A to input$table
##    input$table updates state$df
##    state$df updates output$table
##    output$table updates the table in the browser
## The end result is lost modifications.
## 
## The same thing happens if two events update the state$df outside of handsontable, such as if you click the "Add row" button in rapid succession:
## 1) state$df updates output$table
## 2) this updates input$table (see https://github.com/jrowen/rhandsontable/blob/master/inst/htmlwidgets/rhandsontable.js#L147) (but not in the Shiny busy loop!)
## 3) this results in an update to state$df yet again
## 4) and an update to output$table
## 5) fortunately, it stops there
## If during step 2 the state$df is updated once again, there will be two updates queued.
## The end result is an endless loop (!)
## 
## Both of these issues are resolved in the module below

#' An rhandsontable that is synchronized to a reactiveValues element
#' 
#' @name synchronizedHot
#' @param id unique identifier
#' @export
synchronizedHotUi <- function(id, ...) {
  ns <- NS(id)
  rhandsontable::rHandsontableOutput(outputId=ns("table"), ...)
}


#' @name synchronizedHot
#' @param expr expression used to create a rhandsontable. Make sure to isolate() calls to the state$df object. The observers
#' in this module will make sure the handsontable is recreated if a relevant change to state$df occurs.
#' @param stateDf a reactiveVal data.frame used to write the state
#' @param hot_to_r a function to convert the input$table object into a data.frame of the same format as stateDf
#' @param debug TRUE to enable printing debug messages
#' @export
synchronizedHot <- function(input, output, session, stateDf, expr, hot_to_r=rhandsontable::hot_to_r, debug=FALSE) {
  log <- function(...) {
    if(debug) cat(proc.time()['elapsed'], "::", ..., "\n")
  }
  #ensure expr can be run in a render function, maintaining the original environment
  fun <- shiny::exprToFunction(substitute(expr), quoted=TRUE, env=parent.frame(2))
  
  invalidateTable <- reactiveVal(value=NA)
  #Changing the output also triggers a change in the input$table
  output$table <- rhandsontable::renderRHandsontable({
    log("RENDER output$table")
    invalidateTable()
    fun()
  })
  observeEvent(stateDf(), {
    df <- stateDf()
    if(!is.null(input$table) && !isTRUE(all.equal(df, hot_to_r(input$table)))) {
      log("INVALIDATING output$table")
      invalidateTable(runif(1))
    } else {
      log("IGNORING state$df change; equal to input$table")
    }
  })
  
  observeEvent(input$table, {
    changes <- input$table$changes
    if(isTRUE(all.equal(changes, list(event="afterChange", changes=NULL)))) {
      # change is due to loadData updating the shiny input binding
      # ## changes: { event: "afterChange", changes: null },
      # See https://github.com/jrowen/rhandsontable/blob/master/inst/htmlwidgets/rhandsontable.js#L147
      # this should *not* propagate back to state$df
      log("IGNORING input$table change; source is output$table change")
    } else {
      value <- hot_to_r(input$table)
      log("EVENT input$table with changes ", capture.output(print(input$table$changes)), " storing to state$df as ", capture.output(print(value)), "\n")
      stateDf( value )
    }
  })
  
  return(df)
}

#' @export
singleReactive <- function(reactiveValues, key) {
  function(x) {
    if(missing(x)) reactiveValues[[key]]
    else reactiveValues[[key]] <- x
  }
}