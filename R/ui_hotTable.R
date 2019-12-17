## This file contains modules for a dose editing table or observation editing table

#' Create a user interface to represent the doses for a patient
#' 
#' The interface consists of a *table* (rhandsontable) that reads from the `state$regimen` data.frame. It has five columns:
#' 
#' 1. `date` is a POSIXct date that is represented as YYYY/MM/DD in the table
#' 2. `time` is a character string HH:MM that is represented as a dropdown box
#' 3. `dose` is a numeric vector. If the `state$model` is available, the column title is adapted.
#' 4. `form` is a factor that shows the formulation used. If the `state$model` is available, the options are taken from the model. If not, this is free-form.
#' 5. `fix` is a logical that shows whether the dose can be modified by the recommendation algorithm. The column is hidden from view.
#' 
#' The table has a horizontal line to reflect where `state$now` is situated. If `state$now` is empty, the line is not shown.
#' 
#' Edits to this dosing table are automatically synchronized back to `state$regimen`. The element ensures the administrations are chronological.
#' 
#' The user interface also features a button to **add** new doses. Adding to an empty table creates a line for a dose at **state$now**.
#' Adding to an existing table duplicates the last row. The time is shifted by either the inter-dose interval defined in the model meta-data for
#' the given formulation, or by `24` if this is not available.
#' 
#' @md
#' @name doseTable
#' @export
doseTableUI <- function(id) {
  ns <- NS(id)
  div(
    rhandsontable::rHandsontableOutput(ns("table")),
    actionButton(ns("add"), "Add dose", style="margin-top: 5px;")
  )
}

#' @name doseTable
#' @param state `reactiveValues` object with at least the values `now`, `regimen` and `model`
#' 
#' @export
doseTable <- function(input, output, session, state) {
  ns <- session$ns
  output$table <- rhandsontable::renderRHandsontable({
    doses <- state$regimen
    if(is.null(doses)) doses <- tibble(date=as.POSIXct(character(0)), time=character(0), dose=numeric(0), form=numeric(0), fix=logical(0))
    now <- state$now
    if(is.null(now)) now <- Sys.time()
    model <- state$model
    
    borderRow <- getTableBorderIndex(doses, now, T)
    doseLabel <- getDoseColumnLabel(model)
    
    ## you need to format the dates manually to a character string
    doses$date <- format(doses$date, format="%Y-%m-%d")
    
    z <- rhandsontable::rhandsontable(
      doses,
      useTypes = TRUE,
      stretchH = "all",
      rowHeaders = NULL,
      colHeaders = c("Date", "Time", doseLabel,"Formulation", "Fix")
    )
    z <- timeColumn(z)
    z <- dateColumn(z, dateFormat = "YYYY-MM-DD")
    
    z <- z %>% rhandsontable::hot_col(col="Formulation", type="dropdown", source=getFormulationList(model), autocomplete=TRUE, strict=TRUE) 
    z <- z %>% rhandsontable::hot_col(col = 'Fix', colWidths=0.1) # Trick to hide column, see https://github.com/jrowen/rhandsontable/issues/249
    z <- z %>% rhandsontable::hot_table(
      customBorders = list(list(
        range=list(
          from=list(row=borderRow-1, col=0), 
          to=list(row=borderRow, col=ncol(doses)-1)),
          top=list(width=2, color=nowColorHex())
      ))
    )
    
    z
  })
  observeEvent(input$table, {
    # if the table changes, update the state
    # but only if they are different!!
    df <- rhandsontable::hot_to_r(input$table) #careful, 'date' is a string
    df$date <- as.POSIXct(df$date, format="%Y-%m-%d")
    value <- autoSortByDate(df)
    if(!isTRUE(all.equal( state$regimen, value ))) {
      # they do not match... update!
      state$regimen <- value
    }
  })
  observeEvent(input$add, {
    addDose(state)
  })
}

addDose <- function(state) {
  model <- state$model
  doses <- state$regimen
  
  formulations <- tdmore::getMetadataByClass(model,"tdmore_formulation")
  doseMetadata <- tdmore::getMetadataByName(model, formulations[[1]]$name)
  dosingInterval <- if(is.null(doseMetadata)) {24} else {doseMetadata$dosing_interval} #in hours
  
  if(nrow(doses) > 0) {
    newdose <- doses[ nrow(doses), ] #last dose
    dosetime <- dateAndTimeToPOSIX(newdose$date, newdose$time) + dosingInterval*3600
  } else {
    newdose <- tibble(date="", time="", dose=if(is.null(doseMetadata)) {0} else {doseMetadata$default_value}, form="", fix=FALSE)
    dosetime <- Sys.time()
  }
  newdose$date <- POSIXToDate(dosetime)
  newdose$time <- POSIXToTime(dosetime)
  
  state$regimen <- rbind(state$regimen, newdose)
}

timeColumn <- function(hot, ...) {
  #grid <- expand.grid(pad(c(0,30)), pad(0:23))
  grid <- expand.grid(pad(0:59), pad(0:23) )
  source <- paste0(grid$Var2, ":", grid$Var1)
  rhandsontable::hot_col(hot, col="Time", source=source, allowInvalid=FALSE, strict = TRUE, ...)
}


dateColumn <- function(hot, ...) {
  ## rhandsontable formats all dates as "MM/DD/YYYY" by default...
  ## with no way to change this hardcoded behavior
  ## 
  ## we fix it afterwards
  rhandsontable::hot_col(hot, col="Date", 
                         strict = TRUE, type="date", 
                         allowInvalid = FALSE,...)
}

observationHot <- function(observed, now) {
  borderRow <- getTableBorderIndex(observed, now, F)
  rhandsontable::rhandsontable(observed, useTypes = TRUE, stretchH = "all", rowHeaders = NULL,
                               colHeaders = c("Date", "Time", getMeasureColumnLabel(val$model), "Use")) %>%
    rhandsontable::hot_col("Use", halign = "htCenter") %>%
    timeColumn() %>%
    rhandsontable::hot_table(customBorders = list(list(
      range=list(from=list(row=borderRow-1, col=0), to=list(row=borderRow, col=ncol(observed)-1)),
      top=list(width=2, color=nowColorHex()))))
}