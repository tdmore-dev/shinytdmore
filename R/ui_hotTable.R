## This file contains modules for a dose editing table or observation editing table
# TODO: handle the debounce problem elegantly
# RHandsontable can be edited while the interface is updating.
# It can even be edited WHILE a Reactive path is still being updated.
# This results in the following situation:
# Edit action 1 occurs
# HOT sends a Change message
# input$table updates state$regimen
#                                         Edit action 2 occurs
# state$regimen updates output$table
# Table is re-rendered, which is treated as an update of the contents
# The update is considered as a new modification

# The elegant way to solve this, may lie in one of several solutions:
# 0) Debug the core of rhandsontable
# 1) Try to detect whether a re-render of the table is really necessary.
# We can do this by comparing the state$regimen to output$table.
# Problem is that this only sidesteps the problem: there are still changes in the table
# that may require a re-render...
# 2) Add some sort of intermediate reactive.
# 3) Disable EDIT on the table while it rerenders.
# 4) play with priority?
# The core of the problem is difficult to solve: a well-skilled timing
# can result in a loop of updates.

### TODO: add support for futureDoses and covariates tables



#' Create a user interface to represent the doses for a patient
#' 
#' The interface consists of a *table* (rhandsontable) that reads from the `state$regimen` data.frame. It has five columns:
#' 
#' 1. `time` is a POSIXct date that is represented as two seperate columns YYYY/MM/DD and HH:MM in the table
#' 2. `dose` is a numeric vector. If the `state$model` is available, the column title is adapted.
#' 3. `formulation` is a factor that shows the formulation used. If the `state$model` is available, the options are taken from the model. If not, this is free-form.
#' 4. `fix` is a logical that shows whether the dose can be modified by the recommendation algorithm. The column is hidden from view.
#' 
#' The table has a horizontal line to reflect where `state$now` is situated. If `state$now` is empty, the line is not shown.
#' 
#' Edits to this dosing table are automatically synchronized back to `state$regimen`. The element ensures the administrations are chronological.
#' 
#' The user interface also features a button to **add** new doses. Adding to an empty table creates a line for a dose at **state$now**.
#' Adding to an existing table duplicates the last row. The time is shifted by either the inter-dose interval defined in the model meta-data for
#' the given formulation, or by `24` if this is not available.
#' 
#' @param id identifier for the encapsulating div
#' @param ... applied to the encapsulating div
#' @md
#' @name doseTable
#' @export
doseTableUI <- function(id) {
  ns <- NS(id)
  div(
    id=id,
    synchronizedHotUi(ns("table")),
    actionButton(ns("add"), "Add dose", style="margin-top: 5px;")
  )
}

#' @name doseTable
#' @param state `reactiveValues` object with the values `now`, `regimen` and `model`
#' 
#' @export
doseTable <- function(input, output, session, state) {
  hot_to_r <- function(x){
    if(is.null(x)) {
      return( tibble(time=as.POSIXct(character(0)), dose=numeric(0), formulation=numeric(0), fix=logical(0)) )
    }
    df <- rhandsontable::hot_to_r(x)
    df$time <- as.POSIXct(paste(df$date, df$time), format="%Y-%m-%d %H:%M")
    value <- df %>% select(-date)
    value
  }
  stateDf = function(x) {
    if(missing(x)) return(state$regimen)
    else {
      x <- arrange(x, time) #always sort the input before writing it to the state regimen
      state$regimen <- x
    }
  }
  callModule(synchronizedHot, "table", stateDf=stateDf, expr={
    doses <- isolate({state$regimen})
    if(is.null(doses)) doses <- hot_to_r(NULL)
    now <- state$now
    
    borderRow <- getTableBorderIndex(doses, now)
    
    model <- state$model
    
    doses$date <- format(doses$time, format="%Y-%m-%d")
    doses$time <- format(doses$time, format="%H:%M")
    doses <- doses[, c("date", "time", "dose", "formulation", "fix")]
    
    doseLabel <- getDoseColumnLabel(model)
    
    z <- rhandsontable::rhandsontable(
      doses,
      useTypes = TRUE,
      stretchH = "all",
      rowHeaders = NULL,
      colHeaders = c("Date", "Time", doseLabel,"Formulation", "Fix"),
      selectCallback=TRUE #force callback on every select, see https://github.com/jrowen/rhandsontable/issues/325
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
  }, hot_to_r=hot_to_r)
  observeEvent(input$add, {
    model <- state$model
    doses <- if(is.null(state$regimen)) tibble() else state$regimen
    now <- if(is.null(state$now)) as.POSIXct(NA) else state$now
    
    formulations <- tdmore::getMetadataByClass(model,"tdmore_formulation")
    doseMetadata <- tdmore::getMetadataByName(model, formulations[[1]]$name)
    dosingInterval <- if(is.null(doseMetadata)) {24} else {doseMetadata$dosing_interval} #in hours
    
    if(nrow(doses) > 0) {
      newdose <- doses[ nrow(doses), ] #last dose
      newdose$time <- newdose$time + dosingInterval*3600
    } else {
      newdose <- tibble(time=now, dose=if(is.null(doseMetadata)) {0} else {doseMetadata$default_value}, formulation="", fix=FALSE)
    }
    state$regimen <- rbind(doses, newdose)
  })
}

timeColumn <- function(hot, ...) {
  #grid <- expand.grid(pad(c(0,30)), pad(0:23))
  grid <- expand.grid(pad(0:59), pad(0:23) )
  source <- paste0(grid$Var2, ":", grid$Var1)
  rhandsontable::hot_col(hot, col="Time", source=source, allowInvalid=FALSE, strict = TRUE, autocomplete=TRUE, ...)
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







#' Create a user interface to represent the observations
#' 
#' The interface consists of a *table* (rhandsontable) that reads from the `state$observed` data.frame. It has five columns:
#' 
#' 1. `date` is a POSIXct date that is represented as YYYY/MM/DD in the table
#' 2. `time` is a character string HH:MM that is represented as a dropdown box
#' 3. `dv` is a numeric vector. If the `state$model` is available, the column title is adapted.
#' 5. `use` is a logical that shows whether the observation will be used by the recommendation algorithm.
#' 
#' The table has a horizontal line to reflect where `state$now` is situated. If `state$now` is empty, the line is not shown.
#' 
#' Edits to this observed table are automatically synchronized back to `state$observed`. The element ensures the rows are chronological.
#' 
#' The user interface also features a button to **add** new observations. Adding to an empty table creates a line for an administration at **state$now**.
#' Adding to an existing table duplicates the last row. The time is shifted by `24 hours`.
#' 
#' @md
#' @name observationTable
#' @export
observationTableUI <- function(id) {
  ns <- NS(id)
  div(
    synchronizedHotUi(ns("table")),
    actionButton(ns("add"), "Add observation", style="margin-top: 5px;")
  )
}

#' @name observationTable
#' @param state `reactiveValues` object with at least the values `now`, `observed` and `model`
#' 
#' @export
observationTable <- function(input, output, session, state) {
  hot_to_r <- function(x){
    if(is.null(x)) {
      return( tibble(time=as.POSIXct(character(0)), dv=numeric(0), use=logical(0)) )
    }
    df <- rhandsontable::hot_to_r(x)
    df$time <- as.POSIXct(paste(df$date, df$time), format="%Y-%m-%d %H:%M")
    value <- df %>% select(-date)
    value
  }
  stateDf = function(x) {
    if(missing(x)) return(state$observed)
    else {
      x <- arrange(x, time) #always sort the input before writing it to the state regimen
      state$observed <- x
    }
  }
  callModule(synchronizedHot, "table", stateDf=stateDf, expr={
    df <- isolate({ state$observed })
    if(is.null(df)) df <- hot_to_r(NULL)
    now <- state$now
    model <- state$model
    
    borderRow <- getTableBorderIndex(df, now)
    observedLabel <- getMeasureColumnLabel(model)
    
    ## you need to format the dates manually to a character string
    df$date <- format(df$time, format="%Y-%m-%d")
    df$time <- format(df$time, format="%H:%M")
    df <- df[, c("date", "time", "dv", "use")]
    
    z <- rhandsontable::rhandsontable(df,
                                      useTypes = TRUE, 
                                      stretchH = "all", 
                                      rowHeaders = NULL,
                                      colHeaders = c("Date", "Time", observedLabel, "Use")) %>%
      rhandsontable::hot_col("Use", halign = "htCenter") %>%
      timeColumn() %>%
      dateColumn(dateFormat = "YYYY-MM-DD") %>%
      rhandsontable::hot_table(customBorders = list(list(
        range=list(
          from=list(row=borderRow-1, col=0), 
          to=list(row=borderRow, col=ncol(df)-1)
        ),
        top=list(width=2, color=nowColorHex()))))
    z
  }, hot_to_r=hot_to_r, debug=TRUE)
  
  observeEvent(input$add, {
    df <- if(is.null(state$observed)) tibble() else state$observed
    now <- if(is.null(state$now)) as.POSIXct(NA) else state$now
    dosingInterval <- 24 #in hours
    
    if(nrow(df) > 0) {
      newrow <- df[ nrow(df), ] #last dose
      newrow$time <- newrow$time + dosingInterval*3600
    } else {
      newrow <- tibble(time=now, dv=0, use=TRUE)
    }
    
    state$observed <- rbind(state$observed, newrow)
  })
}

#'
#' Utility function to know where the horizontal line corresponding to the now date should be.
#'
#' @param data a data frame that has a date and time column
#' @param now now date
#' @param dose logical value, true if doses table, false if measures table
#' @return index of this horizontal line or integer(0) if not pertinent
#'
getTableBorderIndex <- function(data, now) {
  if(is.null(now)) return(integer())
  if(is.null(data)) return(integer())
  
  length <- nrow(data)
  if (length==0) {
    return(integer())
  }
  index <- which(data$time >= now)
  
  if (length(index)!=0) {
    if (length(index) > 1) {
      index <- index[1]
    }
    if (index==1) {
      return(integer())
    }
    else {
      return(index)
    }
    return(index)
  } else {
    return(integer())
  }
}