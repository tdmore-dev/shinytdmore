#' Create a user interface to represent a table, with a "table" UI element and an "Add" button
#' @param id id for the encapsulating div
#' @param ... extra arguments passed to actionButton
#' @export
tableUI <- function(id, ...) {
  ns <- NS(id)
  div(
    id=id,
    synchronizedHotUi(ns("table")),
    actionButton(ns("add"), ..., style="margin-top: 5px;"),
    shinyBS::bsAlert(ns("alert"))
  )
}

# Get the formulations for the model
# Note: returns a shiny.silent.error if no formulation metadata was defined
getFormulationList <- function(model) {
  allFormulations <- tdmore::getMetadataByClass(model, "tdmore_formulation", all=TRUE)
  shiny::req(length(allFormulations) > 0)
  list <- vapply(allFormulations, function(form) {form$name}, FUN.VALUE=character(1))
  return(list)
}

# Create a reactive value that only updates/invalidates if the borders really change
# Beware, this creates a new reactive value at every call!
bordersReactiveVal <- function(tableDf, state) {
  borders <- reactiveVal()
  observe({
    borders <- getTableBorder(tableDf(), state$now)
    if(!isTRUE(all.equal(borders(), borders))) borders(borders) #only update if needed!
  })
  borders
}

#' Server component for doseTable
#' 
#' @param state `reactiveValues` object with the values `now`, `regimen` and `model`.
#' `now` can be absent, in which case the `now` time is not shown graphically in the table.
#' Also, the addDose command will then not use the current date by default.
#' 
#' `model` should at least contain formulation metadata. If not, the corresponding reactives
#' that set the output doseTable will show a silent shiny error (see [shiny::req()] for more information).
#' 
#' @inheritParams shinytdmore-module
#' 
#' @export
doseTable <- function(input, output, session, state) {
  defaultReactive <- reactive({
    tibble(time=as.POSIXct(character(0)), 
           dose=numeric(0), 
           formulation=factor(levels=getFormulationList(state$model)), 
           fix=logical(0))
  })
  tableDf <- singleReactive(state, "regimen",
                     default=defaultReactive,
                     to=function(x) {dplyr::arrange(x, .data$time)})
  borders <- bordersReactiveVal(tableDf, state)
  callModule(synchronizedHot, "table", 
             stateDf=tableDf, expr={
    colHeaders <- c("Dose", "Formulation", "Fix")
    
    df <- isolate({ tableDf() })
                  
    z <- timeTable(df, borders(), colHeaders=colHeaders)
    ## Use a custom editor, since the editor for dropdown is broken...
    z <- rhandsontable::hot_col(z, col="Formulation", type="dropdown", editor="select", selectOptions=levels(df$formulation))
    z
  }, hot_to_r=hot_to_r_datetime)
  observeEvent(input$add, {
    res <- tryCatch( addDose(state), error=function(e) e)
    if(!is.null(res)) {
      shinyBS::createAlert(session, session$ns("alert"), title="Error while creating dose", style="danger", content=htmltools::htmlEscape(capture.output(print(res))))
    }
  })
}

addDose <- function(state) {
  model <- state$model
  doses <- if(is.null(state$regimen)) tibble() else state$regimen
  now <- if(is.null(state$now)) as.POSIXct(NA) else state$now
  
  if(nrow(doses) > 0) {
    newdose <- doses[ nrow(doses), ] #last dose
    form <- tdmore::getMetadataByName(model, newdose$formulation) #get the appropriate formulation
    dosing_interval <- form$dosing_interval
    newdose$time <- newdose$time + dosing_interval * 3600
  } else {
    newdose <- tibble(time=now, dose=0, fix=FALSE)
    form <- tdmore::getMetadataByClass(model,"tdmore_formulation", all=FALSE) #first formulation
    if(is.null(form)) stop("Model does not specify any formulation metadata")
    newdose <- tibble::add_column(newdose, formulation=factor(form$name, levels=getFormulationList(state$model)), .before="fix")
    newdose$dose <- form$default_value
  }
  state$regimen <- rbind(doses, newdose)
  NULL #by default, return NULL
}

#' The recommendationTable uses the state$regimen and state$recommendation tables
#' @inheritParams shinytdmore-module
#' 
recommendationTable <- function(input, output, session, state) {
  defaultReactive <- reactive({
    tibble(time=as.POSIXct(character(0)), 
           dose=numeric(0), 
           formulation=factor(levels=getFormulationList(state$model)), 
           fix=logical(0))
  })
  tableDf <- singleReactive(state, "regimen",
                            default=defaultReactive,
                            to=function(x) {x %>% dplyr::select(-.data$recommendation) %>% dplyr::arrange(x, .data$time)})
  borders <- bordersReactiveVal(tableDf, state)
  callModule(synchronizedHot, "table", 
             stateDf=tableDf, expr={
               regimen <- state$recommendation
               shiny::req(regimen)
               df <- isolate({ tableDf() })
               shiny::req(nrow(df) == nrow(regimen)) #otherwise wait for update...
               df$recommendation <- regimen$AMT
               
               colHeaders <- c("Dose", "Formulation", "Fix", "Rec. Dose")
               
               z <- timeTable(df, borders(), colHeaders=colHeaders)
               ## Use a custom editor, since the editor for dropdown is broken...
               z <- rhandsontable::hot_col(z, col="Formulation", type="dropdown", editor="select", selectOptions=levels(df$formulation))
               z <- rhandsontable::hot_col(z, col="Rec. Dose", readOnly=TRUE)
               z
             }, hot_to_r=hot_to_r_datetime)
  observeEvent(input$add, {
    res <- tryCatch( addDose(state), error=function(e) e)
    if(!is.null(res)) {
      shinyBS::createAlert(session, session$ns("alert"), title="Error while creating dose", style="danger", content=htmltools::htmlEscape(capture.output(print(res))))
    }
  })
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
#' @inheritParams shinytdmore-module
#' 
#' @export
observationTable <- function(input, output, session, state) {
  tableDf = singleReactive(state, "observed", default=tibble(time=as.POSIXct(character(0)), dv=numeric(0), use=logical(0)),
                           to=function(x) {dplyr::arrange(x, .data$time)})
  borders <- bordersReactiveVal(tableDf, state)
  callModule(synchronizedHot, "table", stateDf=tableDf, expr={
    df <- isolate({ tableDf() })
    
    observedLabel <- getMeasureColumnLabel(state$model)
    colHeaders <- c(observedLabel, "Use")
    
    timeTable(df, borders(), colHeaders=colHeaders) %>%
      rhandsontable::hot_col("Use", halign = "htCenter")
  }, hot_to_r=hot_to_r_datetime)
  
  observeEvent(input$add, {
    res <- tryCatch( addObservation(state), error=function(e) e)
    if(!is.null(res)) {
      shinyBS::createAlert(session, session$ns("alert"), title="Error while creating observation", style="danger", content=htmltools::htmlEscape(capture.output(print(res))))
    }
  })
}

addObservation <- function(state) {
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
  NULL
}

#' Create a user interface to represent the covariates
#' 
#' The interface consists of a *table* (rhandsontable) that reads from the `state$covs` data.frame. It has at least two columns:
#' 
#' 1. `date` is a POSIXct date that is represented as YYYY/MM/DD in the table
#' 2. `time` is a character string HH:MM that is represented as a dropdown box
#' 3. and further: columns for each covariate in the model
#' 
#' The table has a horizontal line to reflect where `state$now` is situated. If `state$now` is empty, the line is not shown.
#' 
#' Edits to this observed table are automatically synchronized back to `state$covs`. The element ensures the rows are chronological.
#' 
#' The user interface also features a button to **add** new covariates. Adding to an empty table creates a line for an observation at **state$now**.
#' Adding to an existing table duplicates the last row. The time is shifted by `24 hours`.
#' 
#' 
#' @md
#' @inheritParams shinytdmore-module
#' @param state `reactiveValues` object with at least the values `now`, `observed` and `model`
#' 
#' @export
covariatesTable <- function(input, output, session, state) {
  defaultDf <- reactive({
    df <- tibble(time=as.POSIXct(character(0)))
    df[, state$model$covariates] <- numeric(0)
    df
  })
  tableDf = singleReactive(state, "covariates", default=defaultDf,
   to=function(x) {
     # discrete covariates are displayed 
     x <- dplyr::arrange(x, .data$time)
     
     # if a discrete choice, replace the labels back with the values
     for(i in colnames(x)) {
       cov <- tdmore::getMetadataByName(state$model, i)
       if(!is.null(cov$choices)) {
         #convert back to original values
         value <- cov$choices[ match(x[[i]], names(cov$choices) ) ]
         x[[i]] <- unlist(unname( value ) )
       }
     }
     
     x
  })
  borders <- bordersReactiveVal(tableDf, state)
  callModule(synchronizedHot, "table", stateDf=tableDf, expr={
    df <- isolate({ tableDf() })
    
    # setup column headers
    covsNames <- colnames(df)[-1] #names without first column (time)
    colHeaders <- vapply(covsNames, function(x){
      cov <- tdmore::getMetadataByName(state$model, x)
      if(is.null(cov)) return(x)
      
      unit <- if(is.null(cov$unit)) "" else paste0(" (", cov$unit, ")")
      paste0(cov$label, unit)
    }, FUN.VALUE=character(1), USE.NAMES=FALSE)
    
    # swap out discrete columns with their labels
    for(col in colnames(df)) {
      cov <- tdmore::getMetadataByName(state$model, col)
      if(!is.null(cov$choices)) df[[col]] <- factor( df[[col]], levels=cov$choices, labels=names(cov$choices) )
    }
    
    z <- timeTable(df, borders(), colHeaders=colHeaders)
    
    # setup column types
    for(col in seq_len(ncol(df) - 1) ) { #1 time column, N-1 covariate columns
      name <- colnames(df)[col+1]
      cov <- tdmore::getMetadataByName(state$model, name)
      value <- df[[col+1]]
      
      if( is.factor(value) ) {
        z <- rhandsontable::hot_col(z, col=col+2, # add extra column for date/time
                                    type="dropdown", editor="select", selectOptions=levels(value))
      } else {
        # numeric
        if(is.null(cov)) next #no metadata defined
        f <- js_validate_numeric(min=cov$min, max=cov$max)
        z <- rhandsontable::hot_col(hot=z, col=col+2, type="numeric", validator=f, allowInvalid=FALSE)
      }
    }
    
    z
  }, hot_to_r=hot_to_r_datetime)
  
  observeEvent(input$add, {
    res <- tryCatch( addCovariate(state, tableDf()), error=function(e) e)
    if(!is.null(res)) {
      shinyBS::createAlert(session, session$ns("alert"), title="Error while creating covariate", style="danger", content=htmltools::htmlEscape(capture.output(print(res))))
    }
    
  })
}

## https://github.com/jrowen/rhandsontable/issues/337
## PhantomJS does not support ES6 javascript, so we write our own validation function
## that does not use ES6 javascript
js_validate_numeric <- function(min, max) {
  paste0("function (value, callback) {
          if (value === null || value === void 0) {
            value = '';
          }
          if (this.allowEmpty && value === '') {
            return callback(true);
          } else if (value === '') {
            return callback(false);
          }
          var isNumber = /^-?\\d*(\\.|,)?\\d*$/.test(value);
          if (!isNumber) {
            return callback(false);
          }
          if (isNaN(parseFloat(value))) {
            return callback(false);
          }
          
          if (value <= ", min, ") { return callback(false); }
          if (value >= ", max, ") { return callback(false); }
          
          return callback(true);
        }")
}

addCovariate <- function(state, df) {
  now <- if(is.null(state$now)) as.POSIXct(NA) else state$now
  dosingInterval <- 24 #in hours
  
  if(nrow(df) > 0) {
    newrow <- df[ nrow(df), ] #last dose
    newrow$time <- newrow$time + dosingInterval*3600
  } else {
    newrow <- tibble(time=now)
  }
  # set default values, except where covariates already defined
  # we disgregard existing columns with no correspondance to current model covariates
  covs <- setdiff( state$model$covariates, names(newrow)[ !is.na(newrow) ] )
  newrow[, covs] <- as.numeric(NA)
  
  state$covariates <- dplyr::bind_rows(state$covariates, newrow)
  NULL
}


