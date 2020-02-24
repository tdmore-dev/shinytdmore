#' @title Data format description
#' @name shinytdmore-data
#' 
#' @md
#' @details 
#' Shinytdmore uses the following data objects in a reactiveValues() state:
#' * `state$model` a tdmore model with (ideally) metadata
#' * `state$regimen` a data.frame with the following columns:
#'   * `time` a POSIXct time
#'   * `dose` a numeric with the dose amount
#'   * `formulation` a character vector that corresponds to formulations defined in the metadata of the model
#'   * `fix` a boolean vector describing whether the given regimen can be modified in dose recommendation
#' * `state$observed` a data.frame with the following columns:
#'   * `time` a POSIXct time
#'   * `dv` the observed data
#'   * `use` whether the observation should be used in the estimation
#' * `state$covariates` a data.frame with the following columns:
#'   * `time` a POSIXct time
#'   * `...` columns corresponding to covariates required in the tdmore model. If columns are missing, the method returns an error.
#' * `state$now` a POSIXct time representing the current time
#' 
#' [convertDataToTdmore()] converts this into a list of arguments compatible with [tdmore::estimate], [tdmore::predict.tdmore] and [tdmore::findDose]. 
#' * `model` the tdmore model
#' * `regimen` all treatments in the right format
#' * `observed` all observed values where use=TRUE, and with the `dv` column renamed to the default model output
#' * `covariates` all covariates, with an added covariate `FORM` that contains the formulation
#' * `t0` a POSIXct time corresponding to time `0`
#' * `now` a numeric time, corresponding to the different between t0 and now
#' * `doseRows` numeric vector specifying which rows can be modified: rows in the future where fix==FALSE
#' 
#' Please note that `target` is missing here. This is determined by the optimization routines in `tdmore` itself.
#' 
#' @param state reactiveValues() or list-like object with input data, see [shinytdmore-data] for more information
#' @param model a tdmore model with (ideally) metadata
#' @param regimen a data.frame with the following columns:
#'   * `time` a POSIXct time
#'   * `dose` a numeric with the dose amount
#'   * `formulation` a character vector that corresponds to formulations defined in the metadata of the model
#'   * `fix` a boolean vector describing whether the given regimen can be modified in dose recommendation
#' @param observed a data.frame with the following columns:
#'   * `time` a POSIXct time
#'   * `dv` the observed data
#'   * `use` whether the observation should be used in the estimation
#' @param covariates a data.frame with the following columns:
#'   * `time` a POSIXct time
#'   * `...` columns corresponding to covariates required in the tdmore model. If columns are missing, the method returns an error.
#' @param now a POSIXct time representing the current time
#' @param target a named list with min/max values to represent the target concentration
#' @format NULL
#' @usage NULL
NULL

`%||%` <- function(a, b){
  if(!is.null(a)) a else b
}

#' @describeIn shinytdmore-data Default data
#' @export
#' @format NULL
#' @usage NULL
defaultData <- function() {
  list(
    regimen=tibble::tibble(time=as.POSIXct(character()), dose=numeric(), formulation=character(), fix=logical()),
    observed=tibble::tibble(time=as.POSIXct(character()), dv=numeric(), use=logical()),
    covariates=tibble::tibble(time=as.POSIXct(character())), #rest of columns depend on model
    now=as.POSIXct(as.character(NA)),
    target=list(min=as.numeric(NA), max=as.numeric(NA))
  )
}

#' @describeIn shinytdmore-data Convert shinytdmore data to tdmore format
#' @param force if FALSE, emits a silent error (see [shiny::req()]) when no shiny data is available. If TRUE, tries to generate the args anyway.
#' @importFrom dplyr filter transmute full_join select mutate arrange everything
#' @export
#' @usage NULL
convertDataToTdmore <- function(state, force=FALSE) {
  model <- state$model
  regimen <- state$regimen %||% defaultData()$regimen
  observed <- state$observed %||% defaultData()$observed
  covariates <- state$covariates %||% defaultData()$covariates
  now <- state$now %||% defaultData()$now
  
  result <- list()
  result$model <- model
  allTimes <- stats::na.omit( c(regimen$time, observed$time, covariates$time, now) )
  if(length(allTimes)==0) {
    if(force) {
      result$t0 <- as.POSIXct(as.character(NA))
    } else {
      shiny::req(FALSE)
    }
    
  } else {
    result$t0 <- min(allTimes)
  }
  
  result$now <- as.numeric( difftime(now, result$t0, units="hours") )
  result$regimen <- regimen %>% transmute(
    TIME = as.numeric(difftime(.data$time, result$t0, units="hours")),
    AMT = .data$dose,
    FIX = .data$fix,
    FORM = .data$formulation
  )
  if( !is.null(model$iov) ) result$regimen$OCC <- seq_along(result$regimen$AMT) # add IOV column
  
  result$doseRows <- which( regimen$time > now & !regimen$fix )
  
  result$covariates <- covariates %>%
    mutate(
      TIME = as.numeric(difftime(.data$time, result$t0, units="hours"))
    ) %>% select(.data$TIME, everything()) %>% select(-.data$time) %>%
    dplyr::bind_rows(tibble(TIME=0)) %>% #add time '0'
    arrange(.data$TIME) %>%
    tidyr::fill(everything(), .direction="downup")
  if(nrow(covariates)==0) result$covariates <- result$covariates %>% filter(FALSE) #filter out everything
  
  replaceName <- function(df, old, new) { #programmatic version of dplyr::rename
    if(is.null(old)) return(df)
    if(is.null(new)) {
      df[old] <- NULL #remove old
      return(df)
    }
    stats::setNames(df, replace(names(df), which(names(df)==old), new) )
  }
  result$observed <- observed %>% 
    filter(.data$use) %>% select(-.data$use) %>% #only use points where use==TRUE
    filter(.data$time < now) %>% #only before NOW
    replaceName("dv", getModelOutput(model)) %>%
    mutate(
      TIME = as.numeric(difftime(.data$time, result$t0, units="hours"))
    ) %>% select(.data$TIME, everything()) %>% select(-.data$time)

  result
}