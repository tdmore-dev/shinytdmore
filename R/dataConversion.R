`%||%` <- function(a, b){
  if(!is.null(a)) a else b
}

#' Convert shinyTDMore domain to TDMore domain
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
#' This is converted into a list of arguments compatible with [tdmore::estimate], [tdmore::predict] and [tdmore::findDose]. 
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
#' @param state reactiveValues() or list-like object with input data
#' @return named list of tdmore-compatible values
#' @importFrom dplyr filter transmute full_join select mutate arrange everything
#' @export
#'
convertDataToTdmore <- function(state) {
  model <- state$model
  shiny::req(model)
  regimen <- state$regimen %||% tibble(time=as.POSIXct(character(0)), dose=numeric(0), formulation=character(0), fix=logical(0))
  observed <- state$observed %||% tibble(time=as.POSIXct(character(0)), dv=numeric(0), use=logical(0))
  covariates <- state$covariates %||% tibble::tibble(time=as.POSIXct(character(0)))
  now <- state$now %||% as.POSIXct(NA)
  
  result <- list()
  result$model <- model
  result$t0 <- min(c(regimen$time, observed$time, covariates$time))
  result$now <- as.numeric( difftime(result$t0, now, units="hours") )
  result$regimen <- regimen %>% transmute(
    TIME = as.numeric(difftime(.data$time, result$t0, units="hours")),
    AMT = .data$dose,
    FIX = .data$fix,
    FORM = .data$formulation
  )
  if( !is.null(model$iov) ) result$regimen$OCC <- seq_along(result$regimen$AMT) # add IOV column
  
  result$doseRows <- which( regimen$time > now & !regimen$fix )
  result$covariates <- full_join(
    covariates,
    regimen %>% select(.data$time, .data$formulation)
  ) %>% arrange(.data$time) %>%
    mutate(
      TIME = as.numeric(difftime(.data$time, result$t0, units="hours"))
    ) %>% select(.data$TIME, everything()) %>% select(-.data$time)
  
  result$observed <- observed %>% 
    filter(.data$use) %>% select(-.data$use) %>% #only use points where use==TRUE
    filter(.data$time < now) %>% #only before NOW
    setNames(., replace(names(.), names(.)=="dv", getModelOutput(model)) ) %>%
    mutate(
      TIME = as.numeric(difftime(.data$time, result$t0, units="hours"))
    ) %>% select(.data$TIME, everything()) %>% select(-.data$time)

  result
}