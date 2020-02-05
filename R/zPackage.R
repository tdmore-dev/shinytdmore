`%>%` <- magrittr::`%>%`
tibble <- tibble::tibble
add_column <- tibble::add_column
as.tibble <- tibble::as.tibble
bind_cols <- dplyr::bind_cols

#' @import shiny
#' @import ggplot2
#' @importFrom stats runif
#' @importFrom utils capture.output
NULL

#' Basic definition for module server components
#' 
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param state a reactiveValues object with the state of the application. See [convertDataToTdmore()] for more information.
#' @name shinytdmore-module
NULL

#' Data definition
#' 
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
#' @md
#' 
#' @name shinytdmore-data
NULL