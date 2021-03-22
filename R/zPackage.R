#' @import shiny
#' @import ggplot2
#' @importFrom stats runif
#' @importFrom utils capture.output
#' @importFrom magrittr %>%
#' @importFrom tibble tibble add_column as.tibble
#' @importFrom dplyr bind_cols
NULL

#' Basic definition for module server components
#' 
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param state a reactiveValues object with the state of the application. See [convertDataToTdmore()] for more information.
#' @param cr list of calculation-related reactives, see [calculationReactives()]
#' @name shinytdmore-module
NULL