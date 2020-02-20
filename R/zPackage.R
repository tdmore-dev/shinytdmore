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
#' @param cr list of calculation-related reactives, see [calculationReactives()]
#' @name shinytdmore-module
NULL