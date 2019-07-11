assert_that <- assertthat::assert_that
is.string <- assertthat::is.string

#' Check firstname.
#' 
#' @param input user input
#' 
checkFirstname <- function(input) {
  assert_that(is.string(input), msg = "firstname is not a string")
}

#' Check lastname.
#' 
#' @param input user input
#' 
checkLastname <- function(input) {
  assert_that(is.string(input), msg = "lastname is not a string")
}

#' Check model name.
#' 
#' @param input user input
#' 
checkModelName <- function(input) {
  assert_that(is.string(input), msg = "model name is not a string")
}

#' Check covariates.
#' 
#' @param covariates named numeric with the covariates
#' 
checkCovariates <- function(covariates) {
  if(is.null(covariates)) {
    return()
  }
  covariates <- unlist(covariates)
  assert_that(is.numeric(covariates), msg = "covariates is not numeric")
  assert_that(!is.null(names(covariates)), msg = "covariates are not named")
}