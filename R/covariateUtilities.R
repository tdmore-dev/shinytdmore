#' Get all covariate names of a model.
#' By default, covariate names from the model are returned.
#' For MPC models, only the 'true' covariates are returned (e.g. WT, not TVCl).
#'
#' @param model tdmore model
#' @return a character vector with the covariates
#' 
getCovariateNames <- function(model) {
  covariates <- model$covariates
  
  if (inherits(model, "tdmore_mpc")) {
    includedCovariates <- names(model$mpc_theta)
    return(covariates[!(covariates %in% includedCovariates)])
  } else {
    return(covariates)
  }
}

