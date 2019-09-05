#'
#' Get default model.
#' If the given model is not a mixture model, this model is returned.
#' If the given model is a mixture_model, the most likely model is returned.
#' 
#' @param model tdmore model or tdmore_mixture model
#' @return a tdmore model
#' @export
#'
getDefaultModel <- function(model) {
  if (inherits(model, "tdmore_mixture")) {
    return(model$models[[which.max(model$probs)]])
  } else {
    return(model)
  }
}

#'
#' Get winner fit.
#' If the given fit is not a tdmorefit mixture, this fit is returned.
#' If the given fit is a tdmorefit mixture, the winner fit is returned.
#' 
#' @param fit tdmore fit object
#' @return a tdmore model
#' @export
#'
getWinnerFit <- function(fit) {
  if (inherits(fit, "tdmorefit_mixture")) {
    fits <- fit$fits
    mixture <- fit$mixture
    winner <- fit$winner
    print(winner)
    return(fits[[winner]])
  } else {
    return(fit)
  }
}