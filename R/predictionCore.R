#'
#' Get model output variable name.
#' 
#' @param model tdmore model
#' @return the model output variable name, like 'CONC'
#'
getModelOutput <- function(model) {
  return(model$res_var[[1]]$var) # TODO: what if several outputs?
}

#'
#' Retrieve the dosing interval from the tdmore metadata according to the specified formulation.
#' If no formulation matches, dosing interval of the first formulation is returned.
#' If no formulation at all in metadata, 24 hours is returned.
#'
#' @param model tdmore model
#' @param formulation formulation we are looking for, character, can be NULL
#' @return a dosing interval, numeric value
#'
getDosingInterval <- function(model, formulation=NULL) {
  formulations <- getMetadataByClass(model, "tdmore_formulation")
  
  results <- formulations[sapply(formulations, function(x) {!is.null(formulation) && x$name==formulation})]
  if (length(results) > 0) {
    return(results[[1]]$dosing_interval)
  
  } else if (length(formulations) > 0) {
    return(formulations[[1]]$dosing_interval)
  
  } else {
    return(24)
  }
}

#' Prepare population OR individual prediction data.
#' 
#' @param doses doses
#' @param obs observations
#' @param model tdmore/tdmore_mpc/tdmore_mixture model
#' @param covs covariates
#' @param target numeric vector of size 2, min and max value
#' @param population logical value, true for population, false for individual
#' @param now now date, POSIXlt date
#' @return the prediction data and the tdmore data
#' @export
#'
preparePrediction <- function(doses, obs, model, covs, target, population, now) {
  if(nrow(doses)==0) {
    stop("Please add a dose in the left panel")
  }
  defaultModel <- getDefaultModel(model)
  tdmoreData <- convertDataToTdmore(defaultModel, doses, obs, covs, now)
  
  # Retrieving data
  regimen <- tdmoreData$regimen %>% dplyr::select(-PAST, -FORM)
  covariates <- tdmoreData$covariates
  filteredObserved <- tdmoreData$filteredObserved
  firstDoseDate <- tdmoreData$firstDoseDate
  
  # Model info
  isMixture <- inherits(model, "tdmore_mixture")
  isMpc <- inherits(defaultModel, "tdmore_mpc")
  observedVariables <- getObservedVariables(defaultModel)
  
  # Compute fit (even for population)
  if (population) {
    # Population 'fit'
    object <- estimate(model, regimen=regimen, covariates=covariates)
  } else {
    # Fit
    object <- estimate(model, observed=filteredObserved, regimen=regimen, covariates=covariates)
  }
  
  # Predictions
  dosingInterval <- getDosingInterval(defaultModel)
  maxTime <- if(nrow(regimen)==0){dosingInterval} else {max(regimen$TIME)+dosingInterval}
  newdata <- getNewdata(start=0, stop=maxTime, output=getModelOutput(defaultModel), observedVariables=observedVariables)
  
  if (isMpc && !population) {
    data <- predict(object, newdata=newdata, regimen=regimen, covariates=object$covariates, se.fit=F)
  } else {
    data <- predict(object, newdata=newdata, regimen=regimen, covariates=object$covariates, se.fit=T, level=0.95) # 95% CI by default
  }
  data$TIME <- firstDoseDate + data$TIME*60*60
  
  # In case of fit, compute PRED median as well (blue line in fit plot)
  if (!population) {
    pred <- predict(model, newdata=newdata, regimen=regimen, covariates=covariates, se=F)
    data$PRED <- pred[, getModelOutput(defaultModel)]
    for (parameter in observedVariables) {
      data[, paste0("PRED_", parameter)] <- pred[, parameter]
    }
  }
  
  return(list(predictionData=data, tdmoreData=tdmoreData, fit=getWinnerFit(object)))
}

#'
#' Prepare recommendation.
#' 
#' @param doses doses
#' @param obs observations
#' @param model tdmore/tdmore_mpc/tdmore_mixture model
#' @param covs covariates
#' @param target numeric vector of size 2, min and max value
#' @param now now date, POSIXlt date
#' 
#' @return a list with pred, ipred and the recommendation
#' @export
#' @importFrom dplyr last mutate pull select
#'
prepareRecommendation <- function(doses, obs, model, covs, target, now) {
  if (nrow(doses)==0) {
    stop("Please add a dose in the left panel")
  }
  defaultModel <- getDefaultModel(model)
  tdmoreData <- convertDataToTdmore(defaultModel, doses, obs, covs, now)
  regimen <- tdmoreData$regimen
  covariates <- tdmoreData$covariates
  filteredObserved <- tdmoreData$filteredObserved
  firstDoseDate <- tdmoreData$firstDoseDate
  isMpc <- inherits(model, "tdmore_mpc")
  
  # Find dose rows to be adapted
  doseRows <- which(!regimen$PAST)
  if (length(doseRows)==0) {
    stop("There is no dose in the future")
  }
  
  # Compute fit
  fit <- estimate(model, observed=filteredObserved, regimen=regimen %>% dplyr::select(-PAST, -FORM), covariates=covariates)
  winningFit <- getWinnerFit(fit)
  
  # Implementing the iterative process
  nextRegimen <- regimen %>% dplyr::select(-PAST, -FORM)
  
  # Dosing interval of last formulation found is used
  dosingInterval <- getDosingInterval(defaultModel, regimen %>% dplyr::pull(FORM) %>% dplyr::last())
  output <- getModelOutput(defaultModel)
  
  for (index in seq_along(doseRows)) {
    row <- regimen[doseRows[index],]
    last <- index == length(doseRows)
    
    if (last) {
      nextTime <- row$TIME + dosingInterval # By default, II
    } else {
      nextTime <- regimen[doseRows[index + 1],]$TIME - 0.001 # Just before the next dose
    }
    targetDf <- data.frame(TIME=nextTime)
    targetDf[, output] <- (target$min + target$max)/2
    recommendation <- findDose(winningFit, regimen=nextRegimen, doseRows=doseRows[(index:length(doseRows))], target=targetDf)
    nextRegimen <- recommendation$regimen
  }
  
  firstDoseInFutureTime <- regimen$TIME[doseRows[1]]
  covsToUse <- if(isMpc){fit$covariates}else{covariates}
  
  # Predict ipred without adapting the dose
  newdata <- getNewdata(start=0, stop=max(regimen$TIME) + dosingInterval, output=output)
  ipred <-  predict(fit, newdata = newdata, regimen=regimen %>% dplyr::select(-PAST, -FORM), covariates=covsToUse, se.fit=F)
  ipred$TIME <- firstDoseDate + ipred$TIME*3600 # Plotly able to plot POSIXct
  
  # Predict ipred with the new recommendation
  newdata <- getNewdata(start=firstDoseInFutureTime, stop=max(regimen$TIME) + dosingInterval, output=output)
  ipredNew <- predict(fit, newdata=newdata, regimen=nextRegimen, covariates=covsToUse, se.fit=!isMpc, level=0.95) # se.fit disabled if MPC model
  ipredNew$TIME <- firstDoseDate + ipredNew$TIME*3600 # Plotly able to plot POSIXct
  
  # Back compute to POSIXct
  recommendedRegimen <- recommendation$regimen %>% dplyr::mutate(TIME=firstDoseDate + TIME*3600, PAST=regimen$PAST)
  
  return(list(ipred=ipred, ipredNew=ipredNew, recommendedRegimen=recommendedRegimen, tdmoreData=tdmoreData, fit=winningFit))
}