getNewdata <- function(regimen, observed, model) {
  regimen$II <- tdmore::getDosingInterval(regimen$FORM, model=model)
  start <- 0
  stop <- max(c(regimen$TIME + regimen$II, observed$TIME))
    
  times <- seq(start, stop, by=0.5)
  minSamples <- 300
  if (length(times) < minSamples) {
    times <- seq(start, stop, length.out=minSamples)
  }
  important <- c(regimen$TIME, regimen$TIME+regimen$II, observed$TIME)
  times <- sort( unique(c(times, important, important+0.1, important-0.1)) ) #at least include these important timepoints
  times <- times[ times >= 0]
  
  output <- getModelOutput(model)
  observedVariables <- tdmore::getObservedVariables(model)
  
  newdata <- tibble(TIME=times)
  newdata[, output] <- NA
  newdata[, observedVariables] <- NA
  return(newdata)
}

#' This module performs all of the required calculation for the application.
#' 
#' Call it in your application using `calculation(state)`
#' 
#' @section Output:
#' The module sets the following state variables:
#' * `populationPredict` a data-frame with prediction data on the population level
#' * `fit` a tdmorefit object
#' * `individualPredict` a data-frame with prediction data for the individual
#' * `recommendation` a data.frame in the same form as `regimen`, but where the AMT column was adapted
calculation <- function(state) {
  millis <- 500
  model <- debounce(reactive({state$model}), millis=millis)
  regimen <- debounce(reactive({state$regimen}), millis=millis)
  observed <- debounce(reactive({state$observed}), millis=millis)
  covariates <- debounce(reactive({state$covariates}), millis=millis)
  now <- debounce(reactive({state$now}), millis=millis)
  target <- debounce(reactive({state$target}), millis=millis)
  dbstate <- debounce(reactive({
    list(
      model=state$model,
      regimen=state$regimen,
      observed=state$observed,
      covariates=state$covariates,
      now=state$now,
      target=state$target
    )
  }), millis=millis)
  
  tdmoreArgs <- reactive({
    convertDataToTdmore(dbstate())
  })
  # update populationPrediction
  observeEvent(c(regimen, covariates), {
    args <- tdmoreArgs()
    fit <- tdmore::estimate(args$model, regimen=args$regimen, covariates=args$covariates)
    
    newdata <- getNewdata(regimen=args$regimen, observed=args$observed, model=args$model)
    
    data <- predict(fit, newdata=newdata, se.fit=T, level=0.95) # 95% CI by default
    data$TIME <- args$t0 + lubridate::dhours(data$TIME)
    state$populationPredict <- data
  })
  
  # update fit
  observeEvent(tdmoreArgs(), {
    fit <- state$fit
    args <- tdmoreArgs()
    if(needsUpdate(fit, args, onlyEstimate=TRUE) ) {
      fit <- tdmore::estimate(args$model, observed=args$observed, regimen=args$regimen, covariates=args$covariates)
      state$fit <- fit
    } else if (needsUpdate(fit, args, onlyEstimate=FALSE) ){
      #just update the included regimen/observed/covariates
      fit$regimen <- args$regimen
      fit$observed <- args$observed
      fit$covariates <- args$covariates
      state$fit <- fit
    } else {
      #no update needed
    }
  })
  # update individualPredict
  observeEvent(c(regimen, covariates), {
    args <- tdmoreArgs()
    fit <- state$fit
    shiny::req(fit)
    newdata <- getNewdata(regimen=args$regimen, observed=args$observed, model=args$model)
    
    data <- predict(fit, newdata=newdata, se.fit=T, level=0.95) # 95% CI by default
    data$TIME <- args$t0 + lubridate::dhours(data$TIME)
    data$PRED <- state$populationPredict[, getModelOutput(args$model)] #add population prediction line
    state$individualPredict <- data
  })
  
  # update recommendation
  observe({
    shiny::req(state$fit)
    rec <- tdmore::optimize(state$fit, targetMetadata=state$target)
    state$recommendation <- rec$regimen
  })
  # update recommendationPredict
  observeEvent(c(regimen, covariates, state$recommendation), {
    args <- tdmoreArgs()
    
    fit <- state$fit
    newdata <- getNewdata(regimen=args$regimen, observed=args$observed, model=args$model)
    
    data <- predict(fit, regimen=state$recommendation, newdata=newdata, se.fit=T, level=0.95) # 95% CI by default
    data$TIME <- args$t0 + lubridate::dhours(data$TIME)
    state$recommendationPredict <- data
  })
}

needsUpdate <- function(fit, args, onlyEstimate=TRUE) {
  if(is.null(fit)) return(TRUE)
  if(!identical(fit$tdmore, args$model)) return(TRUE)
  if(!isTRUE(all.equal(fit$observed, args$observed))) return(TRUE)
  
  if(onlyEstimate) {
    #only check parts that influence the estimation
    lastObserved <- max( c(0, args$observed$TIME) )
    a <- subset(args$covariates, TIME <= lastObserved)
    b <- subset(fit$covariates, TIME <= lastObserved)
    if(!isTRUE(all.equal(a, b))) return(TRUE)
    
    a <- subset(args$regimen, TIME <= lastObserved)
    b <- subset(fit$regimen, TIME <= lastObserved)
    if(!isTRUE(all.equal(a, b))) return(TRUE)
  } else {
    #check everything
    if(!isTRUE(all.equal(args$covariates, fit$covariates))) return(TRUE)
    if(!isTRUE(all.equal(args$regimen, fit$regimen))) return(TRUE)
  }
  
  return(FALSE)
}