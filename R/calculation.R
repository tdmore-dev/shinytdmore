getNewdata <- function(regimen, observed, model) {
  regimen$II <- tdmore::getDosingInterval(regimen$FORM, model=model)
  start <- 0
  stop <- max(0+24, c(regimen$TIME + regimen$II, observed$TIME))  #at least 1 day
    
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

defaultData <- list(
  regimen=tibble::tibble(time=as.POSIXct(character()), dose=numeric(), formulation=character(), fix=logical()),
  observed=tibble::tibble(time=as.POSIXct(character()), dv=numeric(), use=logical()),
  covariates=tibble::tibble(time=as.POSIXct(character())), #rest of columns depend on model
  now=as.POSIXct("2000-01-01"),
  target=list(min=as.numeric(NA), max=as.numeric(NA))
)

#' This module performs all of the required calculation for the application.
#' 
#' Call it in your application using `calculation(state)`.
#' 
#' Shiny progress calls are used to report which calculation is being done. However, the progress bar does not advance.
#' It is very difficult to create an encompassing progress bar throughout all steps, because some of these steps are shortcutted depending 
#' on the changes done. As an example, changes in future doses will not change the fitted parameters.
#' 
#' @section Output:
#' The module sets the following state variables:
#' * `populationPredict` a data-frame with prediction data on the population level
#' * `fit` a tdmorefit object
#' * `individualPredict` a data-frame with prediction data for the individual
#' * `recommendation` a data.frame in the same form as `regimen`, but where the AMT column was adapted
#' 
#' If any of these values could not be calculated, the outputs are set to the error that occurred during calculation. This may be
#' a simple R error [simpleError()] or a more verbose shiny validation error (e.g. [shiny::validate()] or [shiny::req()])
#' 
#' @param state reactiveValues object with full state
#' @param millis time delay for reacting to changes in the input
#' @param mc.maxpts number of points in monte carlo 
#' 
#' @export
calculation <- function(state, millis=500, mc.maxpts=100) {
  ### TODO: See https://resources.rstudio.com/rstudio-conf-2019/effective-use-of-shiny-modules-in-application-development on time 14:14
  ### Using a reactiveValues() object to pass state around is bad practice
  ### We do it here anyway, because we may change some values
  ### in multiple locations (similar to the AngularJS data binding principle)
  ### See e.g. https://docs.angularjs.org/tutorial/step_06 for an example where
  ### a global state is modified by Input elements.
  
  # setup default values
  isolate({
    missingNames <- setdiff(names(defaultData), names(state))
    for(i in missingNames) state[[i]] <- defaultData[[i]]
  })
  notDebounced <- reactive({
    list(
      model=state$model,
      regimen=state$regimen,
      observed=state$observed,
      covariates=state$covariates,
      now=state$now, #state$now is used by convertDataToTdmore()
      target=state$target #changes the recommendation
    )
  })
  debounced <- debounce(notDebounced, millis=millis)
  
  observeEvent(debounced(), {
    withLogErrors({
      executeCalculation(state, mc.maxpts=mc.maxpts)
    })
  }, 
    label="TDMore calculation",
    priority=-10 #low priority, run at the end
  )
}

executeCalculation <- function(state, mc.maxpts=100) {
  ## TODO: more fine-grained updating: compare original with new value, only store if really new
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  
  progress$set(message = "Calculating prediction", value = 0)
  
  progress$set(detail = "Population prediction")
  # this fires when any input data changes
  args <- convertDataToTdmore(state)
  state$populationPredict <- tryCatch({captureStackTraces(calculatePopulationPredict(state, progress, 1/5, mc.maxpts))}, error=function(e) e)
  progress$inc(1/5, detail = "Fitting")
  fit <- state$fit
  if(needsUpdate(fit, args, onlyEstimate=TRUE) ) {
    state$fit <- tryCatch({captureStackTraces({
      pars <- NULL
      if(!is.null(state$fit)) pars <- stats::coef(state$fit)
      fit <- tdmore::estimate(args$model, 
                              observed=args$observed, 
                              regimen=args$regimen, 
                              covariates=args$covariates,
                              par=pars)
    })}, error=function(e) e)
  } else if (needsUpdate(fit, args, onlyEstimate=FALSE) ){
    state$fit <- tryCatch({captureStackTraces({
      #just update the included regimen/observed/covariates
      fit$regimen <- args$regimen
      fit$observed <- args$observed
      fit$covariates <- args$covariates
    })}, error=function(e) e)
  } else {
    #no update needed
  }
  progress$set(detail = "Individual prediction")
  state$individualPredict <- tryCatch( {captureStackTraces(calculateIndividualPredict(state, progress, 1/5, mc.maxpts))}, error=function(e) e)
  progress$inc(1/5, detail = "Optimizing treatment")
  state$recommendation <- tryCatch({ captureStackTraces(calculateRecommendation(state))}, error=function(e) e)
  progress$set(detail = "Recommendation prediction")
  state$recommendationPredict <- tryCatch({captureStackTraces(calculateRecommendationPredict(state, progress, 1/5, mc.maxpts))}, error=function(e) e)
}



calculateRecommendation <- function(state) {
    shiny::validate(
      shiny::need(state$fit, message="The fit has not been calculated yet...")
    )
    rec <- tdmore::optimize(state$fit, targetMetadata=state$target)
    rec$regimen
}

#' @importFrom dplyr filter select distinct
needsUpdate <- function(fit, args, onlyEstimate=TRUE) {
  tryCatch({
    if(is.null(fit)) return(TRUE)
    if(!identical(fit$tdmore, args$model)) return(TRUE)
    if(!isTRUE(all.equal(fit$observed, args$observed))) return(TRUE)
    
    if(onlyEstimate) {
      #only check parts that influence the estimation
      lastObserved <- max( c(0, args$observed$TIME) )
      a <- filter(args$covariates, .data$TIME <= lastObserved)
      a <- a[, args$model$covariates, drop=FALSE]
      a <- a %>% distinct() %>% as.data.frame
      b <- filter(fit$covariates, .data$TIME <= lastObserved)
      b <- b[, args$model$covariates, drop=FALSE]
      b <- b %>% distinct() %>% as.data.frame
      if(!isTRUE(all.equal(a, b))) return(TRUE)
      a <- args$regimen %>% dplyr::filter(.data$TIME <= lastObserved) %>% dplyr::select(.data$TIME, .data$AMT, .data$FORM)
      b <- fit$regimen %>% dplyr::filter(.data$TIME <= lastObserved) %>% dplyr::select(.data$TIME, .data$AMT, .data$FORM)
      if(!isTRUE(all.equal(a, b))) return(TRUE)
    } else {
      #check everything
      if(!isTRUE(all.equal(args$covariates, fit$covariates))) return(TRUE)
      if(!isTRUE(all.equal(args$regimen, fit$regimen))) return(TRUE)
    }
    
    return(FALSE)
  }, error=function(e) {
    return(TRUE) #definitely recalculate!
  })
}

calculateIndividualPredict <- function(state, progress, amount, mc.maxpts) {
  args <- convertDataToTdmore(state)
  fit <- state$fit
  if(is.null(fit)) stop("Fit not calculated yet...")
  newdata <- getNewdata(regimen=args$regimen, observed=args$observed, model=args$model)
  
  p <- tdmore::ShinyToDplyrProgressFacade$new(proxy=progress, amount=amount)
  data <- stats::predict(fit, newdata=newdata, se.fit=T, level=0.95, mc.maxpts=mc.maxpts, .progress=p) # 95% CI by default
  data$TIME <- args$t0 + lubridate::dhours(data$TIME)
  data
}

calculatePopulationPredict <- function(state, progress, amount, mc.maxpts) {
  args <- convertDataToTdmore(state)
  fit <- tdmore::estimate(args$model, regimen=args$regimen, covariates=args$covariates)
  
  newdata <- getNewdata(regimen=args$regimen, observed=args$observed, model=args$model)
  
  p <- tdmore::ShinyToDplyrProgressFacade$new(proxy=progress, amount=amount)
  data <- stats::predict(fit, newdata=newdata, se.fit=T, level=0.95, mc.maxpts=mc.maxpts, .progress=p) # 95% CI by default
  data$TIME <- args$t0 + lubridate::dhours(data$TIME)
  data
}

calculateRecommendationPredict <- function(state, progress, amount, mc.maxpts) {
  if(inherits(state$recommendation, "error")) stop(state$recommendation)
  args <- convertDataToTdmore(state)
  fit <- state$fit
  newdata <- getNewdata(regimen=args$regimen, observed=args$observed, model=args$model)
  
  p <- tdmore::ShinyToDplyrProgressFacade$new(proxy=progress, amount=amount)
  data <- stats::predict(fit, regimen=state$recommendation, newdata=newdata, se.fit=T, level=0.95, mc.maxpts=mc.maxpts, .progress=p) # 95% CI by default
  data$TIME <- args$t0 + lubridate::dhours(data$TIME)
  data
}