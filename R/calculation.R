#' Create a newdata data.frame compatible with [tdmore::predict.tdmorefit()]
#' 
#' @return a tibble with at least the regimen times, troughs, and observed times, and intermediate times per 0.5h
#' @inheritParams shinytdmore-data
getNewdata <- function(state, model) {
  args <- convertDataToTdmore(state)
  regimen <- args$regimen
  
  regimen$II <- tdmore::getDosingInterval(regimen$FORM, model=model)
  start <- 0
  stop <- max(0+24, c(regimen$TIME + regimen$II, args$allTimes), na.rm=TRUE)  #at least 1 day
  if(!is.finite(stop)) browser()
  times <- seq(start, stop, by=0.5)
  minSamples <- 300
  if (length(times) < minSamples) {
    times <- seq(start, stop, length.out=minSamples)
  }
  maxSamples <- 1000
  if (length(times) > maxSamples) {
    times <- seq(start, stop, length.out=maxSamples)
  }
  important <- c(regimen$TIME+regimen$II, args$allTimes)
  times <- sort( unique(c(times, important, important+0.1, important-0.1)) ) #at least include these important timepoints
  times <- times[ times >= 0]
  
  output <- getModelOutput(model)
  observedVariables <- tdmore::getObservedVariables(model)
  
  newdata <- tibble(TIME=times)
  newdata[, output] <- NA
  newdata[, observedVariables] <- NA
  return(newdata)
}

#' Reactives that can be used to support tdmore calculations
#' 
#' @name calculation
#' @param millis time for debounce, see [shiny::debounce()]
#' @param fit reactive of type [reactiveFit()] with the current fit
#' @param mc.maxpts number of monte carlo samples to take, see [tdmore::predict.tdmorefit()]
#' @param fitMillis time to wait until calculating new fit
#' @param predictMillis time to wait until calculating a new prediction
#' @param recommendationMillis time to wait until calculating a new recommendation
#' @param recommendation recommendation reactive of type [reactiveRecommendation()]
NULL

# By default, only includes the values needed for convertDataToTdmore
debouncedState <- function(state, names=c("model", "regimen", "observed", "covariates", "now"), millis=2000, label=NULL) {
  notDebounced <- reactive({
    value <- lapply(names, function(x){state[[x]]})
    names(value) <- names
    value
  }, label=label)
  debounced <- debounce(notDebounced, millis=millis)
  debounced
}


#' @rdname calculation
#' @inheritParams shinytdmore-data
#' @param population TRUE to calculate a population fit, FALSE to calculate an individual fit
#' @param label label for all reactives in this system
#' @param estimate estimation function, uses `tdmore::estimate` by default
#' @export
reactiveFit <- function(state, population=FALSE, millis=2000, label=if(population) "PopulationFit" else "IndividualFit", estimate=tdmore::estimate) {
  log <- function(...) {
    cat(label, ":: ", ...)
  }
  dbState <- debouncedState(state, millis=millis, label=paste0(label,"::FitState"))
  cache <- new.env()
  observeEvent(state$model, {
    log("Resetting lastFit because model changed")
    cache$lastFit <<- NULL
  }, label=paste0(label,"::FitResetOnModelChange"), ignoreInit=TRUE)
  updateNeeded <- reactiveVal(value=0, label=paste0(label,"::FitNeedsUpdate"))
  observeEvent(dbState(), {
    args <- convertDataToTdmore(dbState())
    if(population) args$observed <- NULL #remove observed values
    
    if(needsUpdate(cache$lastFit, args, onlyEstimate=FALSE)) {
      log("FIT update needed\n")
      updateNeeded(isolate({updateNeeded()}) + 1) #set value, thereby invalidating the reactive below
    } else {
      log("No FIT update needed\n")
    }
  }, label=paste0(label,"::HandleFitNeedsUpdate"), priority=99) #ensure we never do a loop
  reactiveFit <- reactive({
    log("Calculating new fit\n")
    updateNeeded() #set dependency, and use as *only* dependency!
    args <- isolate({ convertDataToTdmore(state) })
    if(population) args$observed <- NULL #remove observed values
    fit <- cache$lastFit
    if(needsUpdate(fit, args, onlyEstimate=TRUE) ) {
      progress <- Progress$new()
      progress$set(message="Calculating fit...")
      on.exit(progress$close())
      pars <- NULL
      if(!is.null(fit)) pars <- stats::coef(fit)
      fit <- estimate(args$model, 
                                observed=args$observed, 
                                regimen=args$regimen, 
                                covariates=args$covariates,
                                par=pars,
                              .progress=progress)
    } else if (needsUpdate(fit, args, onlyEstimate=FALSE) ){
      #just update the included regimen/observed/covariates
      fit$regimen <- args$regimen
      fit$observed <- args$observed
      fit$covariates <- args$covariates
    } else {
      #no update needed
    }
    cache$lastFit <<- fit
    fit
  }, label=paste0(label,"::Fit"))
  attr(reactiveFit, "cache") <- cache
  reactiveFit
}

#' @rdname calculation
#' @inheritParams shinytdmore-data
#' @export
reactiveRecommendation <- function(state, fit, millis=2000) {
  dbState <- debouncedState(state, c("regimen", "target"), millis=millis)
  recommendation <- reactive({
    progress <- Progress$new()
    progress$set(message="Calculating recommendation...")
    on.exit(progress$close())
    dbState()
    fit() #different fit is different recommendation!
    isolate({
      args <- convertDataToTdmore(state)
      fit <- fit()
      rec <- tdmore::optimize(fit, regimen=args$regimen, targetMetadata=state$target %||% defaultData()$target)
      rec$regimen
    })
  })
  recommendation
}

#' @rdname calculation
#' @inheritParams shinytdmore-data
#' @param estimate estimation function, uses `tdmore::estimate` by default
#' @export
calculationReactives <- function(state, mc.maxpts=100, fitMillis=2000, predictMillis=2000, recommendationMillis=500) {
  cr <- list()
  cr$populationPredict <- reactivePredict(state, mc.maxpts=mc.maxpts)
  cr$fit <- reactiveFit(state, millis=fitMillis)
  cr$populationPredictNoSe <- if(mc.maxpts == 0) cr$populationPredict else reactivePredict(state, mc.maxpts=0, millis=predictMillis)
  cr$individualPredict <- reactivePredict(state, cr$fit, mc.maxpts=mc.maxpts, millis=predictMillis)
  cr$individualPredictNoSe <- if(mc.maxpts == 0) cr$individualPredict else reactivePredict(state, cr$fit, mc.maxpts=0, millis=predictMillis)
  cr$recommendation <- reactiveRecommendation(state, cr$fit, millis=recommendationMillis)
  cr$recommendationPredict <- reactivePredict(state, cr$fit, cr$recommendation, mc.maxpts=mc.maxpts, millis=predictMillis)
  cr$updateFit <- function(fit) {
    env <- attr(cr$fit, 'cache')
    env$lastFit <- fit
  }
  cr
}

#' @rdname calculation
#' @inheritParams shinytdmore-data
#' @export
reactivePredict <- function(state, fit=NULL, recommendation=NULL, mc.maxpts=100, millis=2000) {
  if(is.null(fit)) fit = reactiveFit(state, population=TRUE)
  
  dependency <- debounce(reactive({
    list(
      state$regimen,
      state$model,
      state$covariates
    )
  }, label="ReactivePredict::state"), millis=millis)
  
  r <- reactive({
    dependency() #set up dependency
    progress <- Progress$new()
    progress$set(message="Calculating prediction")
    on.exit(progress$close())
    fit <- fit()
    if(is.null(recommendation)) {
      regimen <- NULL
    } else {
      regimen <- recommendation()
    }
    isolate({
      calculatePredict(state, fit=fit, regimen=regimen, progress, mc.maxpts)
    })
  }, label="ReactivePredict::pred")
  r
}

# An 'all.equal'-like function that has a wider interpretation
# of equal if either a or b is empty (or empty-like)
isEquivalent <- function(a, b) {
  empty <- function(x) is.null(x) || nrow(x) == 0
  (empty(a) && empty(b)) ||
    isTRUE(all.equal(a, b))
}

#' @importFrom dplyr filter select distinct
needsUpdate <- function(fit, args, onlyEstimate=TRUE) {
  tryCatch({
    if(is.null(fit)) return(TRUE)
    if(!identical(fit$tdmore, args$model)) return(TRUE)
    if(!isEquivalent(fit$observed, args$observed)) return(TRUE)
    
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

calculatePredict <- function(state, fit=NULL, regimen=NULL, progress, mc.maxpts) {
  args <- convertDataToTdmore(state)
  newdata <- getNewdata(state, model=args$model)
  if(is.null(fit)) fit <- tdmore::estimate(args$model, regimen=args$regimen, covariates=args$covariates)
  
  p <- tdmore::ShinyToDplyrProgressFacade$new(proxy=progress)
  data <- stats::predict(fit, regimen=regimen, newdata=newdata, se.fit=T, level=0.95, mc.maxpts=mc.maxpts, .progress=p) # 95% CI by default
  data$TIME <- args$t0 + lubridate::dhours(data$TIME)
  data
}