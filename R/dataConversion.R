#' Convert shinyTDMore domain to TDMore domain.
#'
#' @param model model
#' @param doses doses
#' @param obs observations
#' @param covs covariates
#' @param now now date, POSIXlt date
#' 
#' @return to be described
#'
convertDataToTdmore <- function(model, doses, obs, covs, now) {
  # Model output
  output <- getModelOutput(model)
  
  # Has model IOV?
  iov <- !is.null(model$iov)
  
  # Important dates
  doseDates <- dateAndTimeToPOSIX(doses$date, doses$time)
  firstDoseDate <- min(doseDates)
  
  # Now but in hours compared to the reference time
  relativeNow <- POSIXToHours(now) - POSIXToHours(firstDoseDate)
  
  # Covariates conversion
  covariates <- covsToTdmore(covs, firstDoseDate, model)
  
  # Make regimen and filtered regimen dataframes
  regimen <- data.frame(
    TIME=as.numeric(difftime(doseDates, firstDoseDate, units="hour")),
    AMT=doses$dose
  )
  if (iov) {
    regimen$OCC <- seq_len(nrow(regimen))
  }
  regimen <- regimen %>% dplyr::mutate(PAST=nearEqual(TIME, relativeNow, mode="lt")) # sign '<' used on purpose
  
  # Make observed and filtered observed dataframes
  if (nrow(obs) > 0) {
    obsDates <- dateAndTimeToPOSIX(obs$date, obs$time)
    observed <- data.frame(TIME=as.numeric(difftime(obsDates, firstDoseDate, units="hour")), USE=obs$use)
    observed[, output] <- obs$measure
    observed <- observed %>% dplyr::mutate(PAST=nearEqual(TIME, relativeNow, mode="ne.lt")) # sign '<=' used on purpose (through concentration can be used for recommendation dose at same time)
    filteredObserved <- observed %>% dplyr::filter(PAST & USE) %>% dplyr::select(-c("PAST", "USE"))
  } else {
    observed <- NULL
    filteredObserved <- NULL
  }
  
  return(list(regimen=regimen, observed=observed, filteredObserved=filteredObserved, firstDoseDate=firstDoseDate, covariates=covariates))
}

#'
#' Covariates conversion (shinyTDMore -> TDMore).
#' 
#' @param covs shinyTDMore covariates
#' @param firstDoseDate first dose date
#' @param model model
#' @return TDMore covariates
#'
covsToTdmore <- function(covs, firstDoseDate, model) {
  covsNames <- colnames(covs)
  covsNames <- covsNames[!(covsNames %in% c("date", "time"))]
  covsDates <- dateAndTimeToPOSIX(covs$date, covs$time)
  if (length(covsNames) > 0) {
    covariates <- bind_cols(data.frame(TIME=as.numeric(difftime(covsDates, firstDoseDate, units="hour"))),
                            covs %>% dplyr::select(covsNames))
  } else {
    covariates <- NULL
  }
  
  return(covariates)
}

#'
#' Get 'newdata' dataframe for tdmore predictions.
#' 
#' @param start start time of 'TIME' column
#' @param stop stop time of 'TIME' column
#' @param output output name (e.g. 'CONC')
#' @param observedVariables additional observed variables
#' @return a dataframe for tdmore
#'
getNewdata <- function(start, stop, output, observedVariables=NULL) {
  times <- seq(start, stop, by=0.5)
  minSamples <- 300
  if (length(times) < minSamples) {
    times <- seq(start, stop, length.out=minSamples)
  }
  newdata <- data.frame(TIME=times)
  newdata[, output] <- NA
  newdata[, observedVariables] <- NA
  return(newdata)
}

#'
#' Near (in)equality function.
#' Different modes:
#' 'ae' : almost equal
#' 'gt' : greater than
#' 'lt' : less than
#' 'ne.gt' : greater than (near equality)
#' 'ne.lt' : less than (near equality)
#' 
#' @param x first vector
#' @param y second vector
#' @param mode comparison mode
#' @param tol tolerance
#' @return logical vector
#'
nearEqual <- function(x, y, mode="ae", tol=1e-8) {
  ae <- mapply(function(x,y) isTRUE(all.equal(x, y, tolerance=tol)), x, y)    
  gt <- x > y
  lt <- x < y
  if (mode == "ae")
    return(ae)
  if (mode == "gt")
    return(gt)
  if (mode == "lt" )
    return(lt)
  if (mode == "ne.gt")
    return(ae | gt)
  if (mode == "ne.lt")
    return(ae | lt)
}