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
  regimen <- regimen %>% dplyr::mutate(PAST=TIME < relativeNow) # sign '<' used on purpose
  
  # Make observed and filtered observed dataframes
  if (nrow(obs) > 0) {
    obsDates <- dateAndTimeToPOSIX(obs$date, obs$time)
    observed <- data.frame(TIME=as.numeric(difftime(obsDates, firstDoseDate, units="hour")), USE=obs$use)
    observed[, output] <- obs$measure
    observed <- observed %>% dplyr::mutate(PAST=TIME <= relativeNow) # sign '<=' used on purpose (through concentration can be used for recommendation dose at same time)
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
                            covs %>% select(covsNames))
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
#' @return a dataframe for tdmore
#'
getNewdata <- function(start, stop, output) {
  times <- seq(start, stop, by=0.5)
  minSamples <- 300
  if (length(times) < minSamples) {
    times <- seq(start, stop, length.out=minSamples)
  }
  newdata <- data.frame(TIME=times)
  newdata[,output] <- NA
  return(newdata)
}