#' Convert shinyTDMore domain to TDMore domain.
#'
#' @param model model
#' @param doses doses
#' @param obs observations
#' @param covs covariates
#' @param now now date, POSIXlt date
#' 
#' @return to be described
#' @importFrom dplyr filter mutate select
#' @export
#'
convertDataToTdmore <- function(state) {
  model <- state$model
  doses <- state$regimen
  obs <- state$observed
  covs <- state$covs
  now <- state$now
  
  # Model output
  output <- getModelOutput(model)
  
  # Has model IOV?
  iov <- !is.null(model$iov)
  
  # Important dates
  doseDates <- doses$time
  firstDoseDate <- min(doseDates)
  
  # Now but in hours compared to the reference time
  relativeNow <- POSIXToHours(now) - POSIXToHours(firstDoseDate)
  
  # Add formulations as covariates for tdmore
  covsMerge <-  mergeFormAndCov(covs, doses)
  
  # Covariates conversion
  covariates <- covsToTdmore(covsMerge, firstDoseDate)
  
  # Make regimen and filtered regimen dataframes
  regimen <- tibble(
    TIME=as.numeric(difftime(doseDates, firstDoseDate, units="hour")),
    AMT=doses$dose,
    FORM=doses$formulation,
    FIX=doses$fix
  )
  if (iov) {
    regimen$OCC <- seq_len(nrow(regimen))
  }
  regimen <- regimen %>% dplyr::mutate(PAST=nearEqual(TIME, relativeNow, mode="lt")) # sign '<' used on purpose
  
  # Make observed and filtered observed dataframes
  if (nrow(obs) > 0) {
    obsDates <- obs$time
    observed <- tibble(TIME=as.numeric(difftime(obsDates, firstDoseDate, units="hour")), USE=obs$use)
    observed[, output] <- obs$dv
    observed <- observed %>% dplyr::mutate(PAST=nearEqual(TIME, relativeNow, mode="ne.lt")) # sign '<=' used on purpose (through concentration can be used for recommendation dose at same time)
    filteredObserved <- observed %>% dplyr::filter(PAST & USE) %>% dplyr::select(-PAST, -USE)
    if (nrow(filteredObserved %>% dplyr::filter(TIME < 0)) > 0) {
      stop("Some measures occur before the first dose")
    }
  } else {
    observed <- NULL
    filteredObserved <- NULL
  }
  
  return(list(regimen=regimen, observed=observed, filteredObserved=filteredObserved, firstDoseDate=firstDoseDate, covariates=covariates))
}

#'
#' Covariates conversion (shinyTDMore -> TDMore).
#' TODO: discuss this code within the team and test it.
#' 
#' @param covs shinyTDMore covariates
#' @param firstDoseDate first dose date
#' @importFrom dplyr bind_cols bind_rows filter select
#' @return TDMore covariates
#'
covsToTdmore <- function(covs, firstDoseDate) {
  covsNames <- colnames(covs)
  covsNames <- covsNames[!(covsNames %in% "time")]
  covsDates <- covs$time
  if (length(covsNames) > 0) {
    covariates <- dplyr::bind_cols(tibble(TIME=as.numeric(difftime(covsDates, firstDoseDate, units="hour"))),
                            covs %>% dplyr::select(covsNames))
    hasCovariatesAfterT0 <- nrow(covariates %>% filter(TIME >= 0)) > 0
    hasCovariatesAtT0 <- nrow(covariates %>% filter(TIME == 0)) > 0
    
    if (hasCovariatesAfterT0) {
      # In this case, keep only covariates after t0
      covariates <- covariates %>% dplyr::filter(TIME >= 0)
      if (!hasCovariatesAtT0) {
        # Duplicate first row to preserve the original covariates
        covariates <- dplyr::bind_rows(covariates[1,], covariates) 
        covariates[1, "TIME"] <- 0 # Set time to 0
      }
    } else {
      # In this case, keep only last one
      covariates <- covariates[nrow(covariates),]
      covariates[1, "TIME"] <- 0 # Replace original negative time by 0
    }
  } else {
    covariates <- NULL
  }
  
  return(covariates)
}

#'
#' Join covariates and formulations (shinyTDMore -> TDMore).
#' 
#' @param covs shinyTDMore covariates
#' @param doses shinyTDMore doses
#' @importFrom dplyr bind_rows select arrange distinct
#' @importFrom tidyr fill
#' @return TDMore covariates
#'
mergeFormAndCov <- function(covs, doses) {
  joinedCov <- dplyr::bind_rows(covs, doses %>% select(-dose, -fix)) %>%
                  dplyr::arrange( time ) %>%
                  tidyr::fill(-time) %>%
                  tidyr::fill(-time, .direction = 'up') %>%
                  dplyr::distinct()
  return(joinedCov)
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
  newdata <- tibble(TIME=times)
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