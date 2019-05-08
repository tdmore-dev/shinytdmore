#' Convert doses (JSON) to dose model.
#'
#' @param doseJson doses, JSON form
#' 
#' @return tibble with date, time and dose columns
#' 
jsonToDoseModel <- function(doseJson) {
  if (is.null(doseJson) || length(doseJson$date) == 0) {
    return(tibble(date=date(), time=character(), dose=numeric()))
  }
  datePosix <- as.POSIXct(unlist(doseJson$date))
  date <- POSIXToDate(datePosix)
  time <- POSIXToTime(datePosix)
  return(tibble(date=date, time=time, dose=as.numeric(unlist(doseJson$amount))))
}

#' Convert dose model to JSON structure.
#'
#' @param doseModel the dose model
#' 
#' @return data frame with date column (full date) and amount column
#' 
doseModelToJson <- function(doseModel) {
  if (is.null(doseModel)) {
    return(data.frame(date=character(), amount=numeric()))
  }
  datePosix <- dateAndTimeToPOSIX(doseModel$date, doseModel$time)
  return(data.frame(date=POSIXToString(datePosix), amount=doseModel$dose))
}

#' Convert measures (JSON) to dose model.
#'
#' @param measureJson measures, JSON form
#' 
#' @return tibble with date, time and dose columns
#' 
jsonToMeasureModel <- function(measureJson) {
  if (is.null(measureJson) || length(measureJson$date) == 0) {
    return(tibble(date=date(), time=character(), measure=numeric()))
  }
  datePosix <- as.POSIXct(unlist(measureJson$date))
  date <- POSIXToDate(datePosix)
  time <- POSIXToTime(datePosix)
  return(tibble(date=date, time=time, measure=as.numeric(unlist(measureJson$measure))))
}

#' Convert measure model to JSON structure.
#'
#' @param measureModel the measure model
#' 
#' @return data frame with date column (full date) and measure column
#' 
measureModelToJson <- function(measureModel) {
  if (is.null(measureModel)) {
    return(data.frame(date=character(), measure=numeric()))
  }
  datePosix <- dateAndTimeToPOSIX(measureModel$date, measureModel$time)
  return(data.frame(date=POSIXToString(datePosix), measure=measureModel$measure))
}

#' Convert patient model to JSON structure.
#'
#' @param patientModel the patient model
#' @return the patient, JSON form
#' @importFrom rjson toJSON
#' 
patientModelToJson <- function(patientModel) {
  patientJson <- patientModel
  patientJson$doses <- doseModelToJson(patientModel$doses)
  patientJson$measures <- measureModelToJson(patientModel$measures)
  patientJson$created_at <- POSIXToString(patientModel$created_at)
  patientJson$modified_at <- POSIXToString(patientModel$modified_at)
  patientJson$now_date <- POSIXToString(patientModel$now_date)
  json <- toJSON(patientJson)
  return(json)
}

#' Convert patient (JSON) to patient model
#'
#' @param patientJson the patient, JSON form
#' @return the patient model
#' 
jsonToPatientModel <- function(patientJson) {
  patientModel <- patientJson
  patientModel$doses <- jsonToDoseModel(patientJson$doses)
  patientModel$measures <- jsonToMeasureModel(patientJson$measures)
  patientModel$created_at <- stringToPOSIX(patientJson$created_at)
  patientModel$modified_at <- stringToPOSIX(patientJson$modified_at)
  patientModel$now_date <- if(is.null(patientJson$now_date)){Sys.time()} else {stringToPOSIX(patientJson$now_date)}
  patientModel$covariates <- unlist(patientJson$covariates)
  return(patientModel)
}


