#' Convert doses (JSON) to dose model.
#'
#' @param doseJson doses, JSON form
#' 
#' @return tibble with date, time and dose columns
#' 
jsonToDoseModel <- function(doseJson) {
  if(is.null(doseJson)) {
    return(NULL)
  }
  datePosix <- as.POSIXct(unlist(doseJson$date))
  date <- as.Date(format(datePosix, format="%Y/%m/%d"))
  time <- format(datePosix, format="%H:%M")
  return(tibble(date=date, time=time, dose=as.numeric(unlist(doseJson$amount))))
}

#' Convert dose model to JSON structure.
#'
#' @param doseModel the dose model
#' 
#' @return data frame with date column (full date) and amount column
#' 
doseModelToJson <- function(doseModel) {
  if(is.null(doseModel)) {
    return(NULL)
  }
  date_tmp <- lubridate::ymd(doseModel$date)
  time <- lubridate::hm(doseModel$time)
  date <- date_tmp + time
  return(data.frame(date=posixToString(date), amount=doseModel$dose))
}

#' Convert measures (JSON) to dose model.
#'
#' @param measureJson measures, JSON form
#' 
#' @return tibble with date, time and dose columns
#' 
jsonToMeasureModel <- function(measureJson) {
  if(is.null(measureJson)) {
    return(NULL)
  }
  datePosix <- as.POSIXct(unlist(measureJson$date))
  date <- as.Date(format(datePosix, format="%Y/%m/%d"))
  time <- format(datePosix, format="%H:%M")
  return(tibble(date=date, time=time, measure=as.numeric(unlist(measureJson$measure))))
}

#' Convert measure model to JSON structure.
#'
#' @param measureModel the measure model
#' 
#' @return data frame with date column (full date) and measure column
#' 
measureModelToJson <- function(measureModel) {
  if(is.null(measureModel)) {
    return(NULL)
  }
  date_tmp <- lubridate::ymd(measureModel$date)
  time <- lubridate::hm(measureModel$time)
  date <- date_tmp + time
  return(data.frame(date=posixToString(date), measure=measureModel$measure))
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
  patientJson$created_at <- posixToString(patientModel$created_at)
  patientJson$modified_at <- posixToString(patientModel$modified_at)
  patientJson$now_date <- posixToString(patientModel$now_date)
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
  patientModel$created_at <- as.POSIXlt(patientJson$created_at)
  patientModel$modified_at <- as.POSIXlt(patientJson$modified_at)
  patientModel$now_date <- if(is.null(patientJson$now_date)){Sys.time()} else {as.POSIXlt(patientJson$now_date)}
  patientModel$covariates <- unlist(patientJson$covariates)
  return(patientModel)
}


