#' Patient constructor.
#'
#' @param firstname patient's firstname
#' @param lastname patient's lastname
#' 
#' @return the patient
#' @export
#' 
createPatient <- function(firstname, lastname) {
  checkFirstname(firstname)
  checkLastname(lastname)
  patient <- list(firstname=firstname, lastname=lastname)
  patient$created_at <- Sys.time()
  patient$modified_at <- Sys.time()
  return(patient)
}

#' Update patient model and covariates
#'
#' @param patient the given patient
#' @param modelName model name
#' 
#' @return the patient
#' @export
#' 
updatePatientModel <- function(patient, modelName) {
  checkModelName(modelName)
  patient$model <- modelName
  patient$modified_at <- Sys.time()
  return(patient)
}

#' Update patient doses.
#'
#' @param patient the given patient
#' @param doses the doses, data frame
#' 
#' @return the patient
#' @export
#' 
updatePatientDoses <- function(patient, doses) {
  patient$doses <- doses
  patient$modified_at <- Sys.time()
  return(patient)
}

#' Update patient measures.
#'
#' @param patient the given patient
#' @param measures the measures, data frame
#' 
#' @return the patient
#' @export
#' 
updatePatientMeasures <- function(patient, measures) {
  patient$measures <- measures
  patient$modified_at <- Sys.time()
  return(patient)
}

#' Update patient covariates.
#'
#' @param patient the given patient
#' @param covariates the covariates, data frame
#' 
#' @return the patient
#' @export
#' 
updatePatientCovariates <- function(patient, covariates) {
  patient$covariates <- covariates
  patient$modified_at <- Sys.time()
  return(patient)
}

#' Update the date of now.
#'
#' @param patient the given patient
#' @param now the date of now, POSIXlt time
#' 
#' @return the patient
#' @export
#' 
updateNowDate <- function(patient, now) {
  patient$now_date <- now
  patient$modified_at <- Sys.time()
  return(patient)
}

#' Convert doses (JSON) to dose model.
#'
#' @param doseJson doses, JSON form
#' 
#' @return tibble with date, time and dose columns
#' 
jsonToDoseModel <- function(doseJson) {
  if (is.null(doseJson) || length(doseJson$date) == 0) {
    return(tibble(date=date(), time=character(), dose=numeric(), formulation=character()))
  }
  datePosix <- as.POSIXct(unlist(doseJson$date))
  date <- POSIXToDate(datePosix)
  time <- POSIXToTime(datePosix)
  return(tibble(date=date, time=time, dose=as.numeric(unlist(doseJson$amount)), formulation=unlist(doseJson$formulation)))
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
  return(data.frame(date=POSIXToString(datePosix), amount=doseModel$dose, formulation=doseModel$formulation))
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

#' Convert covariates (JSON) to covariates model.
#'
#' @param covariateJson covariates, JSON form
#' 
#' @return tibble with date, time, covariate name and value  columns
#' 
jsonToCovariateModel <- function(covariateJson) {
  if (is.null(covariateJson) || length(covariateJson$date) == 0) {
    return(tibble(date=date(), time=character()))
  }
  datePosix <- as.POSIXct(unlist(covariateJson$date))
  date <- POSIXToDate(datePosix)
  time <- POSIXToTime(datePosix)
  tibble <- tibble(date=date, time=time)
  
  covsNames <- names(covariateJson)
  covsNames <- covsNames[covsNames != "date"]
  for (covName in covsNames) {
    tibble <- add_column(tibble, !!covName := as.numeric(unlist(covariateJson[[covName]]))) 
  }
  
  return(tibble)
}

#' Convert covariate model to JSON structure.
#'
#' @param covariateModel the covariate model
#' 
#' @return data frame with date column (full date), covariate name and value
#' 
covariateModelToJson <- function(covariateModel) {
  if (is.null(covariateModel)) {
    return(data.frame(date=character()))
  }
  datePosix <- dateAndTimeToPOSIX(covariateModel$date, covariateModel$time)
  df <- data.frame(date=POSIXToString(datePosix))
  covsNames <- colnames(covariateModel)
  covsNames <- covsNames[!(covsNames %in% c("date", "time"))]
  df <- bind_cols(df, covariateModel %>% select(covsNames))
  return(df)
}

#' Convert patient model to JSON structure.
#'
#' @param patientModel the patient model
#' @return the patient, JSON form
#' @importFrom rjson toJSON
#' 
patientModelToJson <- function(patientModel) {
  patientJson <- patientModel
  patientJson$read_only <- patientModel$read_only
  patientJson$doses <- doseModelToJson(patientModel$doses)
  patientJson$measures <- measureModelToJson(patientModel$measures)
  patientJson$covariates <- covariateModelToJson(patientModel$covariates)
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
  patientModel$covariates <- jsonToCovariateModel(patientJson$covariates)
  patientModel$created_at <- stringToPOSIX(patientJson$created_at)
  patientModel$modified_at <- stringToPOSIX(patientJson$modified_at)
  patientModel$now_date <- if(is.null(patientJson$now_date)){Sys.time()} else {stringToPOSIX(patientJson$now_date)}
  return(patientModel)
}

#' Make a patient read-only.
#'
#' @param patientModel the patient model
#' @return the patient model
#' @export
#' 
toReadOnlyPatient <- function(patientModel) {
  patientModel$read_only <- T
  return(patientModel)
}

#' Say if the given patient is read-only.
#'
#' @param patientModel the patient model
#' @return a logical value
#' @export
#' 
isReadOnlyPatient <- function(patientModel) {
  if (is.null(patientModel$read_only)) {
    return(FALSE)  
  }
  return(patientModel$read_only)
}




