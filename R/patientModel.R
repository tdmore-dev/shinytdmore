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
  patient$read_only <- FALSE
  patient$private <- FALSE
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
    emptyTibble <- tibble(date=as.Date(character()), time=character(), dose=numeric(), formulation=character())
    return(emptyTibble)
  }
  datePosix <- stringToPOSIX(unlist(doseJson$date))
  date <- POSIXToDate(datePosix)
  time <- POSIXToTime(datePosix)
  
  # Backwards compatibility for formulations
  if (is.null(doseJson$formulation)) {
    formulation <- rep("Unknown", length(date)) # By default, form is 'Unknown'
  } else {
    formulation <- unlist(doseJson$formulation)
    formulation <- as.character(formulation)
  }
  
  # Backwards compatibility for fix
  if (is.null(doseJson$fix)) {
    fix <- rep(FALSE, length(date)) # By default, don't fix doses
  } else {
    fix <- unlist(doseJson$fix)
  }
  return(tibble(date=date, time=time, dose=as.numeric(unlist(doseJson$amount)), formulation=formulation, fix=fix))
}

#' Convert dose model to JSON structure.
#'
#' @param doseModel the dose model
#' 
#' @return data frame with date column (full date) and amount column
#' 
doseModelToJson <- function(doseModel) {
  if (is.null(doseModel)) {
    return(tibble(date=character(), amount=numeric(), formulation=character(), fix=logical()))
  }
  datePosix <- dateAndTimeToPOSIX(doseModel$date, doseModel$time)
  return(tibble(date=POSIXToString(datePosix), amount=doseModel$dose, formulation=doseModel$formulation, fix=doseModel$fix))
}

#' Convert measures (JSON) to dose model.
#'
#' @param measureJson measures, JSON form
#' 
#' @return tibble with date, time and dose columns
#' 
jsonToMeasureModel <- function(measureJson) {
  if (is.null(measureJson) || length(measureJson$date) == 0) {
    emptyTibble <- tibble(date=as.Date(character()), time=character(), measure=numeric())
    return(emptyTibble)
  }
  datePosix <- stringToPOSIX(unlist(measureJson$date))
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
    return(tibble(date=character(), measure=numeric()))
  }
  datePosix <- dateAndTimeToPOSIX(measureModel$date, measureModel$time)
  return(tibble(date=POSIXToString(datePosix), measure=measureModel$measure))
}

#' Convert covariates (JSON) to covariates model.
#'
#' @param covariateJson covariates, JSON form
#' 
#' @return tibble with date, time, covariate name and value  columns
#' 
jsonToCovariateModel <- function(covariateJson) {
  if (is.null(covariateJson) || length(covariateJson$date) == 0) {
    emptyTibble <- tibble(date=as.Date(character()), time=character())
    return(emptyTibble)
  }
  datePosix <- stringToPOSIX(unlist(covariateJson$date))
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
    return(tibble(date=character()))
  }
  datePosix <- dateAndTimeToPOSIX(covariateModel$date, covariateModel$time)
  df <- tibble(date=POSIXToString(datePosix))
  covsNames <- colnames(covariateModel)
  covsNames <- covsNames[!(covsNames %in% c("date", "time"))]
  df <- bind_cols(df, covariateModel %>% dplyr::select(covsNames))
  return(df)
}

#' Convert patient model to JSON structure.
#'
#' @param patientModel the patient model
#' @return the patient, JSON form
#' @importFrom rjson toJSON
#' @export
#' 
patientModelToJson <- function(patientModel) {
  patientJson <- patientModel
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
#' @export
#' 
jsonToPatientModel <- function(patientJson) {
  if(is.string(patientJson)) patientJson <- rjson::fromJSON(patientJson)
  patientModel <- patientJson
  patientModel$doses <- jsonToDoseModel(patientJson$doses)
  patientModel$measures <- jsonToMeasureModel(patientJson$measures)
  patientModel$covariates <- jsonToCovariateModel(patientJson$covariates)
  patientModel$created_at <- stringToPOSIX(patientJson$created_at)
  patientModel$modified_at <- stringToPOSIX(patientJson$modified_at)
  patientModel$now_date <- if(is.null(patientJson$now_date)){Sys.time()} else {stringToPOSIX(patientJson$now_date)}
  patientModel$read_only <- if(is.null(patientJson$read_only)){FALSE} else {patientJson$read_only}
  patientModel$private <- if(is.null(patientJson$private)){FALSE} else {patientJson$private}
  return(patientModel)
}

#' Make a patient read-only.
#' A read-only patient cannot be modified in shinytdmore.
#'
#' @param patientModel the patient model
#' @return the patient model
#' @export
#' 
toReadOnlyPatient <- function(patientModel) {
  patientModel$read_only <- T
  return(patientModel)
}

#' Classify a patient as private.
#' These patients will not appear in the patients tab.
#'
#' @param patientModel the patient model
#' @return the patient model
#' @export
#' 
toPrivatePatient <- function(patientModel) {
  patientModel$private <- T
  return(patientModel)
}

#' Say if the given patient is read-only.
#'
#' @param patientModel the patient model
#' @return a logical value
#' @export
#' 
isReadOnlyPatient <- function(patientModel) {
  return(patientModel$read_only)
}

#' Say if the given patient is private.
#'
#' @param patientModel the patient model
#' @return a logical value
#' @export
#' 
isPrivatePatient <- function(patientModel) {
  return(patientModel$private)
}

#' Export patient to JSON file.
#'
#' @param filePath the file path (including file name)
#' @param patientModel the patient model to be exported
#' @export
#' 
exportPatientToJsonFile <- function(filePath, patientModel) {
  jsonPatient <- patientModelToJson(patientModel)
  fileConn <- file(filePath)
  writeLines(jsonPatient, fileConn)
  close(fileConn)
}




