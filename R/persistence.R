
#' Patient constructor.
#'
#' @param firstname patient's firstname
#' @param lastname patient's lastname
#' @param covariates named numeric with the covariates
#' 
#' @return the patient
#' @export
#' 
createPatient <- function(firstname, lastname, covariates=NULL) {
  assert_that(is.character(firstname), msg = "firstname is not character")
  assert_that(is.character(lastname), msg = "lastname is not character")
  
  if (is.null(covariates)) {
    patient <- list(firstname=firstname, lastname=lastname)
  } else {
    covariates <- unlist(covariates)
    assert_that(is.numeric(covariates), msg = "covariates is not numeric")
    assert_that(!is.null(names(covariates)), msg = "covariates are not named")
    patient <- list(firstname=firstname, lastname=lastname, covariates=as.list(covariates))
  }
  
  patient$created_at <- Sys.time()
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

#' Get the TDMore database.
#'
#' @return the TDMore database
#' @importFrom mongolite mongo
#' @export
#' 
getDB <- function() {
  db <- mongo(collection = "virtual_patients", db = "tdmore")
  return(db)
}

#' Add a patient into the database.
#'
#' @param patient patient
#' 
#' @return the patient ID
#' @export
#' 
addPatient <- function(patient) {
  db <- getDB()
  lastIdPatient <- db$find(sort = '{"id": -1}', limit = 1)
  if(nrow(lastIdPatient)==0) {
    patient$id <- 1
  } else {
    patient$id <- as.numeric(lastIdPatient$id) + 1
  }
  db$insert(patientModelToJson(patient))
  return(patient$id)
}

#' Remove a patient from the database.
#'
#' @param id the patient ID
#' @export
#' 
removePatient <- function(id) {
  db <- getDB()
  db$remove(paste0('{"id" : ', id, '}'))
}

#' Update a patient.
#'
#' @param id the ID to be updated
#' @param patient the updated patient
#' 
#' @return the patient ID
#' @export
#' 
updatePatient <- function(id, patient) {
  removePatient(id)
  patient$id <- id
  patient$modified_at <- Sys.time()
  db <- getDB()
  db$insert(patientModelToJson(patient))
  return(patient$id)
}

#' Get a patient from the database. 
#'
#' @param id by ID
#' @param firstname by firstname 
#' @param lastname by lastname
#' @return the requested patient
#' @export
#' 
getPatient <- function(id=NULL, firstname=NULL, lastname=NULL) {
  db <- getDB()
  if(is.null(id)) {
    args <- paste0('{"firstname" : "', firstname, '","lastname" : "', lastname, '"}')
  } else {
    args <- paste0('{"id" : ', id, '}')
  }
  it <- db$iterate(args, limit = 1)
  patientJson <- it$one()
  if(is.null(patientJson)) {
    return(NULL)
  }
  return(jsonToPatientModel(patientJson))
}

#' Get all patients from the database. 
#'
#' @return all the patients in a dataframe
#' @export
#'
getAllPatients <- function() {
  db <- getDB()
  retValue <- db$find(fields = '{"id" : true, "firstname" : true, "lastname" : true, "created_at" : true, "modified_at" : true}', sort = '{"id": -1}')
  return(retValue)
}

#' Convert a posix date to a string. 
#'
#' @param posixDate a posix date
#' @return a well formated string corresponding to the date, time zone included
#' @export
#'
posixToString <- function(posixDate) {
  str <- as.character(posixDate)
  return(str)
}