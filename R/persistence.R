
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
#' @param covariates named numeric with the covariates
#' 
#' @return the patient
#' @export
#' 
updatePatientModel <- function(patient, modelName, covariates=c()) {
  checkModelName(modelName)
  checkCovariates(covariates)
  patient$model <- modelName
  patient$covariates <- as.list(covariates)
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

#' Get the shinytdmore database.
#'
#' @return a mongo database
#' @importFrom mongolite mongo
#' @export
#' 
getDB <- function() {
  dbConfig <- fromConfig(key="shinytdmore_db_config")
  if (is.null(dbConfig)) {
    dbConfig <- defaultDBConfig()
  }
  
  if(!is.null(dbConfig$user)) {
    db <- mongo(collection=dbConfig$collection, db=dbConfig$db,
                url=paste0("mongodb://", dbConfig$user, ":", dbConfig$password ,"@", dbConfig$server, "/"))
  } else {
    db <- mongo(collection=dbConfig$collection, db=dbConfig$db)
  }
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
  retValue <- db$find(fields = '{"id" : true, "firstname" : true, "lastname" : true, "model" : true,"created_at" : true, "modified_at" : true}', sort = '{"id": -1}')
  return(retValue)
}