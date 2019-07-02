#' Initiate MongoDB with a fictive example
#'
#' @export
#' 
initiateDb <- function() {
  patients <- getAllPatients()
  if(nrow(patients)==0) {
    addPatient(createFakePatient())
  }
}

#' Create a fake patient.
#'
#' @return a fake patient
#' @export
#' 
createFakePatient <- function() {
  patient <- createPatient("Ruben", "Faelens")
  patient <- updatePatientModel(patient, "bergmann2014_base")
  currentDate <- Sys.Date()
  currentTime <- Sys.time()
  
  doseModel <- tibble(
    date=currentDate,
    time=c("08:00"),
    dose=c(8)
  )
  patient <- updatePatientDoses(patient, doseModel)
  
  measureModel <- tibble(
    date=currentDate,
    time=c("20:00"),
    measure=c(5.0)
  )
  patient <- updatePatientMeasures(patient, measureModel)
  
  covariateModel <- tibble(
    date=currentDate,
    time=c("08:00"),
    CYP3A5=c(0)
  )
  patient <- updatePatientCovariates(patient, covariateModel)
  
  patient <- updateNowDate(patient, currentTime)
  
  patient <- toReadOnlyPatient(patient)
  
  return(patient)
}