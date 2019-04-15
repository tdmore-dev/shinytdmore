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
  patient <- updatePatientModel(patient, "bergmann2014_base", c(CYP3A5=0))
  
  doseModel <- tibble(
    date=Sys.Date(),
    time=c("08:00"),
    dose=c(8000)
  )
  patient <- updatePatientDoses(patient, doseModel)
  
  measureModel <- tibble(
    date=Sys.Date(),
    time=c("20:00"),
    measure=c(5.0)
  )
  patient <- updatePatientMeasures(patient, measureModel)
  return(patient)
}