#' Initiate MongoDB with a fictive example
#'
#' @export
#' 
initiateDb <- function() {
  patients <- getAllPatients()
  
  if(nrow(patients)==0) {
    patient <- createPatient("Ruben", "Faelens", c(AGE=30, WT=60, CYP3A5=0))
    
    doseModel <- tibble(
      date=as.Date(c("2018/06/25","2018/06/25","2018/06/26","2018/06/26", "2018/06/27")),
      time=c("08:00", "20:00","08:00", "20:00", "08:00"),
      dose=c(6, 6, 7, 7, 7)
    )
    patient <- updatePatientDoses(patient, doseModel)
    
    measureModel <- tibble(
      date=as.Date(c("2018/06/26","2018/06/27")),
      time=c("08:00", "08:00"),
      measure=c(3.1, 5.3)
    )
    patient <- updatePatientMeasures(patient, measureModel)
    addPatient(patient)
  }
}