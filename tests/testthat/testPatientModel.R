library(shinytdmore)
library(testthat)
library(mongolite)
library(assertthat)
library(rjson)
library(tidyverse)

context("Test the patient model")
toConfig(key="db_config", value=testDBConfig()) # Make sure the test DB config is enabled (a test database is used)

# Remove everything from the database
getDB()$remove("{}")

# Doses test
doseModel <- tibble(
  date=as.Date(c("2018/06/25","2018/06/25","2018/06/26","2018/06/26", "2018/06/27")),
  time=c("08:00", "20:00","08:00", "20:00", "08:00"),
  dose=c(6, 6, 7, 7, 7)
)
patientModel <- createPatient("Nicolas", "Luyckx")
patientModel <- updatePatientModel(patientModel, "", c(AGE=30, WT=60))
patientModel <- updatePatientDoses(patientModel, doseModel)
retrievedPatient <- getPatient(addPatient(patientModel))
expect_equal(doseModel, retrievedPatient$doses)

patientModel <- retrievedPatient

# Measures test
measureModel <- tibble(
  date=as.Date(c("2018/06/26","2018/06/27")),
  time=c("08:00", "08:00"),
  measure=c(3.1, 5.3)
)
patientModel <- updatePatientMeasures(patientModel, measureModel)
retrievedPatient <- getPatient(updatePatient(patientModel$id, patientModel))
expect_equal(measureModel, retrievedPatient$measures)
