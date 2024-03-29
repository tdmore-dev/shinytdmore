library(shinytdmore)
library(testthat)
library(mongolite)
library(assertthat)
library(rjson)
library(tidyverse)

context("Test the patient model")

# Create a patient and update the TDM model
patientModel <- createPatient("Nicolas", "Luyckx")
patientModel <- updatePatientModel(patientModel, "")
patientModel$now_date <- Sys.time()

# Add doses
doseModel <- tibble(
  date=as.Date(c("2018/06/25","2018/06/25","2018/06/26","2018/06/26", "2018/06/27")),
  time=c("08:00", "20:00","08:00", "20:00", "08:00"),
  dose=c(6, 6, 7, 7, 7),
  formulation="Tacrolimus",
  fix=c(TRUE, TRUE, FALSE, FALSE, FALSE)
)
patientModel <- updatePatientDoses(patientModel, doseModel)

# Add measures
measureModel <- tibble(
  date=as.Date(c("2018/06/26","2018/06/27")),
  time=c("08:00", "08:00"),
  measure=c(3.1, 5.3)
)
patientModel <- updatePatientMeasures(patientModel, measureModel)

# Add covariates
covariateModel <- tibble(
  date=as.Date(c("2018/06/25", "2018/06/25")),
  time=c("08:00", "09:00"),
  WT=c(60, 61),
  AGE=c(30, 30),
  CYP3A5=1
)
patientModel <- updatePatientCovariates(patientModel, covariateModel)

patientModel$model <- "bergmann2014_base"

#showPatient(patientModel)

db <- InMemoryDatabase$new()
# Add patient to the database
#debugonce(shinytdmore:::jsonToCovariateModel)
idInDB <- db$add(patientModel)$id
expect_equal(idInDB, 1)

# Add patient to DB and find it back from DB
retrievedPatient <- db$get(idInDB)

# Check doses can be retrieved correctly
expect_equal(doseModel, retrievedPatient$doses)

# Check measures can be retrieved correctly
expect_equal(measureModel, retrievedPatient$measures)

# Check covariates can be retrieved correctly
expect_equal(covariateModel, retrievedPatient$covariates)


