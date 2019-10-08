library(shinytdmore)
library(testthat)
library(mongolite)
library(assertthat)
library(rjson)
library(tidyverse)

context("Test the persistence layer")

# Create 4 patients
patient1 <- createPatient("Nicolas", "Luyckx") %>%
  updatePatientModel("") %>%
  updatePatientCovariates(data.frame(date="11/07/2019", time="12:00", AGE=30, WT=63))
patient2 <- createPatient("Quentin", "Leirens") %>%
  updatePatientModel("") %>%
  updatePatientCovariates(data.frame(date="11/07/2019", time="12:00", AGE=30, WT=63))
patient3 <- createPatient("Ruben", "Faelens") %>%
  updatePatientModel("") %>%
  updatePatientCovariates(data.frame(date="11/07/2019", time="12:00", AGE=30, WT=63))
patient4 <- createPatient("Thomas", "Bouillon") %>%
  updatePatientModel("") %>%
  updatePatientCovariates(data.frame(date="11/07/2019", time="12:00", AGE=30, WT=63))

dbLocation <- tempfile(pattern = "FileDatabase")
dir.create(dbLocation)
db <- FileDatabase$new(folder=dbLocation)

# Remove everything from the database
#db$.__enclos_env__$private$db$drop()

# Add these 4 patients in the database
pt1 <- db$add(patient1)
pt2 <- db$add(patient2)
pt3 <- db$add(patient3)
pt4 <- db$add(patient4)

# Retrieve patient
patient <- db$get(pt1$id)
expect_equal(patient$firstname, "Nicolas")
expect_equal(patient$lastname, "Luyckx")

# Remove a patient
db$remove(pt4$id)
expect_warning(
  expect_error(db$get(pt4$id))
)

# Update a patient & check
patient <- db$add(createPatient("Nicolas", "Luyckx"))
patient <- updatePatientModel(patient, "blabla")

db$update(patient$id, patient)
patient <- db$get(patient$id)
expect_equal(patient$model, "blabla")

# Get all patients
#patients <- db$patients[[4]]

