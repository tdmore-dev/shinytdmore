library(shinytdmore)
library(testthat)
library(mongolite)
library(assertthat)
library(rjson)
library(tidyverse)

context("Test the JSON File database")
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

tmp <- tempfile()
dir.create(tmp)
db <- FileDatabase$new(tmp)

#undebug(db$.__enclos_env__$private$doAdd)
# Add these 4 patients in the database
pt1 <- db$add(patient1)
pt2 <- db$add(patient2)
pt3 <- db$add(patient3)
pt4 <- db$add(patient4)

# Retrieve patient
patient <- db$get(pt1$id)
expect_equal(patient$firstname, "Nicolas")
expect_equal(patient$lastname, "Luyckx")
#expect_equal(patient$covariates, c(AGE=30, WT=60))

# Remove a patient
pt4File <- file.path(tmp, sprintf("%s.json", pt4$id))
expect_true(file.exists(pt4File))
db$remove(pt4$id)
expect_false(file.exists(pt4File))
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
patients <- db$patients[[4]]
