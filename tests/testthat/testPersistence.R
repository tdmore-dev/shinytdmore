library(shinytdmore)
library(testthat)
library(mongolite)
library(assertthat)
library(rjson)
library(tidyverse)

context("Test the persistence layer")
toConfig(key="db_config", value=testDBConfig()) # Make sure the test DB config is enabled (a test database is used)

# Remove everything from the database
getDB()$remove("{}")

# Create 4 patients
patient1 <- createPatient("Nicolas", "Luyckx")
patient1 <- updatePatientModel(patient1, "", c(AGE=30, WT=60))

patient2 <- createPatient("Quentin", "Leirens")
patient2 <- updatePatientModel(patient2, "", c(AGE=30, WT=61))

patient3 <- createPatient("Ruben", "Faelens")
patient3 <- updatePatientModel(patient3, "", c(AGE=30, WT=62))

patient4 <- createPatient("Thomas", "Bouillon")
patient4 <- updatePatientModel(patient4, "", c(AGE=30, WT=63))

# Add these 4 patients in the database
addPatient(patient1)
addPatient(patient2)
addPatient(patient3)
addPatient(patient4)

# Retrieve patient
patient <- getPatient(1)
expect_equal(patient$firstname, "Nicolas")
expect_equal(patient$lastname, "Luyckx")
expect_equal(patient$covariates, c(AGE=30, WT=60))

# Remove a patient
removePatient(4)
expect_true(is.null(getPatient(4)))

# Update a patient & check
patient <- createPatient("Nicolas", "Luyckx")
patient <- updatePatientModel(patient, "", c(WT=70, HT=1.8, FEMALE=0, CYP3A5=0, PredDose=50, FirstDay=0, HCT=0.45))

updatePatient(1, patient)
patient <- getPatient(1)
expect_equal(patient$covariates, c(WT=70, HT=1.8, FEMALE=0, CYP3A5=0, PredDose=50, FirstDay=0, HCT=0.45))

# Get all patients
patients <- getAllPatients()
