library(tdmui)
library(testthat)
library(mongolite)
library(assertthat)
library(rjson)
library(tidyverse)

context("Test the persistence layer")

# Remove everything from the database
getDB()$remove("{}")

# Create 4 patients
patient1 <- createPatient("Nicolas", "Luyckx", c(AGE=30, WT=60))
patient2 <- createPatient("Quentin", "Leirens", c(AGE=30, WT=61))
patient3 <- createPatient("Ruben", "Faelens", c(AGE=30, WT=62))
patient4 <- createPatient("Thomas", "Bouillon", c(AGE=30, WT=63))

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
patient <- createPatient("Nicolas", "Luyckx", c(WT=70, HT=1.8, FEMALE=0, CYP3A5=0, PredDose=50, FirstDay=0, HCT=0.45))
updatePatient(1, patient)
patient <- getPatient(1)
expect_equal(patient$covariates, c(WT=70, HT=1.8, FEMALE=0, CYP3A5=0, PredDose=50, FirstDay=0, HCT=0.45))

# Get all patients
patients <- getAllPatients()
