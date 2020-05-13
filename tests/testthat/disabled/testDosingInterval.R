library(tdmore)
library(shinytdmore)
library(testthat)

context("Test the dosing interval is taken into account correctly.
        Last dosing interval should be 12h for Prograft and 24h for Advagraf")

source("testUtils.R")

patient <- findPatient("dosing_interval")

# Prepare recommendation
data <- prepareRecommendationTest(patient)

# Retrieve recommended regimen
recommendedRegimen <- data$recommendedRegimen

# Check recommended doses are fine for Prograft (last dose is Prograft)
expectDoses(recommendedRegimen, c(12, 5))

#########################################

# Change last dose formulation by Advagraf
patient$doses[nrow(patient$doses), "formulation"] <- "Advagraf"

# Prepare recommendation
data <- prepareRecommendationTest(patient)

# Retrieve recommended regimen
recommendedRegimen <- data$recommendedRegimen

# Check recommended doses (last dose is now Advagraf)
expectDoses(recommendedRegimen, c(12, 10.5))
