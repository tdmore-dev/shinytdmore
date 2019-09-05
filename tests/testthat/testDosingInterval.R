library(tdmore)
library(shinytdmore)
library(testthat)

context("Test the dosing interval is taken into account correctly.
        Last dosing interval should be 12h for Prograft and 24h for Advagraf")

source("testUtils.R")

patient <- findPatient(FileDatabase$new("patients"), "dosing_interval")

# Prepare recommendation
data <- prepareRecommendation(
  doses = patient$doses,
  obs = patient$measures,
  model = get(patient$model),
  covs = patient$covariates,
  target = patient$target,
  now = patient$now
)

# Retrieve recommended regimen
recommendedRegimen <- data$recommendedRegimen

# Check recommended doses are fine for PROGRAFT (last dose is PROGRAFT)
expectDoses(recommendedRegimen, c(12.02, 10.40))


