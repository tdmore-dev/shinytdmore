library(shinytdmore)
library(tdmore)
library(testthat)
library(mongolite)
library(assertthat)
library(rjson)
library(tidyverse)

context("Test the mixture model integration in shinytdmore")

#setwd("C:/prj_dev/shinytdmore/tests/testthat")

patient <- findPatient("mixture_test")

# Small issue to solve later on:
# -> 1) use column is added in setPatient method
# -> 2) target is added in setPatient method
measures <- patient$measures
measures$use <- TRUE
patient$measures <- measures
patient$target <- list(min=20, max=30)

# Mixture model prediction
#debugonce(shinytdmore::preparePrediction)
data <- preparePrediction(doses=patient$doses, obs=patient$measures, model=get(patient$model),
                                covs=patient$covs, target=patient$target, population=T, now=patient$now)

# Micture model recommendation
#debugonce(shinytdmore::prepareRecommendation)
data <- prepareRecommendation(doses=patient$doses, obs=patient$measures, model=get(patient$model),
                          covs=patient$covs, target=patient$target, now=patient$now)


# Some code to export patient to json file
# a <-  db$patients[[1]]
# b <- patientModelToJson(a)
# c <- rjson::toJSON(b)