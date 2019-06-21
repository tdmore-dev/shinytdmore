library(shinytdmore)
library(tdmore)
library(testthat)
library(mongolite)
library(assertthat)
library(rjson)
library(tidyverse)

context("Test the recommendation")

#toConfig(key="shinytdmore_db_config", value = createDBConfig(collection="patients", db="tacrolimuskws"))
toConfig(key="shinytdmore_db_config", value = createDBConfig(collection="patients", db="shinytdmore"))

# Very useful to test the recommendation without running the shiny app

# patient <- getPatient(5)
# doses <- patient$doses
# obs <- patient$measures
# obs$use <- T
# model <- get(patient$model)
# covs <- patient$covariates
# now <- patient$now_date
# target <- list(min=2,max=5)
# 
# recommendation <- prepareRecommendation(doses, obs, model, covs, target, now)
