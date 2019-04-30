library(shinytdmore)
library(tdmore)
library(testthat)
library(mongolite)
library(assertthat)
library(rjson)
library(tidyverse)

context("Test the recommendation")

# Very useful to test the recommendation without running the shiny app

# patient <- getPatient(2)
# doses <- patient$doses
# obs <- patient$measures
# model <- bergmann2014_base
# covs <- patient$covariates
# target <- c(10,15)
# now <- patient$now_date
# 
# debugonce(shinytdmore::prepareRecommendation)
# recommendation <- prepareRecommendation(doses, obs, model, covs, target, now)
