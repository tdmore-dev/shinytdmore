library(shinytdmore)
library(tdmore)
library(testthat)
library(mongolite)
library(assertthat)
library(rjson)
library(tidyverse)

context("Test the mixture model integration in shinytdmore")

setwd("C:/prj_dev/shinytdmore/tests/testthat")

db <- FileDatabase$new("patients")
patient <- db$patients[[1]]
debugonce(shinytdmore::preparePrediction)
data <- preparePrediction(doses=patient$doses, obs=patient$measures, model=get(patient$model),
                                covs=patient$covs, target=patient$target, population=T, now=patient$now)
